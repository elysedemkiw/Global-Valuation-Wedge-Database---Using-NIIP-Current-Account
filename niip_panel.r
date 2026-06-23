
library(readxl)
 
SRC <- "/Users/elyse/Downloads/NIIP/EWN-dataset-year-end-2024_4.9.26 (1).xlsx"
OUT <- "/Users/elyse/Downloads/NIIP/niip_valuation_decomposition.csv"
PANEL_OUT  <- sub("\\.csv$", "_panel.csv", OUT)   # country-year long table, lands next to OUT
KAOPEN_URL <- "https://web.pdx.edu/~ito/kaopen_2023.xls"   # Chinn-Ito index, 1970-2023
 
# read the dataset and keep only the columns we need
df <- as.data.frame(read_excel(SRC, sheet = "Dataset"))
names(df)[names(df) == "Net IIP excl gold"]        <- "NIIP"  # observed historical NIIP excluding gold
names(df)[names(df) == "Current account balance"]  <- "CA"
names(df)[names(df) == "GDP (US$)"]                 <- "GDP"
df <- df[, c("Country", "IFS_Code", "Year", "NIIP", "CA", "GDP")]
df$Year <- as.integer(df$Year)
 
 
summarize_gaps <- function(df) {
  # this looks at the raw data before any fixing and counts the gap types
  # three problems can break a clean cumulation
  # 1 no NIIP at all so there is no level to anchor on, these are offshore centers
  # 2 leading CA gap, CA coverage starts after the NIIP window starts
  # 3 trailing CA gap, CA coverage ends before the NIIP window ends
  no_niip <- 0; lead <- 0; trail <- 0; both <- 0
  for (sub in split(df, df$Country)) {
    sub <- sub[order(sub$Year), ]
    nm <- !is.na(sub$NIIP)
    if (!any(nm)) { no_niip <- no_niip + 1; next }
    n0 <- min(sub$Year[nm]); n1 <- max(sub$Year[nm])
    ca_yrs_in <- sub$Year[sub$Year >= n0 & sub$Year <= n1 & !is.na(sub$CA)]
    has_any   <- length(ca_yrs_in) > 0
    has_lead  <- has_any && min(ca_yrs_in) > n0
    has_trail <- has_any && max(ca_yrs_in) < n1
    if (has_lead)  lead  <- lead + 1
    if (has_trail) trail <- trail + 1
    if (has_lead && has_trail) both <- both + 1
  }
  cat("gap summary before fixing\n")
  cat("total entities", length(unique(df$Country)), "\n")
  cat("no NIIP at all, dropped", no_niip, "\n")
  cat("leading CA gap, CA starts late", lead, "\n")
  cat("trailing CA gap, CA ends early", trail, "\n")
  cat("both leading and trailing", both, "\n\n")
}
 
 
pick_window <- function(niip_yrs, ca_yrs) {
  # this is the fix for the gaps
  # the present year T is the latest year that has NIIP and a CA chain reaching it
  # walking T back past a trailing CA gap fixes problem 3
  # the base year t0 is the earliest NIIP year that keeps CA contiguous to T
  # moving t0 forward to where CA begins fixes problem 2
  # if there is no NIIP at all the loop returns nothing which drops problem 1
  for (Tend in sort(intersect(niip_yrs, ca_yrs), decreasing = TRUE)) {
    block_start <- Tend
    while ((block_start - 1) %in% ca_yrs) block_start <- block_start - 1
    candidates <- niip_yrs[niip_yrs >= block_start - 1 & niip_yrs <= Tend - 1]
    if (length(candidates) > 0) return(c(t0 = min(candidates), T = Tend))
  }
  c(t0 = NA_integer_, T = NA_integer_)
}
 
 
decompose <- function(sub) {
  # build the endpoint numbers for one country
  sub <- sub[order(sub$Year), ]
  niip_yrs <- sub$Year[!is.na(sub$NIIP)]
  ca_yrs   <- sub$Year[!is.na(sub$CA)]
  if (length(niip_yrs) == 0) return(NULL)
  w <- pick_window(niip_yrs, ca_yrs)
  t0 <- w[["t0"]]; Tend <- w[["T"]]
  if (is.na(t0)) return(NULL)
  at <- function(col, yr) sub[[col]][match(yr, sub$Year)]
  niip_base   <- at("NIIP", t0)
  niip_actual <- at("NIIP", Tend)
  cum_ca   <- sum(sub$CA[sub$Year >= t0 + 1 & sub$Year <= Tend], na.rm = TRUE)  # cumulative CA from t0+1 to T
  niip_hyp <- niip_base + cum_ca          # flow only counterfactual NIIP
  wedge    <- niip_actual - niip_hyp      # cumulative valuation effect
  gdp_T    <- at("GDP", Tend)
  list(
    IFS_Code          = as.integer(na.omit(sub$IFS_Code)[1]),
    base_year         = t0,
    present_year      = Tend,
    n_years           = Tend - t0,
    niip_base         = niip_base,
    cumulative_CA     = cum_ca,
    niip_hypothetical = niip_hyp,
    niip_actual       = niip_actual,
    valuation_wedge   = wedge,
    GDP_present       = gdp_T,
    wedge_pct_GDP     = if (!is.na(gdp_T) && gdp_T != 0) 100 * wedge / gdp_T else NA_real_
  )
}
 
 
interp_interior <- function(v) {
  # linear interpolation of internal NAs only, leaving leading/trailing NAs alone
  idx <- which(!is.na(v))
  if (length(idx) < 2) return(v)
  rng <- idx[1]:idx[length(idx)]
  v[rng] <- approx(idx, v[idx], xout = rng)$y
  v
}
 
 
country_paths <- function(sub) {
  # build the full year by year actual and hypothetical NIIP series
  # the hypothetical starts at the base NIIP and adds each years CA on top
  sub <- sub[order(sub$Year), ]
  niip_yrs <- sub$Year[!is.na(sub$NIIP)]
  ca_yrs   <- sub$Year[!is.na(sub$CA)]
  if (length(niip_yrs) == 0) return(NULL)
  w <- pick_window(niip_yrs, ca_yrs)
  t0 <- w[["t0"]]; Tend <- w[["T"]]
  if (is.na(t0)) return(NULL)
  years <- t0:Tend
  at <- function(col, yr) sub[[col]][match(yr, sub$Year)]
  actual <- at("NIIP", years)
  ca <- at("CA", years); ca[is.na(ca)] <- 0
  hyp <- numeric(length(years))
  hyp[1] <- at("NIIP", t0)
  for (i in seq_along(years)[-1]) hyp[i] <- hyp[i - 1] + ca[i]  # carry the position forward on CA only
  actual <- interp_interior(actual)  # interpolate small internal holes so the shading has no breaks
  list(paths = data.frame(Year = years, actual = actual, hypothetical = hyp),
       t0 = t0, T = Tend)
}
 
 
# load the Chinn-Ito index once so both the endpoint table and the panel can use it
# cn is the IMF three digit code and lines up with IFS_Code
# kaopen is the raw index, ka_open is the same thing normalized to 0 to 1
# the index ends in 2023 so for a later year we carry the latest reading forward
kaopen_file <- file.path(dirname(OUT), "kaopen_2023.xls")
if (!file.exists(kaopen_file)) download.file(KAOPEN_URL, kaopen_file, mode = "wb")
ci <- as.data.frame(read_excel(kaopen_file))
ci <- ci[!is.na(ci$kaopen), c("cn", "year", "kaopen", "ka_open")]
 
get_kaopen <- function(code, yr) {
  sub <- ci[ci$cn == code & ci$year <= yr, ]
  if (nrow(sub) == 0) return(c(kaopen = NA, kaopen_norm = NA, kaopen_year = NA))
  sub <- sub[which.max(sub$year), ]   # exact year if present, else latest reading before it
  c(kaopen = sub$kaopen, kaopen_norm = sub$ka_open, kaopen_year = sub$year)
}
 
 
build_panel <- function(sub) {
  # one row per country-year over the same window the decomposition uses
  # valuation_wedge is the cumulative valuation effect since the base year (0 at base)
  # valuation_annual is the single-year revaluation, i.e. the change in the wedge
  res <- country_paths(sub)
  if (is.null(res)) return(NULL)
  paths <- res$paths
  yrs   <- paths$Year
  s  <- sub[order(sub$Year), ]
  at <- function(col, yr) s[[col]][match(yr, s$Year)]
  ca    <- at("CA", yrs)
  gdp   <- at("GDP", yrs)
  wedge <- paths$actual - paths$hypothetical
  code  <- as.integer(na.omit(sub$IFS_Code)[1])
  kk    <- t(sapply(yrs, function(y) get_kaopen(code, y)))   # openness at each actual year
  data.frame(
    Country           = sub$Country[1],
    IFS_Code          = code,
    Year              = yrs,
    CA                = ca,
    GDP               = gdp,
    niip_actual       = paths$actual,
    niip_hypothetical = paths$hypothetical,
    valuation_wedge   = wedge,
    valuation_annual  = c(NA, diff(wedge)),
    wedge_pct_GDP     = ifelse(!is.na(gdp) & gdp != 0, 100 * wedge / gdp, NA),
    kaopen            = kk[, "kaopen"],
    kaopen_norm       = kk[, "kaopen_norm"],
    stringsAsFactors  = FALSE
  )
}
 
 
# run the gap summary first
summarize_gaps(df)
 
# endpoint table, one row per country
records <- list()
for (sub in split(df, df$Country)) {
  res <- decompose(sub)
  if (!is.null(res)) {
    res$Country <- sub$Country[1]
    records[[length(records) + 1]] <- as.data.frame(res, stringsAsFactors = FALSE)
  }
}
out <- do.call(rbind, records)
out <- out[order(out$Country), ]
rownames(out) <- NULL
 
money <- c("niip_base", "cumulative_CA", "niip_hypothetical",
           "niip_actual", "valuation_wedge", "GDP_present")
out[money] <- round(out[money], 1)
out$wedge_pct_GDP <- round(out$wedge_pct_GDP, 2)
 
kk <- t(mapply(get_kaopen, out$IFS_Code, out$present_year))
out$kaopen      <- round(as.numeric(kk[, "kaopen"]), 4)
out$kaopen_norm <- round(as.numeric(kk[, "kaopen_norm"]), 4)
out$kaopen_year <- as.integer(kk[, "kaopen_year"])
out <- out[, c(
  "Country", "IFS_Code", "base_year", "present_year", "n_years",
  "niip_base", "cumulative_CA", "niip_hypothetical",
  "niip_actual", "valuation_wedge", "wedge_pct_GDP", "GDP_present",
  "kaopen", "kaopen_norm", "kaopen_year"
)]
dir.create(dirname(OUT), recursive = TRUE, showWarnings = FALSE)
write.csv(out, OUT, row.names = FALSE)
cat("countries decomposed", nrow(out),
    "| kaopen matched", sum(!is.na(out$kaopen)), "\n")
cat("endpoint csv written to", OUT, "\n")
 
# country-year panel, one row per country per year
panel <- do.call(rbind, lapply(split(df, df$Country), build_panel))
panel <- panel[order(panel$Country, panel$Year), ]
rownames(panel) <- NULL
pmoney <- c("CA", "GDP", "niip_actual", "niip_hypothetical",
            "valuation_wedge", "valuation_annual")
panel[pmoney] <- round(panel[pmoney], 1)
panel$wedge_pct_GDP <- round(panel$wedge_pct_GDP, 2)
panel$kaopen        <- round(panel$kaopen, 4)
panel$kaopen_norm   <- round(panel$kaopen_norm, 4)
write.csv(panel, PANEL_OUT, row.names = FALSE)
cat("panel rows", nrow(panel), "across", length(unique(panel$Country)), "countries\n")
cat("panel csv written to", PANEL_OUT, "\n")
 
 
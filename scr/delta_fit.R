delta_fit <- function(fit_less, fit_more, label_less, label_more) {
  fm_less1 <- lavaan::fitMeasures(fit_less, c("cfi", "mfi", "rmsea", "srmr", "df"))
  fm_less2 <- semTools::moreFitIndices(fit_less, "gammaHat")
  
  fm_more1 <- lavaan::fitMeasures(fit_more, c("cfi", "mfi", "rmsea", "srmr", "df"))
  fm_more2 <- semTools::moreFitIndices(fit_more, "gammaHat")
  
  data.frame(
    Comparison   = paste(label_less, "→", label_more),
    `ΔCFI`       = round(fm_more1["cfi"] - fm_less1["cfi"], 3),
    `ΔRMSEA`     = round(fm_more1["rmsea"] - fm_less1["rmsea"], 3),
    `ΔSRMR`       = round(fm_more1["srmr"] - fm_less1["srmr"], 3),
    `∆Gamma hat` = round(fm_more2["gammaHat"] - fm_less2["gammaHat"], 3),
    `ΔMcDonald's NCI` = round(fm_more1["mfi"] - fm_less1["mfi"], 3),
    check.names  = FALSE,
    row.names    = NULL)
}
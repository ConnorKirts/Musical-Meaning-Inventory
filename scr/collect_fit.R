collect_fit <- function(fit_obj, label, n_items = NULL, include_ic = TRUE) {
  fm <- lavaan::fitMeasures(fit_obj, c("chisq","df","pvalue","cfi","tli",
                                       "rmsea","rmsea.ci.lower","rmsea.ci.upper",
                                       "srmr","aic","bic"))
  out <- data.frame(
    Model   = label,
    `χ²`    = round(fm["chisq"], 2),
    df      = fm["df"],
    p       = round(fm["pvalue"], 3),
    CFI     = round(fm["cfi"], 3),
    TLI     = round(fm["tli"], 3),
    RMSEA   = round(fm["rmsea"], 3),
    `90% CI`= paste0("[", round(fm["rmsea.ci.lower"],3),
                     ", ", round(fm["rmsea.ci.upper"],3), "]"),
    SRMR    = round(fm["srmr"], 3),
    check.names = FALSE, 
    row.names = NULL)
  if (!is.null(n_items)) out <- cbind(Items = n_items, out)
  if (include_ic) {
    out$AIC <- round(fm["aic"], 1)
    out$BIC <- round(fm["bic"], 1)
  }
  out
}
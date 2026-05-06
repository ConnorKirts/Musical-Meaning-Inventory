inv_fit_row <- function(fit_obj, label) {
  fm <- lavaan::fitMeasures(fit_obj,
                            c("chisq", "df", "pvalue", "cfi", "tli", "rmsea",
                              "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
  data.frame(
    Model       = label,
    `χ²`        = round(fm["chisq"], 2),
    df          = fm["df"],
    p           = round(fm["pvalue"], 3),
    CFI         = round(fm["cfi"], 3),
    TLI         = round(fm["tli"], 3),
    RMSEA       = round(fm["rmsea"], 3),
    `90% CI`    = paste0("[", round(fm["rmsea.ci.lower"], 3),
                      ", ", round(fm["rmsea.ci.upper"], 3), "]"),
    SRMR        = round(fm["srmr"],  3),
    check.names = FALSE, 
    row.names   = NULL)
}
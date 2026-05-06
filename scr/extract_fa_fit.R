extract_fa_fit <- function(fa_obj, label = "") {
  
  chi_sq   <- fa_obj$STATISTIC
  df       <- fa_obj$dof
  p_val    <- fa_obj$PVAL
  rmsr     <- fa_obj$rms        
  rmsea    <- fa_obj$RMSEA[1]   
  tli      <- fa_obj$TLI
  cfi      <- fa_obj$CFI
  aic      <- chi_sq - (2 * df)
  bic      <- chi_sq - (log(fa_obj$n.obs) * df)
  
  data.frame(
    Solution     = label,
    `χ²`         = round(chi_sq, 3),
    df           = df,
    p            = round(p_val, 4),
    RMSR         = round(rmsr, 3),
    RMSEA        = round(rmsea, 3),
    `90% CI`     = paste0("[", round(fa_obj$RMSEA[2], 3), ", ",
                          round(fa_obj$RMSEA[3], 3), "]"),
    TLI          = round(tli, 3),
    CFI          = round(cfi, 3),
    AIC          = round(aic, 3),
    BIC          = round(bic, 3),
    row.names = NULL,
    check.names = FALSE
  )
}
make_loading_table <- function(fit_obj, cfa_data, caption_text) {
  params   <- parameterEstimates(fit_obj, standardized = TRUE)
  loadings <- params[params$op == "=~", ]
  vars     <- unique(loadings$rhs)
  
  lt <- data.frame(
    Factor = loadings$lhs,
    Item = loadings$rhs,
    Standardized = round(loadings$std.all, 3))
  
  desc <- data.frame(
    Variable = vars,
    Mean = round(sapply(cfa_data[, vars], mean, na.rm = TRUE), 2),
    SD = round(sapply(cfa_data[, vars], sd,   na.rm = TRUE), 2))
  
  out <- merge(lt, desc, by.x = "Item", by.y = "Variable", all.x = TRUE)
  out$Factor <- factor(out$Factor,
                       levels = c("Structure","Memory","Identity","Object","Culture"))
  out <- out[order(out$Factor), ]
  rownames(out) <- NULL
  knitr::kable(out, digits = 2, caption = caption_text)
}
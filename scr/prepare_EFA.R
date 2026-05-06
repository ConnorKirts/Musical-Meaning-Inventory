prepare_EFA <- function(d = NULL, vignette_filter = "ALL", instruments = c('ST','SE','SO')){
  if(is.null(d)){
    stop("Data is NULL. Please provide a data frame.")
  }

# Specify vignette subset --------------------------------------------------
  if(vignette_filter == "ALL"){
    tmp2 <- d
  } else {
    tmp2 <- dplyr::filter(d, VigNro %in% vignette_filter)
  }

# Pivot to wide format -----------------------------------------------------
  tmp_w <- pivot_wider(tmp2, names_from = item, values_from = value,
                       id_expand = FALSE, id_cols = c('ID','VigNro'))
  tmp_w <- data.frame(tmp_w)
  rownames(tmp_w) <- 1:nrow(tmp_w)
  rownames(tmp_w) <- tmp_w$ID
  tmp_w <- select(tmp_w, -ID)

# Subset specified instruments ------------------------------------------
  inst_pattern <- paste0('^(', paste(instruments, collapse = '|'), ')')
  tmp_w <- select(tmp_w, matches(inst_pattern))

  return(tmp_w)

}

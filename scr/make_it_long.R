make_it_long <- function(d = NULL, verbose = TRUE){
  if(is.null(d)){
    stop("Data is NULL. Please provide a data frame.")
  }

  if (any(grepl("^BASIC\\d+$", names(d)))){
    d <- d %>%
      rename(BASIC1_1 = BASIC1,
             BASIC2_2 = BASIC2,
             BASIC3_3 = BASIC3
      )
  }
  
  item_ID <- str_detect(names(d), "_") # find the item names for all instruments
  df <- pivot_longer(d, cols = names(d)[item_ID], names_to = "item", values_to = "value")
  df <- df %>% mutate(across(value, as.numeric))
  
  
# Need to specify the Vignette and VigNro for the within condition
  within_item <- str_detect(df$item, "^(ST|SE|SO|MEEM|HAAS|GEMS|BASIC|MecScale)[1-3]_")
  
  if(any(within_item)){
    df <- df %>%
      mutate(VigNro = if_else(within_item, as.numeric(str_match(item, "^(?:ST|SE|SO|MEEM|HAAS|BASIC|GEMS|MecScale)([1-3])_")[, 2]), VigNro), Vignette = case_when(within_item & VigNro == 1 ~ "ST", within_item & VigNro == 2 ~ "SE", within_item & VigNro == 3 ~ "SO", TRUE ~ Vignette))
  }
  
  if(verbose) {
    print(paste("Number of rows in the long data:", nrow(df)))
    print(paste("Number of unique participants in the long data:", length(unique(df$ID))))
    print(paste("Number of unique items in the long data:", length(unique(df$item))))
  }

  return(df)

}
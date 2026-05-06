descriptives <- function(d, label) {
  N <- length(unique(d$ID))
  
  age     <- d$Age[!duplicated(d$ID)]
  age     <- age[!is.na(age)]
  Mean    <- round(mean(age), 1)
  Median  <- round(median(age), 1)
  SD      <- round(sd(age), 2)
  Min     <- min(age)
  Max     <- max(age)
  
  gender_counts <- table(d$Gender[!duplicated(d$ID)])
  Gender <- paste(
    names(gender_counts),
    gender_counts,
    sep = ": ",
    collapse = "; "
  )
  
  OMSI_counts <- table(d$OMSI[!duplicated(d$ID)])
  OMSI <- paste(
    names(OMSI_counts),
    OMSI_counts,
    sep = ": ",
    collapse = "; "
  )
  
  data.frame(
    Dataset      = label,
    N            = N,
    `Mean Age`   = Mean,
    `Median Age` = Median,
    `SD Age`     = SD,
    `Age Range`  = paste0("[", Min, ", ", Max, "]"),
    Gender       = Gender,
    Musicianship = OMSI,
    row.names    = NULL,
    check.names  = FALSE
  )
}

# Calculating percentages for OMSI, not really necessary
# percent <- function(d){
# count <- table(d$OMSI[!duplicated(d$ID)])
# prop <- prop.table(count)
# per <- round(prop * 100)
# print(per)
# }
# 
# percent(df1b)
# percent(df2b)
# percent(df3b)
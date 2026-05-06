read_qualtrics <- function(filename=NULL, controlQ=NULL, 
                           condition = c("between", "within", "CFA")) {
  
  print(filename); print(controlQ)

  tmp <- read.csv(filename, header = TRUE, na.strings = "")
  names(tmp) <- gsub("\\.", "", names(tmp)) # remove full stops from the column names
  tmp <- tmp[3:nrow(tmp), ]     # delete two first rows (metadata)

  tmp <- dplyr::filter(tmp, Consent == 2, Age > 18) # keep participants who consent

  # Making sure values are numeric --------------------
  tmp <- tmp %>% mutate(across(starts_with("Prog"), as.numeric))
  tmp <- tmp %>% mutate(across(starts_with("Dur"), as.numeric))
  tmp <- tmp %>% mutate(across(starts_with("Age"), ~ as.numeric(as.character(.))))

  print(paste("Participants after consent/age filter:", nrow(tmp)))

  # Participant check ---------------------------------
  tmp2 <- dplyr::filter(tmp,
                 Progress == 100,
                 DistributionChannel != "preview",
                 Age != 99)
  
  print(paste("Participants after complete/preview/age-99 filter:", nrow(tmp2)))

  # Eliminate unnecessary columns --------------------
  drop_cols <- c('StartDate', 'EndDate', 'Status', 'Progress', 'Durationinseconds', 
                 'Finished', 'RecordedDate', 'DistributionChannel', 'UserLanguage', 
                 'ResponseId','Consent')
  d <- dplyr::select(tmp2, -any_of(drop_cols))

  # Renaming values ----------------------------------
  d$Gender <- case_when(d$Gender == 1 ~ "Male", 
                        d$Gender == 2 ~ "Female", 
                        d$Gender == 3 ~ "Non-binary", 
                        d$Gender == 4 ~ "Other", 
                        d$Gender == 5 ~ "Prefer not to say")
  
  d$OMSI <- case_when(d$OMSI == 1 ~ "Nonmusician", 
                      d$OMSI == 2 ~ "Music-loving Nonmusician", 
                      d$OMSI == 3 ~ "Amateur musician", 
                      d$OMSI == 4 ~ "Serious amateur musician", 
                      d$OMSI == 5 ~ "Semiprofessional musician", 
                      d$OMSI == 6 ~ "Professional musician")
  
  # ── Rename MM columns — branch by condition ──────────────────────────────────
  if (condition == "between") {
    
    MM_cols <- which(str_detect(names(d), "MM_"))
    if (length(MM_cols) > 0) {
      old <- c("MM_1","MM_2","MM_3","MM_4","MM_5","MM_6","MM_7","MM_8","MM_9",
               "MM_10","MM_11","MM_12","MM_13","MM_14","MM_15","MM_16","MM_17",
               "MM_18","MM_19","MM_20","MM_21","MM_22","MM_23","MM_24")
      new <- c("ST_1","ST_2","ST_3","ST_4","ST_5","ST_6","ST_7","SE_1","SE_2",
               "SE_3","SE_4","SE_5","SE_6","SE_7","SE_8","SE_9","SE_10","SO_1",
               "SO_2","SO_3","SO_4","SO_5","SO_6","SO_7")
      d <- d %>% rename(!!!setNames(old, new))
    }
    
  } else if (condition == "within") {
    
    MMall_cols <- which(str_detect(names(d), "MM[1,2,3]_"))
    if (length(MMall_cols) > 0) {
      old <- c("MM1_1","MM1_2","MM1_3","MM1_4","MM1_5","MM1_6","MM1_7","MM1_8",
               "MM1_9","MM1_10","MM1_11","MM1_12","MM1_13","MM1_14","MM1_15",
               "MM1_16","MM1_17","MM1_18","MM1_19","MM1_20","MM1_21","MM1_22",
               "MM1_23","MM1_24","MM1_26","MM1_27","MM1_28","MM1_29","MM1_30",
               "MM1_31","MM1_32","MM1_33","MM1_34",
               "MM2_1","MM2_2","MM2_3","MM2_4","MM2_5","MM2_6","MM2_7","MM2_8",
               "MM2_9","MM2_10","MM2_11","MM2_12","MM2_13","MM2_14","MM2_15",
               "MM2_16","MM2_17","MM2_18","MM2_19","MM2_20","MM2_21","MM2_22",
               "MM2_23","MM2_24","MM2_26","MM2_27","MM2_28","MM2_29","MM2_30",
               "MM2_31","MM2_32","MM2_33","MM2_34",
               "MM3_1","MM3_2","MM3_3","MM3_4","MM3_5","MM3_6","MM3_7","MM3_8",
               "MM3_9","MM3_10","MM3_11","MM3_12","MM3_13","MM3_14","MM3_15",
               "MM3_16","MM3_17","MM3_18","MM3_19","MM3_20","MM3_21","MM3_22",
               "MM3_23","MM3_24","MM3_26","MM3_27","MM3_28","MM3_29","MM3_30",
               "MM3_31","MM3_32","MM3_33","MM3_34")
      new <- c("ST1_1","ST1_2","ST1_3","ST1_4","ST1_5","ST1_6","ST1_7","SE1_1",
               "SE1_2","SE1_3","SE1_4","SE1_5","SE1_6","SE1_7","SE1_8","SE1_9",
               "SE1_10","SO1_1","SO1_2","SO1_3","SO1_4","SO1_5","SO1_6","SO1_7",
               "ST1_8","SE1_11","SE1_12","SE1_13","SO1_8","SO1_9","SO1_10",
               "SO1_11","SO1_12",
               "ST2_1","ST2_2","ST2_3","ST2_4","ST2_5","ST2_6","ST2_7","SE2_1",
               "SE2_2","SE2_3","SE2_4","SE2_5","SE2_6","SE2_7","SE2_8","SE2_9",
               "SE2_10","SO2_1","SO2_2","SO2_3","SO2_4","SO2_5","SO2_6","SO2_7",
               "ST2_8","SE2_11","SE2_12","SE2_13","SO2_8","SO2_9","SO2_10",
               "SO2_11","SO2_12",
               "ST3_1","ST3_2","ST3_3","ST3_4","ST3_5","ST3_6","ST3_7","SE3_1",
               "SE3_2","SE3_3","SE3_4","SE3_5","SE3_6","SE3_7","SE3_8","SE3_9",
               "SE3_10","SO3_1","SO3_2","SO3_3","SO3_4","SO3_5","SO3_6","SO3_7",
               "ST3_8","SE3_11","SE3_12","SE3_13","SO3_8","SO3_9","SO3_10",
               "SO3_11","SO3_12")
      d <- d %>% rename(!!!setNames(old, new))
    }
    
  } else if (condition == "CFA") {
    
    MMall_cols <- which(str_detect(names(d), "MM[1,2,3]_"))
    if (length(MMall_cols) > 0) {
      old <- c("MM1_1","MM1_2","MM1_3","MM1_4","MM1_5","MM1_6","MM1_7","MM1_8",
               "MM1_9","MM1_10","MM1_11","MM1_12","MM1_13","MM1_14","MM1_15",
               "MM1_16","MM1_17","MM1_18","MM1_19","MM1_20","MM1_21","MM1_22",
               "MM1_23","MM1_25","MM1_26","MM1_27","MM1_28","MM1_29","MM1_30",
               "MM1_31",
               "MM2_1","MM2_2","MM2_3","MM2_4","MM2_5","MM2_6","MM2_7","MM2_8",
               "MM2_9","MM2_10","MM2_11","MM2_12","MM2_13","MM2_14","MM2_15",
               "MM2_16","MM2_17","MM2_18","MM2_19","MM2_20","MM2_21","MM2_22",
               "MM2_23","MM2_25","MM2_26","MM2_27","MM2_28","MM2_29","MM2_30",
               "MM2_31",
               "MM3_1","MM3_2","MM3_3","MM3_4","MM3_5","MM3_6","MM3_7","MM3_8",
               "MM3_9","MM3_10","MM3_11","MM3_12","MM3_13","MM3_14","MM3_15",
               "MM3_16","MM3_17","MM3_18","MM3_19","MM3_20","MM3_21","MM3_22",
               "MM3_23","MM3_25","MM3_26","MM3_27","MM3_28","MM3_29","MM3_30",
               "MM3_31")
      new <- c("ST1_1","ST1_2","ST1_3","ST1_4","ST1_5","ST1_6","SE1_1","SE1_2",
               "SE1_3","SE1_4","SE1_5","SE1_6","SE1_7","SE1_8","SE1_9","SE1_10",
               "SO1_1","SO1_2","SO1_3","SO1_4","SO1_5","SO1_6","SO1_7","ST1_8",
               "SE1_12","SE1_13","SO1_8","SO1_10","SO1_11","SO1_12",
               "ST2_1","ST2_2","ST2_3","ST2_4","ST2_5","ST2_6","SE2_1","SE2_2",
               "SE2_3","SE2_4","SE2_5","SE2_6","SE2_7","SE2_8","SE2_9","SE2_10",
               "SO2_1","SO2_2","SO2_3","SO2_4","SO2_5","SO2_6","SO2_7","ST2_8",
               "SE2_12","SE2_13","SO2_8","SO2_10","SO2_11","SO2_12",
               "ST3_1","ST3_2","ST3_3","ST3_4","ST3_5","ST3_6","SE3_1","SE3_2",
               "SE3_3","SE3_4","SE3_5","SE3_6","SE3_7","SE3_8","SE3_9","SE3_10",
               "SO3_1","SO3_2","SO3_3","SO3_4","SO3_5","SO3_6","SO3_7","ST3_8",
               "SE3_12","SE3_13","SO3_8","SO3_10","SO3_11","SO3_12")
      d <- d %>% rename(!!!setNames(old, new))
    }
  }
  
  df <- d  # unified reference from here on

# Check control question ---------------------------
  # Q asked participants to select the second value (i.e., 1-[2]-3-4-5)

    if(length(controlQ) > 0){
      c_id<-which(names(df) %in% controlQ)
      inattentive <- df[,c_id]!=2 #(i.e., 1-[2]-3-4-5)
      INATTENTIVE <- data.frame(inattentive) # stupid workaround
      if (ncol(INATTENTIVE)>1){
        inattentive<-rowSums(inattentive)>1
      }
      print(paste("Inattentive total:",sum(inattentive)))
      if(sum(inattentive)>0){
        print("IDs of inattentive participants:")
        print(df$PROLIFIC_PID[inattentive])
        df <- df[!inattentive,] # remove inattentive participants
      }
      df <- dplyr::select(df, -all_of(controlQ)) # remove control question
    }

# Setting up the Prolific ID -----------------------
  prolific_cols <- which(str_detect(tolower(names(df)),"prolific"))

    if(length(prolific_cols) > 0) {
      # If prolific column exists, use it for ID
      df$ID <- df[, prolific_cols[1]]
      df <- df[, -prolific_cols, drop = FALSE]  # Remove the prolific column
    } else {
      # If no prolific column exists, create a warning or default ID
      warning("No prolific column found. Creating sequential ID.")
      df$ID <- 1:nrow(df)
    }
  
# Add Vignette and VigNro columns for within condition -----------------
  
  if (!("Vignette" %in% names(df)) & !("VigNro" %in% names(df))) {
    
    # Detect whether any within-condition items exist (prefixed with 1_, 2_, 3_)
    within_cols <- str_detect(names(df), "^(ST|SE|SO)[1-3]_")
    
    if (any(within_cols)) {
      message("Within condition detected: deriving Vignette and VigNro from item names.")
      df$Vignette <- NA_character_ # No single Vignette per row — each participant did all 3 vignettes
      df$VigNro   <- NA_integer_  # NA here; make_it_long() will resolve this...
    } else {
      warning("Neither Vignette/VigNro columns nor within-condition item prefixes found. Check data.")
    }
  }
  
  df$Vignette <- dplyr::case_when(
  df$Vignette == "By chance, you tune into a documentary about celebrated musicians. Listening to them explain their music gives you a deeper perspective. Their musical story lets you grasp why the music was made and how it has impacted others across the generations." ~ "SO", 
  df$Vignette == "As you are listening to music at home, a song that reminds you of a particular time in your life starts. You find yourself reflecting on why this music brings back previous moments that make you feel a certain way." ~ "SE", 
  df$Vignette == "While listening to your commuting playlist, you become absorbed in how the song unfolds. You notice how the sounds and instruments interact, sense patterns forming, and begin to anticipate what might come next, which makes the impactful moments feel especially powerful." ~ "ST")

  print(paste("Number of accepted participants:", nrow(df)))
  return(df)

}
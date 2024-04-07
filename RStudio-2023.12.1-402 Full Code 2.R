library(stats)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

#### fix the Spawn logging errors: rows that say male instead of female (10 out of 1926 observations) ####
{
  # Load data
  {
    setwd("/Users/anastasia/Desktop/Anastasia's logs/Spawn")
    
    ###TERRITORIAL
    Spawn_1D <- read.csv("logSpawn1D.csv")
    Spawn_1D$Subject <- "S1_T"
    Spawn_3D <- read.csv("logSpawn3D.csv")
    Spawn_3D$Subject <- "S3_T"
    Spawn_5D <- read.csv("logSpawn5D.csv")
    Spawn_5D$Subject <- "S5_T"
    Spawn_8D <- read.csv("logSpawn8D.csv")
    Spawn_8D$Subject <- "S8_T"
    SpawnT <- rbind(Spawn_1D,Spawn_3D,Spawn_5D,Spawn_8D)
    colnames(SpawnT) <- c("Time", "Behavior","TankID")
    SpawnT$Assay <- "Spawn"
    SpawnT$Subject <- "Territorial"
    
    ###NON-TERRITORIAL
    Spawn_1ND <- read.csv("logSpawn1ND.csv")
    Spawn_1ND$Subject <- "S1_NT"
    Spawn_3ND <- read.csv("logSpawn3ND.csv")
    Spawn_3ND$Subject <- "S3_NT"
    Spawn_5ND <- read.csv("logSpawn5ND.csv")
    Spawn_5ND$Subject <- "S5_NT"
    Spawn_8ND <- read.csv("logSpawn8ND.csv")
    Spawn_8ND$Subject <- "S8_NT"
    Spawn_NT <- rbind(Spawn_1ND,Spawn_3ND,Spawn_5ND,Spawn_8ND)
    colnames(Spawn_NT) <- c("Time", "Behavior","TankID")
    Spawn_NT$Assay <- "Spawn"
    Spawn_NT$Subject <- "Non_Territorial"
    AllSpawn <- rbind(SpawnT,Spawn_NT)
  }
  # Check for behaviors in question
  {
    #["chase male"] PRESENT
    value_to_check1 <- "chase male"
    is_present <- value_to_check1 %in% AllSpawn$Behavior
    
    if (is_present) {
      print(paste(value_to_check1, "is present in column Behavior"))
    } else {
      print(paste(value_to_check1, "is not present in column Behavior"))
    }
    
    
    #["attack male"] PRESENT
    value_to_check2 <- "attack male"
    is_present <- value_to_check2 %in% AllSpawn$Behavior
    
    if (is_present) {
      print(paste(value_to_check2, "is present in column Behavior"))
    } else {
      print(paste(value_to_check2, "is not present in column Behavior"))
    }
    
    #["quiver at male"] NOT PRESENT
    value_to_check3 <- "quiver at male" 
    is_present <- value_to_check3 %in% AllSpawn$Behavior
    
    if (is_present) {
      print(paste(value_to_check3, "is present in column Behavior"))
    } else {
      print(paste(value_to_check3, "is not present in column Behavior"))
    }
    
    #["flee from male"] NOT PRESENT
    value_to_check4 <- "flee from male" 
    is_present <- value_to_check4 %in% AllSpawn$Behavior
    
    if (is_present) {
      print(paste(value_to_check4, "is present in column Behavior"))
    } else {
      print(paste(value_to_check4, "is not present in column Behavior"))
    }
  }
  # Replace "male" with "female"
  {
    #["chase male"] - Row 1192
    
    # Value to replace and the new value
    value_to_replace1 <- "chase male"
    new_value1 <- "chase female"
    
    # Find rows where the value is "orange" and replace with "pear"
    rows_to_replace1 <- which(AllSpawn$Behavior == value_to_replace1)
    AllSpawn$Behavior[rows_to_replace1] <- new_value1
    
    
    #["attack male"] - Rows 26  812  886 1022 1357 1415 1454 1598 1767
    
    # Value to replace and the new value
    value_to_replace2 <- "attack male"
    new_value2 <- "attack female"
    
    # Find rows where the value is "orange" and replace with "pear"
    rows_to_replace2 <- which(AllSpawn$Behavior == value_to_replace2)
    AllSpawn$Behavior[rows_to_replace2] <- new_value2
  }
}

#### fix the Spawn time stamps ####
{##################################### format the time stamps correctly 
  
  split_data <- split(AllSpawn, AllSpawn$TankID)
  
  # Iterate over each split dataframe, save it as CSV
  for (tank_id in names(split_data)) {
    tank_df <- split_data[[tank_id]]
    file_name <- paste0("Tank_", tank_id, ".csv")
    write.csv(tank_df, file = file_name, row.names = FALSE)
  }
  
  
  Spawn_1ND <- read.csv("Tank_S1_NT.csv")
  Spawn_3ND <- read.csv("Tank_S3_NT.csv")
  Spawn_5ND <- read.csv("Tank_S5_NT.csv")
  Spawn_8ND <- read.csv("Tank_S8_NT.csv")
  Spawn_1D <- read.csv("Tank_S1_T.csv")
  Spawn_3D <- read.csv("Tank_S3_T.csv")
  Spawn_5D <- read.csv("Tank_S5_T.csv")
  Spawn_8D <- read.csv("Tank_S8_T.csv")
  
  ################################## change the time column formatting for each individual log ################################
  
  # Function to add minutes to time in mm:ss.0 format
  add_minutes <- function(time_string, minutes) {
    time_in_seconds <- as.numeric(as.POSIXct(time_string, format = "%M:%OS")) + minutes * 60
    milliseconds <- round((time_in_seconds - floor(time_in_seconds)) * 10)
    milliseconds <- ifelse(milliseconds == 10, 0, milliseconds) # Round up to 0 if milliseconds reach 10
    updated_time <- format(as.POSIXct(time_in_seconds, origin = "1970-01-01"), format = "%M:%OS")
    return(paste0(updated_time, ".", milliseconds))
  }
  
  #Territorial
  #Spawn_1D
  {
    # Add 09:55.3 to rows 51-62 in mm:ss.0 format
    fixed_time1 <- "09:55.3"
    for (i in 51:62) {
      Spawn_1D$Time[i] <- add_minutes(Spawn_1D$Time[i], as.numeric(difftime(as.POSIXct(fixed_time1, format = "%M:%OS"), as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
    }
    
    #Add 19:02.5 to rows 63-143 in mm:ss.0 format
    fixed_time2 <- "19:02.5" 
    for (i in 63:143) {
      Spawn_1D$Time[i] <- add_minutes(Spawn_1D$Time[i], as.numeric(difftime(as.POSIXct(fixed_time2, format = "%M:%OS"), as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
    }
  }
  #Spawn_3D
  { 
    # Update rows 39 to 68 (add 09:59.9 minutes)
    fixed_time3 <- "09:59.9" 
    for (i in 39:68) {
      Spawn_3D$Time[i] <- add_minutes(Spawn_3D$Time[i], as.numeric(difftime(as.POSIXct(fixed_time3, format = "%M:%OS"), as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
    }
    
    # Update rows 69 to 145 (add 19:48.9 minutes)
    fixed_time4 <- "19:48.9" 
    for (i in 69:145) {
      Spawn_3D$Time[i] <- add_minutes(Spawn_3D$Time[i], as.numeric(difftime(as.POSIXct(fixed_time4, format = "%M:%OS"), as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
    }
  }
  #Spawn_5D
  {
    # Update rows 15 to 44 (add 09:42.3 minutes)
    fixed_time5 <- "09:42.3" 
    for (i in 15:44) {
      Spawn_5D$Time[i] <- add_minutes(Spawn_5D$Time[i], as.numeric(difftime(as.POSIXct(fixed_time5, format = "%M:%OS"), as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
    }
    
    # Update rows 45 to 205 (add 19:36.8 minutes)
    fixed_time6 <- "19:36.8" 
    for (i in 45:205) {
      Spawn_5D$Time[i] <- add_minutes(Spawn_5D$Time[i], as.numeric(difftime(as.POSIXct(fixed_time6, format = "%M:%OS"), as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
    }
  }
  #Spawn_8D
  {
    # Update rows 22 to 169 (add 09:47.0 minutes)
    fixed_time7 <- "09:47.0" 
    for (i in 22:169) {
      Spawn_8D$Time[i] <- add_minutes(Spawn_8D$Time[i], as.numeric(difftime(as.POSIXct(fixed_time7, format = "%M:%OS"), as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
    }
    
    # Update rows 170 to 263 (add 19:52.7 minutes)
    fixed_time8 <- "19:52.7" 
    for (i in 170:263) {
      Spawn_8D$Time[i] <- add_minutes(Spawn_8D$Time[i], as.numeric(difftime(as.POSIXct(fixed_time8, format = "%M:%OS"), as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
    }
  }
  
  #Non-Territorial
  #Spawn_1ND
  {
    # Add 09:57.1 to rows 103-340 in mm:ss.0 format
    fixed_time9 <- "09:57.1"
    for (i in 103:340) {
      Spawn_1ND$Time[i] <- add_minutes(Spawn_1ND$Time[i], as.numeric(difftime(as.POSIXct(fixed_time9, format = "%M:%OS"), as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
    }
    
    #Add 19:53.9 to rows 341-509 in mm:ss.0 format
    fixed_time10 <- "19:53.9" 
    for (i in 341:509) {
      Spawn_1ND$Time[i] <- add_minutes(Spawn_1ND$Time[i], as.numeric(difftime(as.POSIXct(fixed_time10, format = "%M:%OS"), as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
    }
  }
  #Spawn_3ND
  #Spawn_3D
  { 
    # Update rows 52 to 202 (add 10:00.0 minutes)
    fixed_time11 <- "10:00.0" 
    for (i in 52:202) {
      Spawn_3ND$Time[i] <- add_minutes(Spawn_3ND$Time[i], as.numeric(difftime(as.POSIXct(fixed_time11, format = "%M:%OS"), as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
    }
    
    # Update rows 203 to 340 (add 19:58.3 minutes)
    fixed_time12 <- "19:58.3" 
    for (i in 203:340) {
      Spawn_3ND$Time[i] <- add_minutes(Spawn_3ND$Time[i], as.numeric(difftime(as.POSIXct(fixed_time12, format = "%M:%OS"), as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
    }
  }
  #Spawn_5ND
  {
    # Update rows 26 to 57 (add 09:19.9 minutes)
    fixed_time13 <- "09:19.9" 
    for (i in 26:57) {
      Spawn_5ND$Time[i] <- add_minutes(Spawn_5ND$Time[i], as.numeric(difftime(as.POSIXct(fixed_time13, format = "%M:%OS"), as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
    }
    
    # Update rows 58 to 119 (add 19:13.8 minutes)
    fixed_time14 <- "19:13.8" 
    for (i in 58:119) {
      Spawn_5ND$Time[i] <- add_minutes(Spawn_5ND$Time[i], as.numeric(difftime(as.POSIXct(fixed_time14, format = "%M:%OS"), as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
    }
  }  
  #Spawn_8ND
  {
  # Update rows 38 to 132 (add 10:00.4 minutes)
  fixed_time15 <- "10:00.4" 
  for (i in 38:132) {
    Spawn_8ND$Time[i] <- add_minutes(Spawn_8ND$Time[i], as.numeric(difftime(as.POSIXct(fixed_time15, format = "%M:%OS"), as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
  }
  
  # Update rows 133 to 202 (add 19:55.2 minutes)
  fixed_time16 <- "19:55.2" 
  for (i in 133:202) {
    Spawn_8ND$Time[i] <- add_minutes(Spawn_8ND$Time[i], as.numeric(difftime(as.POSIXct(fixed_time16, format = "%M:%OS"), as.POSIXct("00:00.0", format = "%M:%OS"), units = "mins")))
  }
}
  
  # Combine the dataframes
  combined_df <- rbind(Spawn_1ND, Spawn_3ND, Spawn_5ND, Spawn_8ND, Spawn_1D, Spawn_3D, Spawn_5D, Spawn_8D)
}

#### combine Dyad and Spawn logs ####
{  
  #Load the data
  {  
    setwd("/Users/anastasia/Desktop/Anastasia's logs/Dyad")
    
    Dyad_1D <- read.csv("logDyad1D.csv")
    Dyad_1D$Subject <- "D1_T"
    Dyad_3D <- read.csv("logDyad3D.csv")
    Dyad_3D$Subject <- "D3_T"
    Dyad_5D <- read.csv("logDyad5D.csv")
    Dyad_5D$Subject <- "D5_T"
    Dyad_8D <- read.csv("logDyad8D.csv")
    Dyad_8D$Subject <- "D8_T"
    
    Dyad_T <- rbind(Dyad_1D,Dyad_3D,Dyad_5D,Dyad_8D)
    colnames(Dyad_T) <- c("Time", "Behavior","TankID")
    Dyad_T$Subject <- "Territorial"
    
    Dyad_1ND <- read.csv("logDyad1ND.csv")
    Dyad_1ND$Subject <- "D1_NT"
    Dyad_3ND <- read.csv("logDyad3ND.csv")
    Dyad_3ND$Subject <- "D3_NT"
    Dyad_5ND <- read.csv("logDyad5ND.csv")
    Dyad_5ND$Subject <- "D5_NT"
    Dyad_8ND <- read.csv("logDyad8ND.csv")
    Dyad_8ND$Subject <- "D8_NT"
    
    setwd("/Users/anastasia/Desktop/correct timestamps. 12.13.23")
    
    Spawn1D <- read.csv("Spawn1D.correct.timestamps.no.male.csv")
    Spawn3D <- read.csv("Spawn3D.correct.timestamps.no.male.csv")
    Spawn5D <- read.csv("Spawn5D.correct.timestamps.no.male.csv")
    Spawn8D <- read.csv("Spawn8D.correct.timestamps.no.male.csv")
    Spawn_T <- rbind(Spawn1D, Spawn3D, Spawn5D, Spawn8D)
    column_to_remove <- "X" 
    Spawn_T <- Spawn_T[, !colnames(Spawn_T) %in% column_to_remove]
    colnames(Spawn_T) <- c("Time", "Behavior","TankID")
    Spawn_T$Subject <- "Territorial"  
    
    Spawn1ND <- read.csv("Spawn1ND.correct.timestamps.no.male.csv")
    Spawn3ND <- read.csv("Spawn3ND.correct.timestamps.no.male.csv")
    Spawn5ND <- read.csv("Spawn5ND.correct.timestamps.no.male.csv")
    Spawn8ND <- read.csv("Spawn8ND.correct.timestamps.no.male.csv")
    Spawn_NT <- rbind(Spawn1ND, Spawn3ND, Spawn5ND, Spawn8ND) 
    Spawn_NT <- Spawn_NT[, !colnames(Spawn_NT) %in% column_to_remove]
    colnames(Spawn_NT) <- c("Time", "Behavior","TankID")
    Spawn_NT$Subject <- "Non_Territorial"
  }
  
  #Get all dyad data in one dataframe
  Dyad_T <- rbind(Dyad_1D,Dyad_3D,Dyad_5D,Dyad_8D)
  colnames(Dyad_T) <- c("Time", "Behavior","TankID")
  Dyad_T$Subject <- "Territorial"
  Dyad_NT <- rbind(Dyad_1ND,Dyad_3ND,Dyad_5ND,Dyad_8ND)
  colnames(Dyad_NT) <- c("Time", "Behavior","TankID")
  Dyad_NT$Subject <- "Non_Territorial"
  Dyad.t.nt <- rbind(Dyad_T,Dyad_NT)
  Dyad.t.nt$Assay <- "Dyad"
  #Get all spawn data in one dataframe
  Spawn.t.nt <- rbind(Spawn_T,Spawn_NT)
  Spawn.t.nt$Assay <- "Spawn"
  #All data in one dataframe
  T.NT.within.assays <- rbind(Dyad.t.nt,Spawn.t.nt) 
}

#### remove male oriented behaviors from Dyad (so we can run paired-t tests) ####

{
  #Reformat T.NT.within.assays so it includes frequency count by Subject/Behavior/Assay
  summary_T.NT.within.assays <- T.NT.within.assays %>%
    group_by(Subject, Behavior, Assay,TankID) %>%
    summarise(Frequency = n())
  
  #remove male oriented behavior
  # Assuming 'df' is your dataframe
  values_to_remove <- c("chase male", "attack male", "quiver at male", "flee from male")
  
  # Remove rows with specific values in 'Behavior' column
  T.NT.within.assays <- T.NT.within.assays[!(T.NT.within.assays$Behavior %in% values_to_remove), ]
  summary_T.NT.within.assays <- summary_T.NT.within.assays[!(summary_T.NT.within.assays$Behavior %in% values_to_remove), ]
}

#dataframe for Territorial (DyadT and SpawnT)
{
  Territorial.d.s <- T.NT.within.assays[T.NT.within.assays$Subject == "Territorial", ]
  
  #Dyad
  T.d <- Territorial.d.s[Territorial.d.s$Assay == "Dyad", ]
  summaryT.d <- T.d %>%
    group_by(Subject, Behavior, Assay,TankID) %>%
    summarise(Frequency = n())
  #Add 0s, modify TankIDs
  {
    # Get unique behaviors list from Territorial.d.s dataframe
    unique_behaviors <- unique(Territorial.d.s$Behavior)
    
    # Initialize an empty dataframe to store missing behaviors
    missing_behaviors <- data.frame(
      Subject = character(),
      Behavior = character(),
      Assay = character(),
      TankID = character(),
      Frequency = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Iterate through each TankID in summaryT.d
    for (tank_id in unique(summaryT.d$TankID)) {
      # Get behaviors for the current TankID
      behaviors <- summaryT.d$Behavior[summaryT.d$TankID == tank_id]
      
      # Check for missing behaviors
      missing <- setdiff(unique_behaviors, behaviors)
      
      # Add missing behaviors to the missing_behaviors dataframe
      if (length(missing) > 0) {
        missing_rows <- data.frame(
          Subject = rep("Territorial", length(missing)),
          Behavior = missing,
          Assay = rep("Dyad", length(missing)),
          TankID = rep(tank_id, length(missing)),
          Frequency = rep(0, length(missing)),
          stringsAsFactors = FALSE
        )
        missing_behaviors <- rbind(missing_behaviors, missing_rows)
      }
    }
    # Combine summaryT.s with missing_behaviors
    summaryT.d <- rbind(summaryT.d, missing_behaviors)
    
    # Define the transformation function
    transform_tank_id <- function(tank_id) {
      ifelse(tank_id == "D1_T", "1",
             ifelse(tank_id == "D3_T", "3",
                    ifelse(tank_id == "D5_T", "5",
                           ifelse(tank_id == "D8_T", "8", NA))))
    }
    
    # Apply the transformation and replace the existing column
    summaryT.d$TankID <- transform_tank_id(summaryT.d$TankID)
    }
  
  
  #Spawn
  T.s <- Territorial.d.s[Territorial.d.s$Assay == "Spawn", ]
  summaryT.s <- T.s %>%
    group_by(Subject, Behavior, Assay,TankID) %>%
    summarise(Frequency = n())
  #Add 0s
  {
    # Get unique behaviors list from Territorial.d.s dataframe
    unique_behaviors <- unique(Territorial.d.s$Behavior)
    
    # Initialize an empty dataframe to store missing behaviors
    missing_behaviors <- data.frame(
      Subject = character(),
      Behavior = character(),
      Assay = character(),
      TankID = character(),
      Frequency = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Iterate through each TankID in summaryT.s
    for (tank_id in unique(summaryT.s$TankID)) {
      # Get behaviors for the current TankID
      behaviors <- summaryT.s$Behavior[summaryT.s$TankID == tank_id]
      
      # Check for missing behaviors
      missing <- setdiff(unique_behaviors, behaviors)
      
      # Add missing behaviors to the missing_behaviors dataframe
      if (length(missing) > 0) {
        missing_rows <- data.frame(
          Subject = rep("Territorial", length(missing)),
          Behavior = missing,
          Assay = rep("Spawn", length(missing)),
          TankID = rep(tank_id, length(missing)),
          Frequency = rep(0, length(missing)),
          stringsAsFactors = FALSE
        )
        missing_behaviors <- rbind(missing_behaviors, missing_rows)
      }
    }
    
    # Combine summaryT.s with missing_behaviors
    summaryT.s <- rbind(summaryT.s, missing_behaviors)
    
    # Define the transformation function
    transform_tank_id <- function(tank_id) {
      ifelse(tank_id == "S1_T", "1",
             ifelse(tank_id == "S3_T", "3",
                    ifelse(tank_id == "S5_T", "5",
                           ifelse(tank_id == "S8_T", "8", NA))))
    }
    
    # Apply the transformation and replace the existing column
    summaryT.s$TankID <- transform_tank_id(summaryT.s$TankID)
    }
}

#dataframe for Non-Territorial (DyadNT and SpawnNT)
{
  Non_Territorial.d.s <- T.NT.within.assays[T.NT.within.assays$Subject == "Non_Territorial", ]
  
  #Dyad
  NT.d <- Non_Territorial.d.s[Non_Territorial.d.s$Assay == "Dyad", ]
  summaryNT.d <- NT.d %>%
    group_by(Subject, Behavior, Assay,TankID) %>%
    summarise(Frequency = n())
  #Add 0s
  {
    # Get unique behaviors list from T.NT.within.assays dataframe
    unique_behaviors <- unique(T.NT.within.assays$Behavior)
    
    # Initialize an empty dataframe to store missing behaviors
    missing_behaviors <- data.frame(
      Subject = character(),
      Behavior = character(),
      Assay = character(),
      TankID = character(),
      Frequency = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Iterate through each TankID in summaryT.d
    for (tank_id in unique(summaryNT.d$TankID)) {
      # Get behaviors for the current TankID
      behaviors <- summaryNT.d$Behavior[summaryNT.d$TankID == tank_id]
      
      # Check for missing behaviors
      missing <- setdiff(unique_behaviors, behaviors)
      
      # Add missing behaviors to the missing_behaviors dataframe
      if (length(missing) > 0) {
        missing_rows <- data.frame(
          Subject = rep("Non_Territorial", length(missing)),
          Behavior = missing,
          Assay = rep("Dyad", length(missing)),
          TankID = rep(tank_id, length(missing)),
          Frequency = rep(0, length(missing)),
          stringsAsFactors = FALSE
        )
        missing_behaviors <- rbind(missing_behaviors, missing_rows)
      }
    }
    
    # Combine summaryT.d with missing_behaviors
    summaryNT.d <- rbind(summaryNT.d, missing_behaviors)
    
    # Define the transformation function
    transform_tank_id <- function(tank_id) {
      ifelse(tank_id == "D1_NT", "1",
             ifelse(tank_id == "D3_NT", "3",
                    ifelse(tank_id == "D5_NT", "5",
                           ifelse(tank_id == "D8_NT", "8", NA))))
    }
    
    # Apply the transformation and replace the existing column
    summaryNT.d$TankID <- transform_tank_id(summaryNT.d$TankID)
    }
  
  #Spawn
  NT.s <- Non_Territorial.d.s[Non_Territorial.d.s$Assay == "Spawn", ]
  summaryNT.s <- NT.s %>%
    group_by(Subject, Behavior, Assay,TankID) %>%
    summarise(Frequency = n())
  #Add 0s
  {
    # Get unique behaviors list from T.NT.within.assays dataframe
    unique_behaviors <- unique(T.NT.within.assays$Behavior)
    
    # Initialize an empty dataframe to store missing behaviors
    missing_behaviors <- data.frame(
      Subject = character(),
      Behavior = character(),
      Assay = character(),
      TankID = character(),
      Frequency = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Iterate through each TankID in summaryT.d
    for (tank_id in unique(summaryNT.s$TankID)) {
      # Get behaviors for the current TankID
      behaviors <- summaryNT.s$Behavior[summaryNT.s$TankID == tank_id]
      
      # Check for missing behaviors
      missing <- setdiff(unique_behaviors, behaviors)
      
      # Add missing behaviors to the missing_behaviors dataframe
      if (length(missing) > 0) {
        missing_rows <- data.frame(
          Subject = rep("Non_Territorial", length(missing)),
          Behavior = missing,
          Assay = rep("Spawn", length(missing)),
          TankID = rep(tank_id, length(missing)),
          Frequency = rep(0, length(missing)),
          stringsAsFactors = FALSE
        )
        missing_behaviors <- rbind(missing_behaviors, missing_rows)
      }
    }
    
    # Combine summaryT.d with missing_behaviors
    summaryNT.s <- rbind(summaryNT.s, missing_behaviors)
    
    # Define the transformation function
    transform_tank_id <- function(tank_id) {
      ifelse(tank_id == "S1_NT", "1",
             ifelse(tank_id == "S3_NT", "3",
                    ifelse(tank_id == "S5_NT", "5",
                           ifelse(tank_id == "S8_NT", "8", NA))))
    }
    
    # Apply the transformation and replace the existing column
    summaryNT.s$TankID <- transform_tank_id(summaryNT.s$TankID)
    }
}

#Paired t-tests with benjamini hochberg correction for individual behaviors 
{
  #DyadT vs Dyad NT
  {
    # Define Benjamini-Hochberg correction function
    bh_correction <- function(p_values, alpha = 0.05) {
      n <- length(p_values)
      ranked_p_values <- rank(p_values)
      q_values <- (ranked_p_values / n) * alpha
      p_values < q_values
    }
    
    # Paired t-test for Dyad assay (DyadT and DyadNT) with Benjamini-Hochberg correction
    paired_t_test_bh <- function(summaryT.d, summaryNT.d, alpha = 0.05) {
      # Merge the two dataframes based on TankID and Behavior
      merged_data <- merge(summaryT.d, summaryNT.d, by = c("TankID", "Behavior"))
      
      # Perform paired t-test for each behavior
      behaviors <- unique(merged_data$Behavior)
      significant_results <- list()
      for (behavior in behaviors) {
        subset_data <- merged_data[merged_data$Behavior == behavior, ]
        t_test_result <- t.test(subset_data$Frequency.x, subset_data$Frequency.y, paired = TRUE)
        if (t_test_result$p.value < alpha) {
          # Extract df and t-value
          df <- t_test_result$parameter
          t_value <- t_test_result$statistic
          
          p_adjusted <- t_test_result$p.value  # Initialize adjusted p-value
          significant_results[[behavior]] <- c(t_value, df, p_adjusted)
        }
      }
      
      # Combine significant results into a dataframe
      significant_df <- as.data.frame(do.call(rbind, significant_results))
      colnames(significant_df) <- c("t_value", "df", "p_adjusted")
      significant_df$Behavior <- rownames(significant_df)
      row.names(significant_df) <- NULL
      
      # Apply Benjamini-Hochberg correction
      p_values <- significant_df$p_adjusted
      adjusted <- bh_correction(p_values, alpha)
      significant_df$p_adjusted <- ifelse(adjusted, p_values, NA)
      
      # Print out df, t-value, and adjusted p-values
      print(significant_df[, c("Behavior", "df", "t_value", "p_adjusted")])
    }
    
    # Call the function with your dataframes summaryT.d and summaryNT.d
    paired_t_test_bh(summaryT.d, summaryNT.d)
  }
  #SpawnT vs Spawn NT
  {
    # Define Benjamini-Hochberg correction function
    bh_correction <- function(p_values, alpha = 0.05) {
      n <- length(p_values)
      ranked_p_values <- rank(p_values)
      q_values <- (ranked_p_values / n) * alpha
      p_values < q_values
    }
    
    # Paired t-test for Spawn assay (SpawnT and SpawnNT) with Benjamini-Hochberg correction
    paired_t_test_bh <- function(summaryT.s, summaryNT.s, alpha = 0.05) {
      # Merge the two dataframes based on TankID and Behavior
      merged_data <- merge(summaryT.s, summaryNT.s, by = c("TankID", "Behavior"))
      
      # Perform paired t-test for each behavior
      behaviors <- unique(merged_data$Behavior)
      significant_results <- list()
      for (behavior in behaviors) {
        subset_data <- merged_data[merged_data$Behavior == behavior, ]
        t_test_result <- t.test(subset_data$Frequency.x, subset_data$Frequency.y, paired = TRUE)
        if (t_test_result$p.value < alpha) {
          p_adjusted <- t_test_result$p.value  # Initialize adjusted p-value
          significant_results[[behavior]] <- c(t_test_result$p.value, p_adjusted)
        }
      }
      
      # Check if there are significant results
      if (length(significant_results) == 0) {
        cat("No significant results found.\n")
        return(NULL)
      }
      
      # Combine significant results into a dataframe
      significant_df <- as.data.frame(do.call(rbind, significant_results))
      colnames(significant_df) <- c("p_value", "p_adjusted")
      significant_df$Behavior <- rownames(significant_df)
      row.names(significant_df) <- NULL
      
      # Apply Benjamini-Hochberg correction
      p_values <- significant_df$p_value
      adjusted <- bh_correction(p_values, alpha)
      significant_df$p_adjusted <- ifelse(adjusted, p_values, NA)
      
      # Print out regular and adjusted p-values
      print(significant_df[, c("Behavior", "p_value", "p_adjusted")])
    }
    
    # Call the function with your dataframes summaryT.s and summaryNT.s
    paired_t_test_bh(summaryT.s, summaryNT.s)
  }
  #DyadT vs Spawn T
  {
    # Define Benjamini-Hochberg correction function
    bh_correction <- function(p_values, alpha = 0.05) {
      n <- length(p_values)
      ranked_p_values <- rank(p_values)
      q_values <- (ranked_p_values / n) * alpha
      p_values < q_values
    }
    
    # Paired t-test for Territorial subjects (DyadT and SpawnT) with Benjamini-Hochberg correction
    paired_t_test_bh <- function(summaryT.s, summaryT.d, alpha = 0.05) {
      # Merge the two dataframes based on TankID and Behavior
      merged_data <- merge(summaryT.s, summaryT.d, by = c("TankID", "Behavior"))
      
      # Perform paired t-test for each behavior
      behaviors <- unique(merged_data$Behavior)
      results <- data.frame(Behavior = character(), 
                            t_value = numeric(), 
                            df = numeric(),  # Add degrees of freedom column
                            p_value = numeric(),
                            p_adjusted = numeric(),
                            stringsAsFactors = FALSE)
      
      for (behavior in behaviors) {
        subset_data <- merged_data[merged_data$Behavior == behavior, ]
        t_test_result <- t.test(subset_data$Frequency.x, subset_data$Frequency.y, paired = TRUE)
        if (!is.na(t_test_result$p.value)) {
          df <- t_test_result$parameter
          t_value <- t_test_result$statistic
          p_value <- t_test_result$p.value
          p_adjusted <- p.adjust(p_value, method = "BH")
          results <- rbind(results, data.frame(Behavior = behavior,
                                               t_value = t_value,
                                               df = df,
                                               p_value = p_value,
                                               p_adjusted = p_adjusted))
        }
      }
      
      # Print out df, t-value, and p-values
      print(results[, c("Behavior", "df", "t_value", "p_value", "p_adjusted")])
    }
    
    # Call the function with your dataframes summaryT.s and summaryT.d
    paired_t_test_bh(summaryT.s, summaryT.d)
    
  }
  #DyadNT vs Spawn NT
  {
    # Define Benjamini-Hochberg correction function
    bh_correction <- function(p_values, alpha = 0.05) {
      n <- length(p_values)
      ranked_p_values <- rank(p_values)
      q_values <- (ranked_p_values / n) * alpha
      p_values < q_values
    }
    
    # Paired t-test for Non-Territorial subjects (DyadNT and SpawnNT) with Benjamini-Hochberg correction
    paired_t_test_bh <- function(summaryNT.s, summaryNT.d, alpha = 0.05) {
      # Merge the two dataframes based on TankID and Behavior
      merged_data <- merge(summaryNT.s, summaryNT.d, by = c("TankID", "Behavior"))
      
      # Perform paired t-test for each behavior
      behaviors <- unique(merged_data$Behavior)
      results <- data.frame(Behavior = character(), 
                            t_value = numeric(), 
                            df = numeric(),  # Add degrees of freedom column
                            p_value = numeric(),
                            p_adjusted = numeric(),
                            stringsAsFactors = FALSE)
      
      for (behavior in behaviors) {
        subset_data <- merged_data[merged_data$Behavior == behavior, ]
        t_test_result <- t.test(subset_data$Frequency.x, subset_data$Frequency.y, paired = TRUE)
        if (!is.na(t_test_result$p.value)) {
          df <- t_test_result$parameter
          t_value <- t_test_result$statistic
          p_value <- t_test_result$p.value
          p_adjusted <- p.adjust(p_value, method = "BH")
          results <- rbind(results, data.frame(Behavior = behavior,
                                               t_value = t_value,
                                               df = df,
                                               p_value = p_value,
                                               p_adjusted = p_adjusted))
        }
      }
      
      # Print out df, t-value, and p-values
      print(results[, c("Behavior", "df", "t_value", "p_value", "p_adjusted")])
    }
    
    # Call the function with your dataframes summaryNT.s and summaryNT.d
    paired_t_test_bh(summaryNT.s, summaryNT.d)
    
  }
}

#Paired t-tests with benjamini hochberg correction for categorical behaviors 
{
  #combine all data 
  AllData <- rbind(summaryT.d,summaryNT.d,summaryT.s,summaryNT.s)
  
  AllData <- AllData %>%
    mutate(Category = case_when(
      grepl("pot exit", Behavior) ~ "Reproductive", # If Behavior contains "pot exit", set Category to "Aversive"
      grepl("pot entry", Behavior) ~ "Reproductive",
      grepl("attack female", Behavior) ~ "Reproductive",
      grepl("lead swim", Behavior) ~ "Reproductive",
      grepl("dig", Behavior) ~ "Reproductive",
      grepl("quiver at female", Behavior) ~ "Reproductive",
      grepl("chase female", Behavior) ~ "Agressive",
      grepl("lateral display", Behavior) ~ "Agressive",
      grepl("chase male", Behavior) ~ "Agressive",
      grepl("attack male", Behavior) ~ "Agressive",
      grepl("quiver at male", Behavior) ~ "Agressive",
      grepl("frontal display", Behavior) ~ "Agressive",
      grepl("flee from female", Behavior) ~ "Aversive",
      grepl("flee from male", Behavior) ~ "Aversive"
    ))
  # Separate data by categories
  aggressive <- subset(AllData, Category == "Agressive")
  aversive <- subset(AllData, Category == "Aversive")
  reproductive <- subset(AllData, Category == "Reproductive")
  
  #AGRESSIVE IN Dyad (dyad t vs dyad nt) 
  {# Filter data for Dyad assay
    dyad_data <- subset(aggressive, Assay == "Dyad")
    
    # Perform paired t-test for each category
    categories <- unique(dyad_data$Category)
    results_dyad <- data.frame(Category = character(), 
                               t_value = numeric(), 
                               df = numeric(),  # Add degrees of freedom column
                               p_value = numeric(),
                               p_adjusted = numeric(),  # Add adjusted p-value column
                               stringsAsFactors = FALSE)
    
    # Perform paired t-test for each category with Benjamini-Hochberg correction
    for (cat in categories) {
      territorial <- subset(dyad_data, Subject == "Territorial" & Category == cat)$Frequency
      non_territorial <- subset(dyad_data, Subject == "Non_Territorial" & Category == cat)$Frequency
      result <- t.test(territorial, non_territorial, paired = TRUE)
      
      # Extract df and t-value
      df <- result$parameter
      t_value <- result$statistic
      
      # Apply Benjamini-Hochberg correction
      p_value <- result$p.value
      p_adjusted <- p.adjust(p_value, method = "BH")
      
      results_dyad <- rbind(results_dyad, data.frame(Category = cat,
                                                     t_value = t_value,
                                                     df = df,
                                                     p_value = p_value,
                                                     p_adjusted = p_adjusted))
    }
    
    # Print results for Dyad assay
    cat("Results for Dyad assay:\n")
    print(results_dyad)
  }
  #AGRESSIVE IN spawn (spawn t vs spawn nt)
  {
    # Filter data for spawn assay
    spawn_data <- subset(aggressive, Assay == "Spawn")
    
    # Perform paired t-test for each category
    categories <- unique(spawn_data$Category)
    results_spawn <- data.frame(Category = character(), 
                                t_value = numeric(), 
                                df = numeric(),  # Add degrees of freedom column
                                p_value = numeric(),
                                p_adjusted = numeric(),  # Add adjusted p-value column
                                stringsAsFactors = FALSE)
    
    # Perform paired t-test for each category with Benjamini-Hochberg correction
    for (cat in categories) {
      territorial <- subset(spawn_data, Subject == "Territorial" & Category == cat)$Frequency
      non_territorial <- subset(spawn_data, Subject == "Non_Territorial" & Category == cat)$Frequency
      result <- t.test(territorial, non_territorial, paired = TRUE)
      
      # Extract df and t-value
      df <- result$parameter
      t_value <- result$statistic
      
      # Apply Benjamini-Hochberg correction
      p_value <- result$p.value
      p_adjusted <- p.adjust(p_value, method = "BH")
      
      results_spawn <- rbind(results_spawn, data.frame(Category = cat,
                                                       t_value = t_value,
                                                       df = df,
                                                       p_value = p_value,
                                                       p_adjusted = p_adjusted))
    }
    
    # Print results for spawn assay
    cat("Results for spawn assay:\n")
    print(results_spawn)
  }
  #AGGRESSIVE IN TERRITORIALS (dyad t vs spawn t)
  {
    # Filter data for territorial subject
    Territorial_data <- subset(aggressive, Subject == "Territorial")
    
    # Perform paired t-test for each category
    categories <- unique(Territorial_data$Category)
    results_territorial <- data.frame(Category = character(), 
                                      t_value = numeric(), 
                                      df = numeric(),  # Add degrees of freedom column
                                      p_value = numeric(),
                                      p_adjusted = numeric(),  # Add adjusted p-value column
                                      stringsAsFactors = FALSE)
    
    # Perform paired t-test for each category with Benjamini-Hochberg correction
    for (cat in categories) {
      dyad <- subset(Territorial_data, Assay == "Dyad" & Category == cat)$Frequency
      spawn <- subset(Territorial_data, Assay == "Spawn" & Category == cat)$Frequency
      result <- t.test(dyad, spawn, paired = TRUE)
      
      # Extract df and t-value
      df <- result$parameter
      t_value <- result$statistic
      
      # Apply Benjamini-Hochberg correction
      p_value <- result$p.value
      p_adjusted <- p.adjust(p_value, method = "BH")
      
      results_territorial <- rbind(results_territorial, data.frame(Category = cat,
                                                                   t_value = t_value,
                                                                   df = df,
                                                                   p_value = p_value,
                                                                   p_adjusted = p_adjusted))
    }
    
    # Print results for Territorial 
    cat("Results for Territorial:\n")
    print(results_territorial)
  }
  #AGGRESSIVE IN NON_TERRITORIALS (dyad nt vs spawn nt)
  {
    # Filter data for territorial subject
    Non_Territorial_data <- subset(aggressive, Subject == "Non_Territorial")
    
    # Perform paired t-test for each category
    categories <- unique(Non_Territorial_data$Category)
    results_non_territorial <- data.frame(Category = character(), 
                                          t_value = numeric(), 
                                          df = numeric(),  # Add degrees of freedom column
                                          p_value = numeric(),
                                          p_adjusted = numeric(),  # Add adjusted p-value column
                                          stringsAsFactors = FALSE)
    
    # Perform paired t-test for each category with Benjamini-Hochberg correction
    for (cat in categories) {
      dyad <- subset(Non_Territorial_data, Assay == "Dyad" & Category == cat)$Frequency
      spawn <- subset(Non_Territorial_data, Assay == "Spawn" & Category == cat)$Frequency
      result <- t.test(dyad, spawn, paired = TRUE)
      
      # Extract df and t-value
      df <- result$parameter
      t_value <- result$statistic
      
      # Apply Benjamini-Hochberg correction
      p_value <- result$p.value
      p_adjusted <- p.adjust(p_value, method = "BH")
      
      results_non_territorial <- rbind(results_non_territorial, data.frame(Category = cat,
                                                                           t_value = t_value,
                                                                           df = df,
                                                                           p_value = p_value,
                                                                           p_adjusted = p_adjusted))
    }
    
    # Print results for Non_Territorial 
    cat("Results for Non_Territorial:\n")
    print(results_non_territorial)
  }
  
  #AVERSIVE IN Dyad T (dyad t vs dyad nt) 
  {# Filter data for Dyad assay
    dyad_data <- subset(aversive, Assay == "Dyad")
    
    # Perform paired t-test for each category
    categories <- unique(dyad_data$Category)
    results_dyad <- data.frame(Category = character(), 
                               t_value = numeric(), 
                               df = numeric(),  # Add degrees of freedom column
                               p_value = numeric(),
                               p_adjusted = numeric(),  # Add adjusted p-value column
                               stringsAsFactors = FALSE)
    
    # Perform paired t-test for each category with Benjamini-Hochberg correction
    for (cat in categories) {
      territorial <- subset(dyad_data, Subject == "Territorial" & Category == cat)$Frequency
      non_territorial <- subset(dyad_data, Subject == "Non_Territorial" & Category == cat)$Frequency
      result <- t.test(territorial, non_territorial, paired = TRUE)
      
      # Extract df and t-value
      df <- result$parameter
      t_value <- result$statistic
      
      # Apply Benjamini-Hochberg correction
      p_value <- result$p.value
      p_adjusted <- p.adjust(p_value, method = "BH")
      
      results_dyad <- rbind(results_dyad, data.frame(Category = cat,
                                                     t_value = t_value,
                                                     df = df,
                                                     p_value = p_value,
                                                     p_adjusted = p_adjusted))
    }
    
    # Print results for Dyad assay
    cat("Results for Dyad assay:\n")
    print(results_dyad)
  }
  #AVERSIVE IN spawn (spawn t vs spawn nt)
  {
    # Filter data for spawn assay
    spawn_data <- subset(aversive, Assay == "Spawn")
    
    # Perform paired t-test for each category
    categories <- unique(spawn_data$Category)
    results_spawn <- data.frame(Category = character(), 
                                t_value = numeric(), 
                                df = numeric(),  # Add degrees of freedom column
                                p_value = numeric(),
                                p_adjusted = numeric(),  # Add adjusted p-value column
                                stringsAsFactors = FALSE)
    
    # Perform paired t-test for each category with Benjamini-Hochberg correction
    for (cat in categories) {
      territorial <- subset(spawn_data, Subject == "Territorial" & Category == cat)$Frequency
      non_territorial <- subset(spawn_data, Subject == "Non_Territorial" & Category == cat)$Frequency
      result <- t.test(territorial, non_territorial, paired = TRUE)
      
      # Extract df and t-value
      df <- result$parameter
      t_value <- result$statistic
      
      # Apply Benjamini-Hochberg correction
      p_value <- result$p.value
      p_adjusted <- p.adjust(p_value, method = "BH")
      
      results_spawn <- rbind(results_spawn, data.frame(Category = cat,
                                                       t_value = t_value,
                                                       df = df,
                                                       p_value = p_value,
                                                       p_adjusted = p_adjusted))
    }
    
    # Print results for spawn assay
    cat("Results for spawn assay:\n")
    print(results_spawn)
  }
  #AVERSIVE IN TERRITORIALS (dyad t vs spawn t)
  {
    # Filter data for territorial subject
    Territorial_data <- subset(aversive, Subject == "Territorial")
    
    # Perform paired t-test for each category
    categories <- unique(Territorial_data$Category)
    results_territorial <- data.frame(Category = character(), 
                                      t_value = numeric(), 
                                      df = numeric(),  # Add degrees of freedom column
                                      p_value = numeric(),
                                      p_adjusted = numeric(),  # Add adjusted p-value column
                                      stringsAsFactors = FALSE)
    
    # Perform paired t-test for each category with Benjamini-Hochberg correction
    for (cat in categories) {
      dyad <- subset(Territorial_data, Assay == "Dyad" & Category == cat)$Frequency
      spawn <- subset(Territorial_data, Assay == "Spawn" & Category == cat)$Frequency
      result <- t.test(dyad, spawn, paired = TRUE)
      
      # Extract df and t-value
      df <- result$parameter
      t_value <- result$statistic
      
      # Apply Benjamini-Hochberg correction
      p_value <- result$p.value
      p_adjusted <- p.adjust(p_value, method = "BH")
      
      results_territorial <- rbind(results_territorial, data.frame(Category = cat,
                                                                   t_value = t_value,
                                                                   df = df,
                                                                   p_value = p_value,
                                                                   p_adjusted = p_adjusted))
    }
    
    # Print results for Territorial 
    cat("Results for Territorial:\n")
    print(results_territorial)
  }
  #AVERSIVE IN NON_TERRITORIALS (dyad t vs spawn t)
  {
    # Filter data for territorial subject
    Non_Territorial_data <- subset(aversive, Subject == "Non_Territorial")
    
    # Perform paired t-test for each category
    categories <- unique(Non_Territorial_data$Category)
    results_non_territorial <- data.frame(Category = character(), 
                                          t_value = numeric(), 
                                          df = numeric(),  # Add degrees of freedom column
                                          p_value = numeric(),
                                          p_adjusted = numeric(),  # Add adjusted p-value column
                                          stringsAsFactors = FALSE)
    
    # Perform paired t-test for each category with Benjamini-Hochberg correction
    for (cat in categories) {
      dyad <- subset(Non_Territorial_data, Assay == "Dyad" & Category == cat)$Frequency
      spawn <- subset(Non_Territorial_data, Assay == "Spawn" & Category == cat)$Frequency
      result <- t.test(dyad, spawn, paired = TRUE)
      
      # Extract df and t-value
      df <- result$parameter
      t_value <- result$statistic
      
      # Apply Benjamini-Hochberg correction
      p_value <- result$p.value
      p_adjusted <- p.adjust(p_value, method = "BH")
      
      results_non_territorial <- rbind(results_non_territorial, data.frame(Category = cat,
                                                                           t_value = t_value,
                                                                           df = df,
                                                                           p_value = p_value,
                                                                           p_adjusted = p_adjusted))
    }
    
    # Print results for Non_Territorial 
    cat("Results for Non_Territorial:\n")
    print(results_non_territorial)
  }
  
  #REPRODUCTIVE IN Dyad (dyad t vs dyad nt) 
  {# Filter data for Dyad assay
    dyad_data <- subset(reproductive, Assay == "Dyad")
    
    # Perform paired t-test for each category
    categories <- unique(dyad_data$Category)
    results_dyad <- data.frame(Category = character(), 
                               t_value = numeric(), 
                               df = numeric(),  # Add degrees of freedom column
                               p_value = numeric(),
                               p_adjusted = numeric(),  # Add adjusted p-value column
                               stringsAsFactors = FALSE)
    
    # Perform paired t-test for each category with Benjamini-Hochberg correction
    for (cat in categories) {
      territorial <- subset(dyad_data, Subject == "Territorial" & Category == cat)$Frequency
      non_territorial <- subset(dyad_data, Subject == "Non_Territorial" & Category == cat)$Frequency
      result <- t.test(territorial, non_territorial, paired = TRUE)
      
      # Extract df and t-value
      df <- result$parameter
      t_value <- result$statistic
      
      # Apply Benjamini-Hochberg correction
      p_value <- result$p.value
      p_adjusted <- p.adjust(p_value, method = "BH")
      
      results_dyad <- rbind(results_dyad, data.frame(Category = cat,
                                                     t_value = t_value,
                                                     df = df,
                                                     p_value = p_value,
                                                     p_adjusted = p_adjusted))
    }
    
    # Print results for Dyad assay
    cat("Results for Dyad assay:\n")
    print(results_dyad)
  }
  #REPRODUCTIVE IN spawn (spawn t vs spawn nt)
  {
    # Filter data for spawn assay
    spawn_data <- subset(reproductive, Assay == "Spawn")
    
    # Perform paired t-test for each category
    categories <- unique(spawn_data$Category)
    results_spawn <- data.frame(Category = character(), 
                                t_value = numeric(), 
                                df = numeric(),  # Add degrees of freedom column
                                p_value = numeric(),
                                p_adjusted = numeric(),  # Add adjusted p-value column
                                stringsAsFactors = FALSE)
    
    # Perform paired t-test for each category with Benjamini-Hochberg correction
    for (cat in categories) {
      territorial <- subset(spawn_data, Subject == "Territorial" & Category == cat)$Frequency
      non_territorial <- subset(spawn_data, Subject == "Non_Territorial" & Category == cat)$Frequency
      result <- t.test(territorial, non_territorial, paired = TRUE)
      
      # Extract df and t-value
      df <- result$parameter
      t_value <- result$statistic
      
      # Apply Benjamini-Hochberg correction
      p_value <- result$p.value
      p_adjusted <- p.adjust(p_value, method = "BH")
      
      results_spawn <- rbind(results_spawn, data.frame(Category = cat,
                                                       t_value = t_value,
                                                       df = df,
                                                       p_value = p_value,
                                                       p_adjusted = p_adjusted))
    }
    
    # Print results for spawn assay
    cat("Results for spawn assay:\n")
    print(results_spawn)
  }
  #REPRODUCTIVE IN TERRITORIALS (dyad t vs spawn t)
  {
    # Filter data for territorial subject
    Territorial_data <- subset(reproductive, Subject == "Territorial")
    
    # Perform paired t-test for each category
    categories <- unique(Territorial_data$Category)
    results_territorial <- data.frame(Category = character(), 
                                      t_value = numeric(), 
                                      df = numeric(),  # Add degrees of freedom column
                                      p_value = numeric(),
                                      p_adjusted = numeric(),  # Add adjusted p-value column
                                      stringsAsFactors = FALSE)
    
    # Perform paired t-test for each category with Benjamini-Hochberg correction
    for (cat in categories) {
      dyad <- subset(Territorial_data, Assay == "Dyad" & Category == cat)$Frequency
      spawn <- subset(Territorial_data, Assay == "Spawn" & Category == cat)$Frequency
      result <- t.test(dyad, spawn, paired = TRUE)
      
      # Extract df and t-value
      df <- result$parameter
      t_value <- result$statistic
      
      # Apply Benjamini-Hochberg correction
      p_value <- result$p.value
      p_adjusted <- p.adjust(p_value, method = "BH")
      
      results_territorial <- rbind(results_territorial, data.frame(Category = cat,
                                                                   t_value = t_value,
                                                                   df = df,
                                                                   p_value = p_value,
                                                                   p_adjusted = p_adjusted))
    }
    
    # Print results for Territorial 
    cat("Results for Territorial:\n")
    print(results_territorial)
  }
  #REPRODUCTIVE IN NON_TERRITORIALS (dyad t vs spawn t)
  {
    # Filter data for territorial subject
    Non_Territorial_data <- subset(reproductive, Subject == "Non_Territorial")
    
    # Perform paired t-test for each category
    categories <- unique(Non_Territorial_data$Category)
    results_non_territorial <- data.frame(Category = character(), 
                                          t_value = numeric(), 
                                          df = numeric(),  # Add degrees of freedom column
                                          p_value = numeric(),
                                          p_adjusted = numeric(),  # Add adjusted p-value column
                                          stringsAsFactors = FALSE)
    
    # Perform paired t-test for each category with Benjamini-Hochberg correction
    for (cat in categories) {
      dyad <- subset(Non_Territorial_data, Assay == "Dyad" & Category == cat)$Frequency
      spawn <- subset(Non_Territorial_data, Assay == "Spawn" & Category == cat)$Frequency
      result <- t.test(dyad, spawn, paired = TRUE)
      
      # Extract df and t-value
      df <- result$parameter
      t_value <- result$statistic
      
      # Apply Benjamini-Hochberg correction
      p_value <- result$p.value
      p_adjusted <- p.adjust(p_value, method = "BH")
      
      results_non_territorial <- rbind(results_non_territorial, data.frame(Category = cat,
                                                                           t_value = t_value,
                                                                           df = df,
                                                                           p_value = p_value,
                                                                           p_adjusted = p_adjusted))
    }
    
    # Print results for Non_Territorial 
    cat("Results for Non_Territorial:\n")
    print(results_non_territorial)
  }
}



#PLOTS

#Individual Behaviors
{
  Plot.1 <- ggplot(AllData, aes(x = Behavior, y = Frequency, fill = Subject)) +
    geom_boxplot(fatten = NULL, position = position_dodge(width = 0.75)) +  # Boxplot
    stat_summary(fun.y = mean, geom = "crossbar", position = position_dodge(width = 0.75), width = 0.5) +
    facet_wrap(~Assay, scales = "fixed") +
    labs(title = "All T/NT Behaviors Across Subjects: Total Frequency", x = "Behavior", y = "Total Frequency") +
    theme(axis.text.x = element_text(angle = 55, hjust = 1),
          text = element_text(size = 14)) +  # Increase font size for all labels
    scale_y_continuous(limits = c(0, 200))+
    geom_text(data = subset(AllData, Assay == "Dyad"), aes(label = "*"), x = 1, y = 190, vjust = -0.5, size = 6) +
    geom_text(data = subset(AllData, Assay == "Dyad"), aes(label = "*"), x = 4, y = 190, vjust = -0.5, size = 6) +
    geom_text(data = subset(AllData, Assay == "Dyad"), aes(label = "*"), x = 7, y = 190, vjust = -0.5, size = 6) +
    geom_text(data = subset(AllData, Assay == "Dyad"), aes(label = "*"), x = 8, y = 190, vjust = -0.5, size = 6)
  
  
  print(Plot.1)
  ggsave("All.png", plot = Plot.1, width = 8, height = 8, units = "in")
}

#Categorical Behaviors
{
  #Reformat AllData so it includes frequency count by Category/Assay/Subject/TankID
  summary_AllData <- AllData %>%
    group_by(Assay, Category) %>%
    summarise(Frequency = sum(Frequency))
  
  #Calculate mean and SEM
  summary_AllData <- AllData %>%
    group_by(Subject,Assay,Category) %>%
    summarise(mean_total = mean(Frequency),
              sem = sd(Frequency) / sqrt(n()))
  
  categorical.1 <- ggplot(AllData, aes(x = Category, y = Frequency, fill = Subject))+
    geom_boxplot(fatten = NULL, position = position_dodge(width = 0.75)) +  # Boxplot
    facet_wrap(~Assay, scales = "fixed") +
    stat_summary(fun.y = mean, geom = "crossbar", position = position_dodge(width = 0.75), width = 0.5) +
    labs(title = "T/NT Behavioral Categories Across Subjects: Total Frequency", x = "Behavioral Category", y = "Total Frequency") +
    theme(axis.text.x = element_text(angle = 55, hjust = 1),
          text = element_text(size = 14))+ # Increase font size for all labels
    scale_y_continuous(limits = c(0, 200))+
    geom_text(data = subset(AllData, Assay == "Dyad"), aes(label = "*"), x = 2, y = 190, vjust = -0.5, size = 7)+
    geom_text(data = subset(AllData, Assay == "Dyad"), aes(label = "*"), x = 3, y = 190, vjust = -0.5, size = 7)
  
  print(categorical.1)
  ggsave("All.cat.png", plot = categorical.1, width = 8, height = 8, units = "in")
}

#Latency to engage in behaviors
{
  # First, arrange the dataframe by Time to ensure observations are in chronological order
  T.NT.within.assays <- T.NT.within.assays %>%
    arrange(Time)
  
  # Then, group by TankID and Behavior, and filter for the first occurrence of each behavior for each TankID
  first_occurrence <- T.NT.within.assays %>%
    group_by(TankID, Behavior) %>%
    slice(1) %>%
    ungroup()
  
  # Convert Time column to numeric minutes
  first_occurrence$Time_numeric <- as.numeric(sub("^(\\d+):(\\d+\\.\\d+)$", "\\1", first_occurrence$Time)) +
    as.numeric(sub("^(\\d+):(\\d+\\.\\d+)$", "\\2", first_occurrence$Time)) / 60
  
  Subset.f.a <- subset(first_occurrence, Assay == "Spawn")
  
  # Calculate standard error of the mean (SEM) for each behavior
  sem_data <- Subset.f.a %>%
    group_by(Behavior, Subject, Assay) %>%
    summarise(mean_time = mean(Time_numeric),
              sem = sd(Time_numeric) / sqrt(n()))
  
  # Calculate the maximum time across all facets
  max_time <- max(Subset.f.a$Time_numeric)
  
  b.t <- ggplot(Subset.f.a, aes(x = Behavior, y = Time_numeric, fill = Subject)) +
    geom_boxplot(fatten = NULL, position = position_dodge(width = 0.75)) +  # Boxplot
    stat_summary(fun.y = mean, geom = "crossbar", position = position_dodge(width = 0.75), width = 0.5) +
    scale_y_continuous(breaks = seq(0, ceiling(max_time), by = 5)) +
    labs(title="Latency to Initial Engagement in Each Behavior", x = "Behavior", y = "Time (minutes)", fill = "Subject") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("Latency.png", plot = b.t, width = 8, height = 8, units = "in")
  
  # paired t-test
  {
    
    first_occurrence <- first_occurrence %>%
      filter(!grepl("Dyad", Assay)) 
    
    # Filter behaviors with two values
    behaviors_with_two_values <- first_occurrence %>%
      group_by(Behavior) %>%
      filter(n_distinct(Subject) == 2) %>%
      ungroup()
    
    # Define the mapping
    tank_id_mapping <- c("S1_T" = "1",
                         "S3_T" = "3",
                         "S5_T" = "5",
                         "S8_T" = "8",
                         "S1_NT" = "1",
                         "S3_NT" = "3",
                         "S5_NT" = "5",
                         "S8_NT" = "8")
    
    # Apply the mapping to the TankID column
    behaviors_with_two_values <- behaviors_with_two_values %>%
      mutate(TankID = tank_id_mapping[TankID])
    
    
    # Custom function to perform paired t-test
    paired_t_test <- function(x) {
      if (nrow(x) < 2) return(NA)  # Check if there are at least two observations
      if (length(unique(x$Subject)) != 2) return(NA)  # Check if there are exactly two unique subjects
      if (any(is.na(x$Time_numeric))) return(NA)  # Check for missing values
      
      t_test_result <- t.test(Time_numeric ~ Subject, data = x, paired = TRUE)
      return(t_test_result$p.value)
    }
    
    # Perform paired t-tests
    results <- behaviors_with_two_values %>%
      group_by(Behavior, TankID) %>%
      summarise(p_value = paired_t_test(.))
    
    # Print results
    print(results)
  }
}

#Decision making heatmap (average time between behavior transitions)
{
  setwd("/Users/anastasia/Desktop/Anastasia's logs")
  All <- read.csv("02.15.24.T.NT.no.male.all.csv")
  Spawn <- subset(All, Assay == "Spawn")
  
  
  S1_T <- subset(Spawn, TankID == "S1_T")
  S3_T <- subset(Spawn, TankID == "S3_T")
  S5_T <- subset(Spawn, TankID == "S5_T")
  S8_T <- subset(Spawn, TankID == "S8_T")
  S1_NT <- subset(Spawn, TankID == "S1_NT")
  S3_NT <- subset(Spawn, TankID == "S3_NT")
  S5_NT <- subset(Spawn, TankID == "S5_NT")
  S8_NT <- subset(Spawn, TankID == "S8_NT")
  
  
  #calculate the average transition times for Territorial
  {
    #S1_T
    {
      # Function to convert time strings to numeric representation in seconds
      convert_time <- function(time_str) {
        parts <- unlist(strsplit(time_str, ":"))
        minutes <- as.numeric(parts[1])
        seconds <- as.numeric(parts[2])
        total_seconds <- minutes * 60 + seconds
        return(total_seconds)
      }
      
      # Convert the Time column from character to numeric format representing minutes and seconds
      S1_T$Time_Numeric <- sapply(S1_T$Time, convert_time)
      
      # Calculate the time difference between consecutive rows in seconds
      S1_T$Time_Diff <- c(NA, diff(S1_T$Time_Numeric))
      
      # Create an empty dataframe to store transitions_S1_T and time durations
      transitions_S1_T <- data.frame(
        From_Behavior = character(),
        To_Behavior = character(),
        Time_Duration = numeric()
      )
      
      # Iterate through each row of S1_T to calculate transitions_S1_T and time durations
      for (i in 1:(nrow(S1_T) - 1)) {
        from_behavior <- S1_T$Behavior[i]
        to_behavior <- S1_T$Behavior[i + 1]
        time_duration <- S1_T$Time_Diff[i + 1]
        
        # Append the transition to the transitions_S1_T dataframe
        transitions_S1_T <- rbind(transitions_S1_T, data.frame(From_Behavior = from_behavior,
                                                               To_Behavior = to_behavior,
                                                               Time_Duration = time_duration))
      }
      
    }
    #S3_T
    {
      # Function to convert time strings to numeric representation in seconds
      convert_time <- function(time_str) {
        parts <- unlist(strsplit(time_str, ":"))
        minutes <- as.numeric(parts[1])
        seconds <- as.numeric(parts[2])
        total_seconds <- minutes * 60 + seconds
        return(total_seconds)
      }
      
      # Convert the Time column from character to numeric format representing minutes and seconds
      S3_T$Time_Numeric <- sapply(S3_T$Time, convert_time)
      
      # Calculate the time difference between consecutive rows in seconds
      S3_T$Time_Diff <- c(NA, diff(S3_T$Time_Numeric))
      
      # Create an empty dataframe to store transitions_S3_T and time durations
      transitions_S3_T <- data.frame(
        From_Behavior = character(),
        To_Behavior = character(),
        Time_Duration = numeric()
      )
      
      # Iterate through each row of S3_T to calculate transitions_S3_T and time durations
      for (i in 1:(nrow(S3_T) - 1)) {
        from_behavior <- S3_T$Behavior[i]
        to_behavior <- S3_T$Behavior[i + 1]
        time_duration <- S3_T$Time_Diff[i + 1]
        
        # Append the transition to the transitions_S3_T dataframe
        transitions_S3_T <- rbind(transitions_S3_T, data.frame(From_Behavior = from_behavior,
                                                               To_Behavior = to_behavior,
                                                               Time_Duration = time_duration))
      }
      
    }
    #S5_T
    {  
      # Function to convert time strings to numeric representation in seconds
      convert_time <- function(time_str) {
        parts <- unlist(strsplit(time_str, ":"))
        minutes <- as.numeric(parts[1])
        seconds <- as.numeric(parts[2])
        total_seconds <- minutes * 60 + seconds
        return(total_seconds)
      }
      
      # Convert the Time column from character to numeric format representing minutes and seconds
      S5_T$Time_Numeric <- sapply(S5_T$Time, convert_time)
      
      # Calculate the time difference between consecutive rows in seconds
      S5_T$Time_Diff <- c(NA, diff(S5_T$Time_Numeric))
      
      # Create an empty dataframe to store transitions_S5_T and time durations
      transitions_S5_T <- data.frame(
        From_Behavior = character(),
        To_Behavior = character(),
        Time_Duration = numeric()
      )
      
      # Iterate through each row of S5_T to calculate transitions_S5_T and time durations
      for (i in 1:(nrow(S5_T) - 1)) {
        from_behavior <- S5_T$Behavior[i]
        to_behavior <- S5_T$Behavior[i + 1]
        time_duration <- S5_T$Time_Diff[i + 1]
        
        # Append the transition to the transitions_S5_T dataframe
        transitions_S5_T <- rbind(transitions_S5_T, data.frame(From_Behavior = from_behavior,
                                                               To_Behavior = to_behavior,
                                                               Time_Duration = time_duration))
      }
    }  
    #S8_T
    {
      # Function to convert time strings to numeric representation in seconds
      convert_time <- function(time_str) {
        parts <- unlist(strsplit(time_str, ":"))
        minutes <- as.numeric(parts[1])
        seconds <- as.numeric(parts[2])
        total_seconds <- minutes * 60 + seconds
        return(total_seconds)
      }
      
      # Convert the Time column from character to numeric format representing minutes and seconds
      S8_T$Time_Numeric <- sapply(S8_T$Time, convert_time)
      
      # Calculate the time difference between consecutive rows in seconds
      S8_T$Time_Diff <- c(NA, diff(S8_T$Time_Numeric))
      
      # Create an empty dataframe to store transitions_S8_T and time durations
      transitions_S8_T <- data.frame(
        From_Behavior = character(),
        To_Behavior = character(),
        Time_Duration = numeric()
      )
      
      # Iterate through each row of S8_T to calculate transitions_S8_T and time durations
      for (i in 1:(nrow(S8_T) - 1)) {
        from_behavior <- S8_T$Behavior[i]
        to_behavior <- S8_T$Behavior[i + 1]
        time_duration <- S8_T$Time_Diff[i + 1]
        
        # Append the transition to the transitions_S8_T dataframe
        transitions_S8_T <- rbind(transitions_S8_T, data.frame(From_Behavior = from_behavior,
                                                               To_Behavior = to_behavior,
                                                               Time_Duration = time_duration))
      }
      
    }
    
    T_transitions <- rbind(transitions_S1_T,transitions_S3_T,transitions_S5_T,transitions_S8_T)
    
    # Aggregate the time durations by transition
    T_avg_time <- aggregate(Time_Duration ~ From_Behavior + To_Behavior, data = T_transitions, FUN = mean)
  }
  
  #calculate the average transition times for Non_Territorial
  {
    
    #S1_NT
    {
      # Function to convert time strings to numeric representation in seconds
      convert_time <- function(time_str) {
        parts <- unlist(strsplit(time_str, ":"))
        minutes <- as.numeric(parts[1])
        seconds <- as.numeric(parts[2])
        total_seconds <- minutes * 60 + seconds
        return(total_seconds)
      }
      
      # Convert the Time column from character to numeric format representing minutes and seconds
      S1_NT$Time_Numeric <- sapply(S1_NT$Time, convert_time)
      
      # Calculate the time difference between consecutive rows in seconds
      S1_NT$Time_Diff <- c(NA, diff(S1_NT$Time_Numeric))
      
      # Create an empty dataframe to store transitions_S1_NT and time durations
      transitions_S1_NT <- data.frame(
        From_Behavior = character(),
        To_Behavior = character(),
        Time_Duration = numeric()
      )
      
      # Iterate through each row of S1_NT to calculate transitions_S1_NT and time durations
      for (i in 1:(nrow(S1_NT) - 1)) {
        from_behavior <- S1_NT$Behavior[i]
        to_behavior <- S1_NT$Behavior[i + 1]
        time_duration <- S1_NT$Time_Diff[i + 1]
        
        # Append the transition to the transitions_S1_NT dataframe
        transitions_S1_NT <- rbind(transitions_S1_NT, data.frame(From_Behavior = from_behavior,
                                                                 To_Behavior = to_behavior,
                                                                 Time_Duration = time_duration))
      }
      
    }
    #S3_NT
    {
      # Function to convert time strings to numeric representation in seconds
      convert_time <- function(time_str) {
        parts <- unlist(strsplit(time_str, ":"))
        minutes <- as.numeric(parts[1])
        seconds <- as.numeric(parts[2])
        total_seconds <- minutes * 60 + seconds
        return(total_seconds)
      }
      
      # Convert the Time column from character to numeric format representing minutes and seconds
      S3_NT$Time_Numeric <- sapply(S3_NT$Time, convert_time)
      
      # Calculate the time difference between consecutive rows in seconds
      S3_NT$Time_Diff <- c(NA, diff(S3_NT$Time_Numeric))
      
      # Create an empty dataframe to store transitions_S3_NT and time durations
      transitions_S3_NT <- data.frame(
        From_Behavior = character(),
        To_Behavior = character(),
        Time_Duration = numeric()
      )
      
      # Iterate through each row of S3_NT to calculate transitions_S3_NT and time durations
      for (i in 1:(nrow(S3_NT) - 1)) {
        from_behavior <- S3_NT$Behavior[i]
        to_behavior <- S3_NT$Behavior[i + 1]
        time_duration <- S3_NT$Time_Diff[i + 1]
        
        # Append the transition to the transitions_S3_NT dataframe
        transitions_S3_NT <- rbind(transitions_S3_NT, data.frame(From_Behavior = from_behavior,
                                                                 To_Behavior = to_behavior,
                                                                 Time_Duration = time_duration))
      }
      
    }
    #S5_NT
    {
      # Function to convert time strings to numeric representation in seconds
      convert_time <- function(time_str) {
        parts <- unlist(strsplit(time_str, ":"))
        minutes <- as.numeric(parts[1])
        seconds <- as.numeric(parts[2])
        total_seconds <- minutes * 60 + seconds
        return(total_seconds)
      }
      
      # Convert the Time column from character to numeric format representing minutes and seconds
      S5_NT$Time_Numeric <- sapply(S5_NT$Time, convert_time)
      
      # Calculate the time difference between consecutive rows in seconds
      S5_NT$Time_Diff <- c(NA, diff(S5_NT$Time_Numeric))
      
      # Create an empty dataframe to store transitions_S5_NT and time durations
      transitions_S5_NT <- data.frame(
        From_Behavior = character(),
        To_Behavior = character(),
        Time_Duration = numeric()
      )
      
      # Iterate through each row of S5_NT to calculate transitions_S5_NT and time durations
      for (i in 1:(nrow(S5_NT) - 1)) {
        from_behavior <- S5_NT$Behavior[i]
        to_behavior <- S5_NT$Behavior[i + 1]
        time_duration <- S5_NT$Time_Diff[i + 1]
        
        # Append the transition to the transitions_S5_NT dataframe
        transitions_S5_NT <- rbind(transitions_S5_NT, data.frame(From_Behavior = from_behavior,
                                                                 To_Behavior = to_behavior,
                                                                 Time_Duration = time_duration))
      }
      
    }
    #S8_NT
    {
      # Function to convert time strings to numeric representation in seconds
      convert_time <- function(time_str) {
        parts <- unlist(strsplit(time_str, ":"))
        minutes <- as.numeric(parts[1])
        seconds <- as.numeric(parts[2])
        total_seconds <- minutes * 60 + seconds
        return(total_seconds)
      }
      
      # Convert the Time column from character to numeric format representing minutes and seconds
      S8_NT$Time_Numeric <- sapply(S8_NT$Time, convert_time)
      
      # Calculate the time difference between consecutive rows in seconds
      S8_NT$Time_Diff <- c(NA, diff(S8_NT$Time_Numeric))
      
      # Create an empty dataframe to store transitions_S8_NT and time durations
      transitions_S8_NT <- data.frame(
        From_Behavior = character(),
        To_Behavior = character(),
        Time_Duration = numeric()
      )
      
      # Iterate through each row of S8_NT to calculate transitions_S8_NT and time durations
      for (i in 1:(nrow(S8_NT) - 1)) {
        from_behavior <- S8_NT$Behavior[i]
        to_behavior <- S8_NT$Behavior[i + 1]
        time_duration <- S8_NT$Time_Diff[i + 1]
        
        # Append the transition to the transitions_S8_NT dataframe
        transitions_S8_NT <- rbind(transitions_S8_NT, data.frame(From_Behavior = from_behavior,
                                                                 To_Behavior = to_behavior,
                                                                 Time_Duration = time_duration))
      }
      
    }
    
    NT_transitions <- rbind(transitions_S1_NT,transitions_S3_NT,transitions_S5_NT,transitions_S8_NT)
    
    # Aggregate the time durations by transition
    NT_avg_time <- aggregate(Time_Duration ~ From_Behavior + To_Behavior, data = NT_transitions, FUN = mean)
  }
  
  #heatmap
  {
    T_avg_time$Subject <- "Territorial"
    NT_avg_time$Subject <- "Non_territorial"
    
    All_avg <- rbind(T_avg_time,NT_avg_time)
    
    # Create the heatmap with facets for each subject, tilted x-axis labels, customized gradient, and title
    p <- ggplot(All_avg, aes(x = From_Behavior, y = To_Behavior, fill = Time_Duration)) +
      geom_tile() +
      scale_fill_gradient(low = "#E6F5FF", high = "#004D99") + # Customizing gradient colors
      labs(x = "From Behavior", y = "To Behavior", fill = "Time (seconds)") +
      ggtitle("Decision Making Latency in Spawn Assay") +  # Adding title
      theme_minimal() +
      facet_wrap(~ Subject) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Save the plot as a PNG file
    ggsave("decision_making_latency_plot.png", p, width = 10, height = 6, bg = "white")
  }
  
  #point plot
  {

    mean_transitions <- aggregate(Time_Duration ~ From_Behavior + To_Behavior + Subject, 
                                  data = T_NT, 
                                  FUN = function(x) mean(x))
  
    mean_sem <- merge(mean_transitions, sem_transitions, by = c("From_Behavior", "To_Behavior", "Subject"))
      
    # Filter data for Territorial and Non-territorial subjects
    mean_sem_subset <- subset(mean_sem, Subject %in% c("Territorial", "Non_territorial"))
    
    # Reorder the levels of the interaction variable alphabetically
    mean_sem_subset$interaction_order <- with(mean_sem_subset, reorder(interaction(From_Behavior, To_Behavior), as.character(interaction(From_Behavior, To_Behavior))))
    
    # Point plot with error bars
    point_plot <- ggplot(mean_sem_subset, aes(x = mean, y = interaction_order, color = Subject)) +
      geom_point(position = position_dodge(width = 0.5)) +  # Add points with dodge position
      geom_errorbar(aes(xmin = mean - sem, xmax = mean + sem), width = 0.2, position = position_dodge(width = 0.5)) +  # Add error bars
      labs(x = "Mean Time (seconds)", y = "Behavioral Transitions", color = "Subject") +
      ggtitle("Mean Decision Making Latency with SEM") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8, hjust = 0)) +  # Adjust size and alignment of y-axis labels
      scale_color_manual(values = c("Territorial" = "blue", "Non_territorial" = "red"))  # Custom color for subjects
    
    # Print the point plot
    print(point_plot)
    ggsave("point_plot.png", plot = point_plot, width = 8, height = 6, units = "in", dpi = 300)
  }
}

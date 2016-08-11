# Read and clean absenteeism data
read_and_clean_absenteeism_data <- function(file = 'data/srubbed_data_for_joe.csv'){
  
  # Package requirements
  require(readr)
  require(plyr)
  require(dplyr)
  
  # Read file
  df <- read_csv(file)
  
  # Clean up date of birth
  df$dob <- substr(df$dob, 
                   start = 1,
                   stop = 10)
  
  # Make date objects
  df$absenceDate <- as.Date(df$absenceDate)
  df$dob <- as.Date(df$dob)
  
  # Clean up race ethnicity
  df$race <-
    ifelse(df$raceEthnicity == 'A', 'Asian',
           ifelse(df$raceEthnicity == 'B', 'Black',
                  ifelse(df$raceEthnicity == 'H', 'Hispanic',
                         ifelse(df$raceEthnicity == 'I', 'Indigenous',
                                ifelse(df$raceEthnicity == 'M', 'Mixed',
                                       ifelse(df$raceEthnicity == 'W', 'White', NA))))))
  
  # Clean up lunch status
  #1  Applied Not Eligible
  #0  Did not apply
  #2	Eligible for free lunch
  #6	Eligible for free lunch/Direct Certified/Decline
  #9	Eligible for free lunch/Direct Certified
  #3	Eligible Reduced
  #4	Enrolled USDA approved Prov 2 school
  #Z	Unknown
  df$lunch <-
    ifelse(df$lunchStatus %in% c(0, 1), 'Not free/reduced',
           ifelse(df$lunchStatus %in% c(2, 3, 9), 'Free/reduced', NA))
  
  # Remove useless columns and clean up names
  df <- df %>% 
    dplyr::select(personID,
                  lunch,
                  race,
                  schoolName,
                  grade,
                  dob,
                  absenceDate) 
  df <- df %>%
    dplyr::rename(id = personID,
           school = schoolName,
           date = absenceDate)
  
  return(df)
}

# Read and clean school immunization data
read_and_clean_immunization_data <- function(directory = 'data/immunization_data/'){
  
  # Packages
  require(readr)
  require(dplyr)
  
  # Snapshot the current working directory
  cwd <- getwd()
  # And set to new working directory
  setwd(directory)
  
  # Read in
  files <- dir()
  results_list <- list()
  counter <- 0
  for (i in 1:length(files)){
    this_file <- suppressWarnings(read_csv(files[i]))
    this_file <- data.frame(this_file)
    # Snapshot the column names if the first
    if(i == 1){
      this_file <-
        this_file %>%
        dplyr::select(-`NA.`)
      assign('column_names',
             names(this_file),
             envir = .GlobalEnv)
    } else {
      for (j in 1:length(column_names)){
        this_column <- column_names[j]
        if(!this_column %in% names(this_file)){
          this_file[,this_column] <- NA
        }
      }
      this_file <- this_file[,column_names]
    }
    # Remove any NA row
    this_file <- this_file %>%
      filter(!is.na(this_file$student_id))
    # Make all characters
    for(j in 1:length(column_names)){
      this_file[,j] <-
        as.character(this_file[,j])
    }
    results_list[[i]] <- this_file
    message(paste0('Just finished reading in data for: ', this_file$school_name[1],
                   '\n',
                   'has ', nrow(this_file), ' rows.\n\n'))
    counter <- counter + nrow(this_file)
  }
  rm(column_names, envir = .GlobalEnv)
  df <- do.call('rbind', results_list)
  
  # Set back to original working directory
  setwd(cwd)
  
  # Clean up the dataset
  df$consent_form_return <-
    ifelse(df$consent_form_return %in% 
             c('y', 'y\`', 'Y', 'yes'), TRUE, FALSE)
  df$consent_form_yes <- 
    ifelse(df$consent_form_yn %in% c('n', 'N', 'no', 'No'), FALSE,
           ifelse(df$consent_form_yn %in% c('y', 'Y', 'yes'), TRUE,
                  NA))
  df$vaccine_date <- as.Date(df$vaccine_date)
  df$vfc_priv <-
    ifelse(df$vfc_priv %in% c('peiv', 'pri', 'PRI', 'pric', 'priv',
                              'Priv', 'prtiv', 'vpri'), 'Private',
           ifelse(df$vfc_priv %in% c('fc', 'vc', 'vf', 'vfc', 'VFC'), 'VFC',
                  NA))
  df$refusal <-
    ifelse(df$refusal %in% c('1', 'ref', 'REF', 'y', 'Y', 'yes'), TRUE, FALSE)
  df$absence <-
    ifelse(is.na(df$absence), FALSE, TRUE)
  df$vaccine <-
    ifelse(df$vaccine %in% c('1', '2'), TRUE, 
           ifelse(df$vaccine %in% c('0'), FALSE,
                  NA))
  
  # Reduce columns
  df <- 
    df %>%
    dplyr::select(school_name,
                  grade,
                  student_id,
                  consent_form_return,
                  consent_form_yes,
                  vaccine,
                  vaccine_date)
  
  # Make data.frame 
  df <- data.frame(df)
  
  # Remove those for whom student id appears to be a birthday
  df <- df[!grepl('/', df$student_id),]
  
  # Remove duplicates
  # arrange so that yesses come first
  # (this is justified since "no" is default, and any yes most likely indicates
  # a true yes)
  df <- df %>%
    arrange(desc(consent_form_return), 
            desc(consent_form_yes),
            desc(vaccine))
  df <- df[!duplicated(df$student_id),]
  return(df)
}

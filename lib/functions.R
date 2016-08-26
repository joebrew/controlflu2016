# Read and clean absenteeism data
read_and_clean_absenteeism_data <- function(file = 'data/scrubbed_data_for_joe_correct_ids.csv'){
  
  # Package requirements
  require(readr)
  require(plyr)
  require(dplyr)
  
  
  # Read file
  # df <- read_csv(file)
  df <- read.csv(file)
  
  # Clean up grade
  df$grade[df$grade %in% c('30', '31')] <- '3'
  
  # Clean up date of birth
  df$dob <- substr(df$dob, 
                   start = 1,
                   stop = 10)
  
  # Make date objects
  df$absenceDate <- as.Date(df$absenceDate, format = '%m/%d/%Y')
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
    dplyr::select(studentNumber,
                  lunch,
                  race,
                  schoolName,
                  grade,
                  dob,
                  absenceDate) 
  df <- df %>%
    dplyr::rename(id = studentNumber,
           school = schoolName,
           date = absenceDate)
  
  # Make id character
  df$id <- as.character(df$id)
  
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
    ifelse(df$vaccine %in% c('0', '?'), FALSE, 
           ifelse(is.na(df$vaccine), TRUE, TRUE))
  
  # Reduce columns
  df <- 
    df %>%
    dplyr::select(school_name,
                  grade,
                  student_id,
                  consent_form_return,
                  consent_form_yes,
                  vaccine,
                  vaccine_date) %>%
    dplyr::rename(id = student_id)
  
  # Make data.frame 
  df <- data.frame(df)
  
  # Remove those for whom student id appears to be a birthday
  df <- df[!grepl('/', df$id),]
  df <- df[!grepl('-', df$id),]
  
  # Make id character
  df$id <- as.character(df$id)
  
  # Remove duplicates
  # arrange so that yesses come first
  # (this is justified since "no" is default, and any yes most likely indicates
  # a true yes)
  df <- df %>%
    arrange(desc(consent_form_return), 
            desc(consent_form_yes),
            desc(vaccine))
  df <- df[!duplicated(df$id),]
  
  # Fix date screw up
  df$vaccine_date[df$vaccine_date <= '2015-01-01'] <- '2015-10-01'
  return(df)
}

# Create panel data
create_panel_data <- function(ab,
                              students){
  # Manually create date range
  date_range <- seq(as.Date('2015-08-25'), as.Date('2016-05-29'), by = 1)
  
  # Make a dataframe
  df <- data.frame(date = date_range)
  df$day <- weekdays(df$date)
  
  # Remove weekends
  df <- df %>% filter(!day %in% c('Saturday', 'Sunday'))
  
  # Remove christmas break, etc. (dates for which there are 0 absences)
  df <- df %>%
    filter(! date %in% df$date[!df$date %in% sort(unique(ab$date))])
  
  # Create an expanded grid of all dates with all students
  df <- expand.grid(date = df$date,
                    id = students$id)
  
  # Join the expanded grid to absences
  df <- left_join(df,
                  ab %>%
                    mutate(absent = TRUE) %>%
                    dplyr::select(id, date, absent),
                  by = c('date', 'id'))
  
  # If not absent, assume present
  df$absent[is.na(df$absent)] <- FALSE
  
  # Return
  return(df)
}

# Make pretty table
make_pretty <- function(x){
  require(Hmisc)
  require(DT)
  the_names <- capitalize(gsub('_', ' ', toupper(names(x))))
  x <- data.frame(x)
  for (j in 1:ncol(x)){
    if(class(x[,j]) %in% c('numeric', 'integer')){
      x[,j] <- round(x[,j], digits = 2)
    } else {
      x[,j] <- toupper(x[,j])
    }
  }
  for (j in 1:ncol(x)){
    if(grepl('rate', names(x)[j])){
      x[,j] <- paste0(x[,j], '%')
    }
  }
  names(x) <- the_names
  DT::datatable(x)
}
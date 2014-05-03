source("rankhospital.R")

rankall <- function(outcome, num = "best",
                    data_file_name = "outcome-of-care-measures.csv",
                    col_prefix     = "Hospital.30.Day.Death..Mortality..Rates.from.",
                    hospital_col_index = 2,
                    state_col_index = 7) {
  
  ## Read outcome data
  data <- read.csv(data_file_name, colClasses="character")
  
  outcome <- stop_if_invalid_outcome(outcome)
  
  ## For each state, find the hospital of the given rank
  
  # create new data frame that has only the hospital name column and the column
  # for the requested outcome
  outcome_index <- get_col_index(data, col_prefix, outcome)                  # column index of wanted outcome    
  data <- data[ , c(hospital_col_index, state_col_index, outcome_index) ]    # keep only hospital name, state, and requested outcome
  data[,3] <- suppressWarnings( as.numeric(data[,3]) )                       # convert to numeric
  data <- data[ !is.na( data[,3]), ]                                         # drop NAs
  
  hospital_name <- names(data)[1]
  state_name    <- names(data)[2]   
  outcome_name  <- names(data)[3]
  data <- data[ order(data[state_name], data[outcome_name], data[hospital_name]), ]
   
  df_list <- split(data, as.factor(data$State))
  if (num != "worst") rank <- get_rank(num, data)
  result <- sapply(df_list, function(df){ 
    if (num == "worst")
      index <- nrow(df)
    else
      index <- rank
    c(df[index, 1], df[1,2]) 
  })
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  result <- as.data.frame(t(result))
  names(result) <- c("hospital", "state")
  result
}
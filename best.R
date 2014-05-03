# global
valid_outcomes <- c("heart attack", "heart failure", "pneumonia")

best <- function(state, outcome, 
                 data_file_name = "outcome-of-care-measures.csv",
                 col_prefix     = "Hospital.30.Day.Death..Mortality..Rates.from.",
                 hospital_col_index = 2) {  
  
  ## Read outcome data
  data <- read.csv(data_file_name, colClasses="character")
  
  state   <- stop_if_invalid_state(state, data)
  outcome <- stop_if_invalid_outcome(outcome)
  
  ## Return hospital name in that state with lowest 30-day death rate
  outcome_index <- get_col_index(data, col_prefix, outcome)                         # column index of wanted outcome
  state_data <- data[ data$State==state, c(hospital_col_index, outcome_index) ]     # filter for wanted state
  best_hospital_index <- suppressWarnings( which.min( as.numeric(state_data[,2])) ) # get index of minimum
  state_data[best_hospital_index, 1]                                                # and return the hospital name
}

stop_if_invalid_state <- function(state, data) {
  if (toupper(state) %in% unique(data$State)) {
    state
  }
  else {
    stop("invalid state")
  }
}

stop_if_invalid_outcome <- function(outcome) {
  if (tolower(outcome) %in% valid_outcomes) {
    s <- strsplit(outcome, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=".")        
  }
  else {
    stop("invalid outcome")
  }
}

get_col_index <- function(data, col_prefix, outcome) {
  colname <- paste(col_prefix, outcome, sep="")
  # create logical vector containing TRUE at position of wanted column
  l <- colnames(data) == colname  
  # return index of matching position
  match(TRUE, l, nomatch=0)
}

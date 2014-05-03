source("rankhospital.R")

best <- function(state, outcome, 
                 data_file_name = "outcome-of-care-measures.csv",
                 col_prefix     = "Hospital.30.Day.Death..Mortality..Rates.from.",
                 hospital_col_index = 2) {  

  rankhospital(state, outcome, "best", data_file_name, col_prefix, hospital_col_index)
  
}
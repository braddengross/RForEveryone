rankhospital <- function(state, outcome, num = "best") {
  #Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  ## Check that state and outcome are valid
  legit_outcomes <- c("heart attack","heart failure", "pneumonia")
  legit_states <- unique(outcome_data$State)
  if(!is.element(outcome, legit_outcomes)){ 
    stop("invalid outcome")
  }
  if(!is.element(state, legit_states)){
    stop("invalid state")
  }
  
  if(outcome == "heart attack"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if(outcome == "heart failure"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"         
  } else if(outcome == "pneumonia"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  ## Return hospital name in that state with lowest 30-day death
  state_hospitals <- outcome_data[outcome_data$State == state,]
  state_hospitals_outcomes <- state_hospitals[,c("Hospital.Name", outcome)]
  state_hospitals_outcomes_without_na <- state_hospitals_outcomes[complete.cases(state_hospitals_outcomes), ]
  
  if(num == "best"){
    num <- 1
  } else if(num == "worst"){
    num <- nrow(state_hospitals_outcomes_without_na)
  } else if(num > nrow(state_hospitals_outcomes_without_na)){
    NA
    return 
  }
  
  state_hospitals_outcomes_without_na[, 2] <- as.numeric(state_hospitals_outcomes_without_na[,2])
  ordered <- state_hospitals_outcomes_without_na[order(state_hospitals_outcomes_without_na[,2], 
                                                       state_hospitals_outcomes_without_na[,1]), ]
  best_hospital <- ordered[num, ]$Hospital.Name
  print(best_hospital)
}
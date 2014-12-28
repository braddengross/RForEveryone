  best <- function(state, outcome) {
    #Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
    state_hospitals_outcomes[, 2] <- as.numeric(state_hospitals_outcomes[,2])
    best_hospital <- state_hospitals_outcomes[which(state_hospitals_outcomes[,2]==min(state_hospitals_outcomes[,2], na.rm = TRUE)), 1]
    if(length(best_hospital) > 1){
      best_hospital <- sort(best_hospital)
      best_hospital <- best_hospital[1]
    }
    print(best_hospital)
  }
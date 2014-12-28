rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  
  ## Check that state and outcome are valid
  legit_outcomes <- c("heart attack","heart failure", "pneumonia")
  if(!is.element(outcome, legit_outcomes)){ 
    stop("invalid outcome")
  }
  
  if(outcome == "heart attack"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if(outcome == "heart failure"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"         
  } else if(outcome == "pneumonia"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
    
  legit_states <- sort(unique(outcome_data$State))
  
  df <- data.frame(matrix(NA, nrow = length(legit_states), ncol = 2))
  colnames(df) <- c("hospital", "state")
  
  for(n in 1:length(legit_states)){
    state <- legit_states[n]
    state_hospitals <- outcome_data[outcome_data$State == state,]
    state_hospitals_outcomes <- state_hospitals[,c("Hospital.Name", outcome)]
    state_hospitals_outcomes_without_na <- state_hospitals_outcomes[complete.cases(state_hospitals_outcomes), ]
    state_num <- NA
    if(num == "best"){
      state_num <- 1
    } else if(num == "worst"){
      state_num <- nrow(state_hospitals_outcomes_without_na)
    } else if(num > nrow(state_hospitals_outcomes_without_na)){
      state_num <- NA
    } else {
      state_num <- num
    }
    if(!is.na(state_num)){
      state_hospitals_outcomes_without_na[, 2] <- as.numeric(state_hospitals_outcomes_without_na[,2])
      ordered <- state_hospitals_outcomes_without_na[order(state_hospitals_outcomes_without_na[,2], 
                                                           state_hospitals_outcomes_without_na[,1]), ]
      best_hospital <- ordered[state_num, ]$Hospital.Name
    } else {
      best_hospital <- NA
    }
    df[n, 1] <- best_hospital
    df[n, 2] <- state
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  df
}
best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  outcomes = c("heart attack", "heart failure", "pneumonia")
  if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
  
  states <- data[, 7]
  states <- unique(states)
  if( state %in% states == FALSE ) stop("invalid state")
  
 
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  if(outcome=="heart attack"){
    data <- data[data[,7]==state & data[ ,11] != 'Not Available', ]
    datavalid <- data[, 11] 
 
  } else if (outcome=="heart failure"){
    data <- data[data[,7]==state & data[ ,17] != 'Not Available', ]
    datavalid <- data[, 17]
  } else if (outcome=="pneumonia"){
    data <- data[data[,7]==state & data[ ,23] != 'Not Available', ]
    datavalid <- data[, 23]
  } 
   
  lignehospital <- which.min(datavalid)
  
  
  print(data[lignehospital,2] ) 
  
  
}
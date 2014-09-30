rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  outcomes = c("heart attack", "heart failure", "pneumonia")
  if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
  
  if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
  
  if(outcome=="heart attack"){
   numoutcome <- 11;
  } else if (outcome=="heart failure"){
    numoutcome <- 17;
  } else if (outcome=="pneumonia"){
    numoutcome <- 23;
  } 
  
  
  data <- data[data[ ,numoutcome] != 'Not Available', ]
  
  data[,numoutcome] <- as.data.frame(sapply(data[,numoutcome], as.numeric))
  
  data <- data[order(data[,2], decreasing = FALSE), ]
  data <- data[order(data[,numoutcome], decreasing = FALSE), ]
  
  getHospByRank <- function(databyrank, s, n) {
    databyrank <- databyrank[databyrank[,7]==s, ]
    datavalid <- databyrank[, numoutcome] 
    
    if( n == "best" ) {
      lignehospital <- which.min(datavalid)
    } else if( n == "worst" ) {
      lignehospital <- which.max(datavalid)
    } else {
      lignehospital <- n
      
    }
    databyrank[lignehospital,2] 
    
  }
   
  states <- data[, 7]
  states <- unique(states)
  newdata <- data.frame("hospital"=character(), "state"=character())
  for(st in states) {
    hosp <- getHospByRank(data, st, num)
    newdata <- rbind(newdata, data.frame(hospital=hosp, state=st))
  } 
  
  newdata <- newdata[order(newdata['state'], decreasing = FALSE), ]
  newdata
  
  




}
 
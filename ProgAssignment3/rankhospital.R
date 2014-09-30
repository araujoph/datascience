rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  
 
     data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    outcomes = c("heart attack", "heart failure", "pneumonia")
    if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
    
    states <- data[, 7]
    states <- unique(states)
    if( state %in% states == FALSE ) stop("invalid state")
    
    
    if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    if(outcome=="heart attack"){
      data <- data[data[,7]==state & data[ ,11] != 'Not Available', ]
      
      data[,11] <- as.data.frame(sapply(data[,11], as.numeric))
      
      data <- data[order(data[,2], decreasing = FALSE), ]
      data <- data[order(data[,11], decreasing = FALSE), ]
      
      datavalid <- data[, 11]
      
    } else if (outcome=="heart failure"){
      data <- data[data[,7]==state & data[ ,17] != 'Not Available', ]
      
      data[,17] <- as.data.frame(sapply(data[,17], as.numeric))
      
      data <- data[order(data[,2], decreasing = FALSE), ]
      data <- data[order(data[,17], decreasing = FALSE), ]
    
      datavalid <- data[, 17]
    } else if (outcome=="pneumonia"){
      data <- data[data[,7]==state & data[ ,23] != 'Not Available', ]
         
      data[,23] <- as.data.frame(sapply(data[,23], as.numeric))
      
      data <- data[order(data[,2], decreasing = FALSE), ]
      data <- data[order(data[,23], decreasing = FALSE), ]
      
      datavalid <- data[, 23]
    } 
 
    if( num == "best" ) {
      lignehospital <- which.min(datavalid)
    } else if( num == "worst" ) {
      lignehospital <- which.max(datavalid)
    } else {
      lignehospital <- num
       
    }
    
    
    print(data[lignehospital,2] ) 
    
    
}
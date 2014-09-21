complete <- function(directory, id = 1:332) {
 
  result <- matrix( , nrow=length(id), ncol=2)
  colnames(result) <- c("id", "nobs")
  rownames(result) <- c(1:length(id))
  
  result <- data.frame(result)
  
  for (i in 1:length(id)){ 
    if(id[i]<10){ 
      filename <- paste("00", id[i], sep="")
    }else if(id[i]>=10 && id[i]<=99){ 
      filename <- paste("0", id[i], sep="")  
    }else{ 
      filename <- id[i]
    }
    
    datafile <- paste(filename, "csv", sep=".")
     
    ##print(datafile)
    
    pathfile <- paste(directory, datafile, sep="/")
     
    data <- read.csv(file=pathfile )
    
    numbcases<-sum(complete.cases(data))
    
    result[i,1] <- filename
    result[i,2] <- numbcases
    
  }
  result 
}
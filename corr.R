corr  <- function(directory, threshold = 0) {
 
  
  dataframe <- complete(directory)
  dataframe <- subset(dataframe, nobs> threshold)
   
  corrscomplete<-numeric(0)
  
  for (iddata in dataframe$id) {
    
    datafile <- paste(iddata, "csv", sep=".")
    
    ##print(datafile)
    
    pathfile <- paste(directory, datafile, sep="/")
    
    ##print(pathfile)
    
    data <- read.csv(pathfile )
    
    data<-na.omit(data)
    
    corrscomplete <- c(corrscomplete,cor(data$sulfate, data$nitrate))
 
  }
  return(corrscomplete)
}
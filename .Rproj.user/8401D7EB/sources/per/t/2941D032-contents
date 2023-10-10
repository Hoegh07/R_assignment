best <- function(state,out){
  #Read csv
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  #Check outcome is valid
  if(out == "heart attack"){
    x <- 11
  }
  else if(out == "heart failure"){
    x <- 17
  }
  else if(out == "pneumonia"){
    x <- 23
  }
  else{
    stop("invalid outcome")
  }
  
  #Check state is valid
  s <- unique(data[,7])
  if(!(state %in% s)){
    stop("invalid state")
  }
  
  #Filter according to state
  data <- data[data$State == state,]
  
  #Cast the mortality rate column to be numeric
  data[,x] <- as.numeric(data[,x])
  
  #Extract only relevant colunms (hospital name and mortality rate)
  #and omit the NANs
  data <- data[,c(2,x)]
  data <- na.omit(data)
  
  #Get smallest mortality rate and filter according to this
  mini <- min(data[,2])
  data <- data[data[,2] == mini,]
  min(data[,1])
}







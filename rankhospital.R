rankhospital <- function(state,out,num){
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
  
  #Filter to get the names of the hospitals ordered by mortality_rate
  #and then by the name of the hospital
  data <- data[data$State == state,]
  data <- data[,c(2,x)]
  data[,2] <- as.numeric(data[,2])
  data <- na.omit(data)
  colnames(data) = c("name","mortality_rate")
  data <- data[order(data$mortality_rate,data$name),]
  data <- data[,1]
  
  #Select the appropriate row
  if(num == "best"){
    data[1]
  }
  else if(num == "worst"){
    data[length(data)]
  }
  else if(num > length(data)){
    z <- "NA"
    z
  }
  else{
    data[num]
  }
}












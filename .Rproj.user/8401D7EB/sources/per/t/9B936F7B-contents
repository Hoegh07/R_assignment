rankall <- function(out,num="best"){
  #read data
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
  
  #get states
  s <- unique(data[,7])
  
  #Choose the required colums (name,state,mortality rate)
  data <- data[,c(2,7,x)]
  colnames(data) <- c("name","State","mortality_rate")
  data[,3] <- as.numeric(data[,3])
  data <- data[order(data$State,data$mortality_rate,data$name),]
  data <- na.omit(data)
  data <- split(data,data$State)
  
  if(num == "best"){
    name <- sapply(data,function(x){x[[1]][1]})
    state <- sapply(data,function(x){x[[2]][1]})
    u <- cbind(name,state)
    data.frame(u)
  }
  else if(num == "worst"){
    name <- sapply(data,function(x){x[[1]][length(x[[1]])]})
    state <- sapply(data,function(x){x[[2]][1]})
    u <- cbind(name,state)
    data.frame(u)
  }
  else{
    name <- sapply(data,function(x){x[[1]][num]})
    state <- sapply(data,function(x){x[[2]][1]})
    u <- cbind(name,state)
    data.frame(u)
  }
}
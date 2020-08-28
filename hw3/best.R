setwd("G:\\Learn\\Coursera\\Data Science\\2R Programming\\hw3")
library(VIM)

best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  hd <- read.csv("hospital-data.csv", header = T)
  st <- as.character(unique(hd$State))
  
  if(sum(state == st) == 0){
    stop("invalid state")
  }
  if(outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia"){
    stop("invalid outcome")
  }
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  name <- names(data)
  data1 <- subset(data, State == state, select = c(2, 11, 17, 23))
  names(data1)[1:4] <- c("name", "ha", "hf", "p")
  data1[data1 == "Not Available"] <- NA
  data2 <- na.omit(data1)
  
  if(outcome == "heart attack"){
    return(data2[[1]][which.min(data2[[2]])])
  }
  else if(outcome == "heart failure"){
    return(data2[[1]][which.min(data2[[3]])])
  }
  else{
    return(data2[[1]][which.min(data2[[4]])])
  }
}
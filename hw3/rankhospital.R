setwd("G:\\Learn\\Coursera\\Data Science\\2R Programming\\hw3")
library(VIM)

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
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
  if(nrow(data1) != 0){
    data1[data1 == "Not Available"] <- NA
  }
  
  data2 <- data1
  data3 <- data2[order(data2[1]),]
  
  if(nrow(data2) == 0){
    return(NA)
  }
  
  if(num == "best"){num <- 1}
  if(num == "worst"){num <- nrow(data2)}
  
  if(outcome == "heart attack"){
    data4 <- subset(data3, !is.na(ha), select = c(1,2))
    index <- data4[[1]][order(as.numeric(data4[[2]]))[num]]
    return(index)
  }
  else if(outcome == "heart failure"){
    data4 <- subset(data3, !is.na(hf), select = c(1,3))
    index <- data4[[1]][order(as.numeric(data4[[2]]))[num]]
    return(index)
  }
  else{
    data4 <- subset(data3, !is.na(p), select = c(1,4))
    index <- data4[[1]][order(as.numeric(data4[[2]]))[num]]
    return(index)
  }
  
  
}
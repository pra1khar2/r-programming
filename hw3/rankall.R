setwd("G:\\Learn\\Coursera\\Data Science\\2R Programming\\hw3")
library(VIM)

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  source("rankhospital.R")
  
  hd <- read.csv("hospital-data.csv", header = T)
  st <- sort(as.vector(unique(hd$State)))
  
  hospitalList <- c()
  
  for(state in st){
    #print(state)
    hos <- rankhospital(state, outcome, num)
    hospitalList <- c(hospitalList, hos)
    #print(hos)
  }
  
  #print(hospitalList)
  
  re <- data.frame(hospitalList, st)
  names(re)[1:2] <- c("hospital", "state")
  
  return(re)
}
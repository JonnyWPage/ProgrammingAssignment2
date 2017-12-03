rankall <- function(outcome, num = "best") {
        ## outcome is a character vector of length one specifying 
        ## one of three outcomes: "heart attack", "heart failure"
        ## or "pneumonia".
  
        ## num is a numeric vector of length one specifying the 
        ## ranking of a desired hospital in the given state.
        ## num can also take the value of 'best' or 'worst', in 
        ## which case rank hospital returns the highest or lowest
        ## ranked hospital respectively.
  
        ## rankall returns a data frame containing the hospital in
        ## US state at the specified rank for 30-day mortality
        ## rates for the specified outcome.
  
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors=FALSE)
  
  ## Check that outcome is valid
  trueOutcomes <- c("heart attack","heart failure","pneumonia")
  
  if (all(outcome != trueOutcomes)) {
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  if (outcome == "heart failure") {
    outcomeData <- outcomeData[order(outcomeData$Hospital.Name),] 
    ocData <- outcomeData[order(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,decreasing=FALSE),]
    mortalityRate <- ocData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  }
  
  else if (outcome == "heart attack") {
    outcomeData <- outcomeData[order(outcomeData$Hospital.Name),] 
    ocData <- outcomeData[order(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,decreasing=FALSE),]
    mortalityRate <- ocData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  }
  
  else{
    outcomeData <- outcomeData[order(outcomeData$Hospital.Name),] 
    ocData <- outcomeData[order(outcomeData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,decreasing=FALSE),]
    mortalityRate <- ocData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  }
  
  state <- ocData$State
  hospital <- ocData$Hospital.Name
  ocMat <- data.frame(hospital,mortalityRate,state)
 # ocMat <- na.omit(ocMat)
  
  ocSplit <- split(ocMat,ocMat$state)
  
  findRankHosp <- function(hospitals) {
    if (num == "best") {
      newnum <- 1
    }
    else if (num == "worst") {
      newnum <- dim(hospitals)[1]
    }
    else {
      newnum <- num
    }
    return(hospitals$hospital[newnum])
  }
  
  hospDf <- lapply(ocSplit,findRankHosp)
  
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  numHosps <- data.frame(hospital=unlist(hospDf), state=names(hospDf), row.names=names(hospDf))
}
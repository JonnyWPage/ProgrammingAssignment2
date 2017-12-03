rankhospital <- function(state, outcome, num = "best") {
        ## state is a 2-character abbreviated name of a US state
  
        ## outcome is a character vector of length one specifying 
        ## one of three outcomes: "heart attack", "heart failure"
        ## or "pneumonia".
  
        ## num is a numeric vector of length one specifying the 
        ## ranking of a desired hospital in the given state.
        ## num can also take the value of 'best' or 'worst', in 
        ## which case rank hospital returns the highest or lowest
        ## ranked hospital respectively.
  
        ## rankhospital returns a character vector with the name of
        ## the hospital in a specific state ranked at the given 'num'
        ## value for 30-day mortality rates in a given 'outcome'
  
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors=FALSE)
  
  ## Check that state and outcome are valid
  if (!any(state==unique(outcomeData$State))) {
    stop("invalid state")
  }
  
  trueOutcomes <- c("heart attack","heart failure","pneumonia")
  
  if (all(outcome != trueOutcomes)) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  stateData <- outcomeData[(outcomeData$State==state),]
  
  if (outcome == "heart failure") {
    ocData <- stateData[order(stateData$Hospital.Name),]
    ocData <- ocData[order(ocData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,decreasing=FALSE),]
    nameMat <- matrix(ocData$Hospital.Name,ncol=1)
    mortMat <- matrix(ocData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,ncol=1)
    ocMat <- cbind(nameMat,mortMat)
    ocMat <- na.omit(ocMat)
  }
  else if (outcome == "heart attack") {
    ocData <- stateData[order(stateData$Hospital.Name),]
    ocData <- ocData[order(ocData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,decreasing=FALSE),]
    nameMat <- matrix(ocData$Hospital.Name,ncol=1)
    mortMat <- matrix(ocData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,ncol=1)
    ocMat <- cbind(nameMat,mortMat)
    ocMat <- na.omit(ocMat)
  }
  else {
    ocData <- stateData[order(stateData$Hospital.Name),]
    ocData <- ocData[order(ocData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,decreasing=FALSE),]
    nameMat <- matrix(ocData$Hospital.Name,ncol=1)
    mortMat <- matrix(ocData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,ncol=1)
    ocMat <- cbind(nameMat,mortMat)
    ocMat <- na.omit(ocMat)
  }
  
  if (num == "best") {
    rankHosp <- ocMat[1,1]
  }
  else if (num == "worst") {
    rankHosp <- tail(ocMat,n=1)[1]
  }
  else if (num > nrow(ocMat)) {
    rankHosp <- NA
  }
  else {
    rankHosp <- ocMat[num,1]
  }
  
  return(rankHosp)
}
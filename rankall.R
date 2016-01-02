## setwd('~/coursera/repoGIT//rProgramming/ProgrammingAssignment3')
## source('rankhospital.r')
## uTest()
rankall <- function(outcome, num = "best") {
	## Read outcome data
	read <- outcomeOfCare()	
	
	
	
	## Check that outcome are valid
	if( validOutcome(outcome) == FALSE){
		## - invalid outcome
		stop(paste('Error in best(',state, ', \"',outcome,'\") : invalid outcome'))
	}
	## Select the proper colum based on the outcome selected
	if (outcome == "pneumonia") 			colNum = 23
	else if( outcome == "heart failure")	colNum = 17
	else ( outcome == "heart attack")    	colNum = 11
	
	hospitalList <- read[,c(2, 7,colNum)]
	availableData <- hospitalList[which(hospitalList[,3] != "Not Available"),]
	hlSplit <- split(availableData[,c(1,3)], availableData[,2]) ## split by state
	
	lapply( hlSplit, function(hl, rank){
		hl[order(as.numeric(hl[z,rank])),]
	})
	
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
}


######################################################################
# UNIT TEST
uTest <- function(){
	

}

##################################################################
## UTILITIES
outcomeOfCare <- function(){
	read.csv("outcome-of-care-measures.csv", colClasses = "character")
}

## check if the state is a valid one by reading the column state from the outcome file
## [in] state to look for
## [in] data table
## [out] boolean: true if state is valid
validSate <- function(state, read){
	x <-  unique(read$State)
	if ( state %in% x) TRUE
	else 	FALSE
}

## check if the outcome is valid, i.e. one of the 3 given
validOutcome<-function(outcome){
	if((outcome == "heart attack") || (outcome == "heart failure") ||(outcome == "pneumonia") ) TRUE
	else FALSE
}
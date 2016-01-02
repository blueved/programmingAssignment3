##setwd('~/coursera/repoGIT//rProgramming/ProgrammingAssignment3')
## source('prog_3.r')
## testBest()

best <- function(state, outcome) {
	## Read outcome data
	read <- outcomeOfCare()	
	
	
	## Check that state and outcome are valid
	if( validOutcome(outcome) == FALSE){
		## - invalid outcome
		##res <- paste('Error in best(',state, ', \"',outcome,'\") : invalid outcome')
		stop(paste('Error in best(',state, ', \"',outcome,'\") : invalid outcome'))
	}else if (validSate(state, read) == FALSE){
		## - invalid state
		##res <- paste('Error in best("',state,'", \"',outcome,'\") : invalid state')
		stop(paste('Error in best("',state,'", \"',outcome,'\") : invalid state'))
	}else{
		res <- "-"
		## - valid inputs: 
		## - select the proper colum based on the outcome selected
		if (outcome == "pneumonia") 			colNum = 23
		else if( outcome == "heart failure")	colNum = 17
		else if( outcome == "heart attack")    	colNum = 11
		else {
			message(paste("ERROR : invalid outcome", outcome))
		}
		## get the list of hospitals in the state
		hospitalList <- read[which(read[,7] == state),c(2, 7,colNum)]
		availableData <- hospitalList[which(hospitalList[,3] != "Not Available"),]
		
		## get the mean value
		min <- min(as.numeric(availableData[,3]))

		## look up the hospital(s)
		bestHospitals <- availableData[which(as.numeric(availableData[,3])== min),]
		## Return first hospital in the list
		res <- bestHospitals[1,1]
	}
	
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

##################################################################
## TESTING
testBest <- function(){
	bestRes <- c(	"CYPRESS FAIRBANKS MEDICAL CENTER",
					"FORT DUNCAN MEDICAL CENTER",
					"JOHNS HOPKINS HOSPITAL, THE",
					"GREATER BALTIMORE MEDICAL CENTER",
					"invalid state",
					"invalid outcome")
	bestCalc<- c(	best("TX", "heart attack"), 
					best("TX", "heart failure"),
					best("MD", "heart attack"),
					best("MD", "pneumonia"),
					best("BB", "heart attack"),
					best("NY", "hert attack"))
	res <- data.frame(bestRes, bestCalc)

	colnames(res)<-c('Expected', 'Calculated')
	res
}



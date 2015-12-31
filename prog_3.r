##setwd('~/coursera/repoGIT//rProgramming/ProgrammingAssignment3')

best <- function(state, outcome) {
	## Read outcome data
	read <- outcomeOfCare()	
	res <- ""
	
	## Check that state and outcome are valid
	if( validOutcome(outcome) == FALSE){
		## - invalid outcome
		res <- paste('Error in best(',state, ', \"',outcome,'\") : invalid outcome')
	}else if (validSate(state, read) == FALSE){
		## - invalid state
		res <- paste('Error in best("',state,'", \"',outcome,'\") : invalid state')
	}else{
		## - valid inputs: 
		
		

		## Return hospital name in that state with lowest 30-day death
		res <- "CC"
		## rate
	}
	res
}

##################################################################
## UTILITIES
outcomeOfCare <- function(){
	read.csv("outcome-of-care-measures.csv", colClasses = "character")
}

## get the state codes from us_states.csv (not part of this homework)
stateCode <- function(){
	stateList<-read.csv("us_states.csv")
	stateList[,2]
}

## check if the state is a valid one by reading the column state from the outcome file
## [in] state to look for
## [in] data table
## [out] boolean: true if state is valid
validSate <- function(state, read){
	x <-  unique(read$State)##stateCode()
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
	bestRes <- c(	"FORT DUNCAN MEDICAL CENTER",
					"JOHNS HOPKINS HOSPITAL, THE",
					"GREATER BALTIMORE MEDICAL CENTER",
					"invalid state",
					"invalid outcome")
	bestCalc<- c(	best("TX", "heart attack"), 
					best("MD", "heart attack"), 
					best("MD", "pneumonia"),
					best("BB", "heart attack"),
					best("NY", "hert attack"))
	res <- data.frame(bestRes, bestCalc)

	colnames(res)<-c('Expected', 'Calculated')
	res
}



## setwd('~/coursera/repoGIT//rProgramming/ProgrammingAssignment3')
## source('rankhospital.r')
## uTest()

rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	read <- outcomeOfCare()	
	
	## Check that state and outcome are valid
	if( validOutcome(outcome) == FALSE){
		## - invalid outcome
		stop(paste('Error in best(',state, ', \"',outcome,'\") : invalid outcome'))
	}else if (validSate(state, read) == FALSE){
		## - invalid state
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
		sortedData <- availableData[order(as.numeric(availableData[,3])),]
		rank <- c(1: dim(sortedData)[1])
		sortedData$rank<-rank
		totalItem <- nrow(sortedData)
		
		if(class(num) == "numeric"){
			if(num > totalItem){
				"NA"
			}else{
				sortedData[num, 1]
			}
		} else{
			if (num == "best"){
				sortedData[1, 1]
			}else if(num == "worst"){
				sortedData[totalItem, 1]
			}else{
				message(paste("Don't know this one: ", num))
			}
		}
	}
	
}

######################################################################
# UNIT TEST
uTest <- function(){
	expected <- c("DETAR HOSPITAL NAVARRO", "HARFORD MEMORIAL HOSPITAL", "NA" )
	calculated <- c(
		rankhospital("TX", "heart failure", 4),
		rankhospital("MD", "heart attack", "worst"),
		rankhospital("MN", "heart attack", 5000))
	## res <- data.frame(expected, calculated)
	## colnames(res)<-c('Expected', 'Calculated')
	## res
	calculated
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
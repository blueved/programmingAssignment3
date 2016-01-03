## setwd('~/coursera/repoGIT//rProgramming/ProgrammingAssignment3')
## source('rankall.r')
## uTest()
rankall <- function(outcome, rang = "best") {
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
	else colNum = 11
	

	hospitalList <- read[,c(2, 7,colNum)]
	availableData <- hospitalList[which(hospitalList[,3] != "Not Available"),]
	hlSplit <- split(availableData[,c(1,3, 2)], availableData[,2])   ## split by state
	
	res <- data.frame( "hospital" = character(), "state" = character(), stringsAsFactors=FALSE)
	lapply( hlSplit, function(state_list, r = rang, result = res){
		totalItem <- nrow(state_list)
		if(class(r) == "numeric"){
			
		} else{
			if (r == "best"){
				r = 1
			}else if(r == "worst"){
				r = totalItem
			}
		}

		if (r > totalItem)  {
			result[ nrow(result)+1, ] <- c("<NA>",  state_list[1,3])
		} else {
			state_ordered<-state_list[order(as.numeric(state_list[,2]), state_list[,1]),]
			result[nrow(result)+1, ] <- c(state_ordered[r,1], state_ordered[r,3])
		}
		res <<- result
	})
	res
}


######################################################################
# UNIT TEST
uTest <- function(){
	message ("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
	message('*** head(rankall("heart attack", 20), 10)')
	print(head(rankall("heart attack", 20), 10))
	
	a <- c("AK", "<NA>","AK")
	b <- c("AL","D W MCMILLAN MEMORIAL HOSPITAL","AL")
	d <- c("AR", "ARKANSAS METHODIST MEDICAL CENTER","AR")
	e <- c("AZ", "JOHN C LINCOLN DEER VALLEY HOSPITAL","AZ")
	f <- c("CA", "SHERMAN OAKS HOSPITAL","CA")
	##aa <- data.frame(a,b,c,d,e,f)
		c("CT", "MIDSTATE MEDICAL CENTER","CT")
		c("DC", "<NA> ","CD")
		c("DE", "<NA>","AR")
		c("FL", "SOUTH FLORIDA BAPTIST HOSPITAL FL","FL")
	##aa.colnames <- c("x",'y','z')
	message ("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")	
	message('*** tail(rankall("pneumonia", "worst"), 3)')
	print(tail(rankall("pneumonia", "worst"), 3))
	message ("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")	
	message('*** tail(rankall("heart failure"), 10)')
	tail(rankall("heart failure"), 10)

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
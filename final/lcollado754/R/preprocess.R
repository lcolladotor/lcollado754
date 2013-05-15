#' Pre-process the raw data
#'
#' Merge the data from the two data sets and format appropriately for use in R.
#'
#'@examples data <- preprocess()
#' head(data)
#' summary(data)
#'@export

preprocess <- function() {
	require(plyr)
	
	## Join both input data sets to have all 17 variables
	data <- join(loansData, loansData2, type = "full")	
	
	## Fix the interest variable and others that have %
	data$Interest.Rate <- as.numeric(gsub("%", "", as.character(data$Interest.Rate)))
	data$Debt.To.Income.Ratio <- as.numeric(gsub("%", "", as.character(data$Debt.To.Income.Ratio)))
	
	## Fix the Loan.Length var by removing "months"
	data$Loan.Length <- as.integer(gsub(" months", "", as.character(data$Loan.Length)))
	
	## Fix the date variables
	data$Issued.Date <- as.Date(as.character(data$Issued.Date), format="%m/%d/%y")
	data$Earliest.CREDIT.Line <- as.Date(as.character(data$Earliest.CREDIT.Line), format="%m/%d/%y")
	
	## Fix the NA's in Employment.Length
	data$Employment.Length[which(data$Employment.Length == "n/a")] <- NA
	data$Employment.Length <- data$Employment.Length[, drop=TRUE]
	
	## Drop unused levels in Home.Ownership, FICO.Range, ...
	data$Home.Ownership <- data$Home.Ownership[, drop=TRUE]
	data$FICO.Range <- data$FICO.Range[, drop=TRUE]
	data$Loan.Purpose <- data$Loan.Purpose[, drop=TRUE]
	data$State <- data$State[, drop=TRUE]
	
	## Done =)
	return(data)
}

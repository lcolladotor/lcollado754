#' Pre-process the raw data
#'
#' Merge the data from the two data sets and format appropriately for use in R.
#'
#'@param data If \code{NULL} then it merges the two default data sets (loansData, loansData2). Otherwise, it must be a data.frame with the same columns as those in the previously mentioned data sets.
#'
#'@return A data.frame with 20 columns. The 17 columns from loansData and loansData2 properly merged and processed, plus the State Region code (01 to 10) according to the AQS data set and the State Coast (East or West). The column \code{FICO.num} has the value of the mean of the FICO range to use it as a numerical variable.
#'
#'@examples data <- preprocess()
#' head(data)
#' summary(data)
#'@export
#'@seealso \link{loansData} \link{loansData2} \link{states}

preprocess <- function(data=NULL) {
	require(plyr)
	## If no data is given then:
	## Join both input data sets to have all 17 variables
	if(is.null(data)) {
		data <- join(loansData, loansData2, type = "full")
	} else {
		if(is.data.frame(data) == FALSE) stop("'data' must be a data.frame")
		
		if(sum(colnames(data) %in% c('Amount.Requested','Amount.Funded.By.Investors','Interest.Rate','Loan.Length','Loan.Purpose','Debt.To.Income.Ratio','State','Home.Ownership','Monthly.Income','FICO.Range','Open.CREDIT.Lines','Revolving.CREDIT.Balance','Inquiries.in.the.Last.6.Months','Employment.Length','Issued.Date','Earliest.CREDIT.Line','Total.CREDIT.Lines')) != 17 )
			stop("'data' is missing one of the default columns. Check the columns of loansData and loansData2 to determine which ones you are missing.")
	}
	
	## Load state information
	codes <- as.list(tapply(states$State.Abbr, states$Region, table))[1:10]
	regions <- ldply(codes, function(x) { data.frame("State.Abbr" = x)})[, c(".id", "State.Abbr.Var1")]
	colnames(regions) <- c("id", "abbr")
	
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
	
	## Group states	
	idx <- sapply(as.character(data$State), function(x) { which(regions$abbr == x) })
	data$State.Region <- factor(regions$id[idx])
	data$State.Coast <- factor(sapply(regions$id[idx], function(y) { ifelse(as.integer(y) <=5, "East", "West") }))
	
	# FICO score by mean
	data$FICO.num <- sapply(as.character(data$FICO.Range), function(z) { mean(as.integer(strsplit(z, "-")[[1]])) })
	
	## Done =)
	return(data)
}

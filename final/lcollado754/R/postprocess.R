#' Process the initial data using insight from the EDA step.
#'
#' Re-formats the data using insight gained from EDA.
#'
#'@param data A data set produced by \link{preprocess}. 
#'
#'@return A data.frame with 17 columns. Most are the same as the ones returned by \link{preprocess}. Others are modifications of previous variables. Some of the state information is dropped.
#'
#'@examples data <- preprocess()
#' ready <- postprocess(data)
#' head(ready)
#' summary(ready)
#'@export
#'@seealso \link{loansData} \link{loansData2} \link{states} \link{preprocess}

postprocess <- function(data) {
	## Fix some dates
	idx <- data$Earliest.CREDIT.Line > as.Date("01/01/14", format="%d/%m/%y")
	if(sum(idx, na.rm=TRUE) > 0) {
		message(paste("Removing the earliest credit line information for", sum(idx, na.rm=TRUE), "observations."))
		data$Earliest.CREDIT.Line[idx] <- NA
	}
	
	## Transform the monthly income
	data$Monthly.Income.Log <- log(data$Monthly.Income)
	
	## Re-categorize the loan purpose
	idx <- sapply(data$Loan.Purpose, function(x) { !x %in% c("credit_card", "debt_consolidation")} )
	new <- data$Loan.Purpose
	new[idx] <- "other"
	new <- new[, drop=TRUE]
	data$Loan.Purpose.Mod <- new
	
	## Re-categorize the home status
	idx <- sapply(data$Home.Ownership, function(x) { !x %in% c("MORTGAGE", "OWN", "RENT")} )
	new <- data$Home.Ownership
	new[idx] <- NA
	new <- new[, drop=TRUE]
	data$Home.Ownership.Mod <- new
	
	## Re-order the employment length
	data$Employment.Length.Mod <- factor(data$Employment.Length, levels=c('< 1 year','1 year','2 years','3 years','4 years','5 years','6 years','7 years','8 years','9 years', '10+ years'), ordered=TRUE)
	
	## Balance
	data$Revolving.CREDIT.Balance.log <- log(1 + data$Revolving.CREDIT.Balance)
	
	## Remove variables that won't be used anymore
	data <- data[, !colnames(data) %in% c("Monthly.Income", "State.Coast", "State", "Loan.Purpose", "Home.Ownership", "Employment.Length", "FICO.Range", "Revolving.CREDIT.Balance")]	
	
	## Done =)
	return(data)
}

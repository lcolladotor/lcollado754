#' Infer interest rates.
#'
#' Infers the interest rates for a given data set.
#'
#'@param topred Must be a data.frame in the same format as the one obtained from \link{postprocess}.
#'@param model If \code{NULL} the model result already included in the package will be used. Otherwise this option is meant to be used when running \link{reproduceAnalysis}.
#'
#'@return A data.frame with two columns: Estimate and SE. The names are self explanatory.
#'
#'@export

inferInterestRate <- function(topred, model=NULL) {
	require(randomForest)
	if(is.null(model)) {
		## Load model data 
		toload <- system.file("lcollado754run/VarSelect/model.RData", package="lcollado754")
		load(toload)
	}
	
	## Subset the data
	predictions <- predict(object=model, newdata=topred, type="response", predict.all=TRUE)
	
	## Build the result
	se <- apply(predictions$individual, 1, sd)
	res <- data.frame(Estimate=predictions$aggregate, SE=se)
	rownames(res) <- rownames(topred)
	
	## Done
	return(res)
}

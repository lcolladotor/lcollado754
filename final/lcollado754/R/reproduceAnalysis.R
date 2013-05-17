#' Reproduce the analysis
#'
#' Reproduces all the analysis of Leonardo Collado-Torres' 140.754 final project. To do so it will create a directory called lcollado754run in your working directory.
#'
#'@param step If \code{all} then it reproduces all the steps. Other options are \code{EDA}, \code{VarSelect} and \code{Infer}.
#'@param verbose If \code{TRUE} then progress messages will be printed along the way.
#'@param cores Number of cores to be used when running the random forests in the variable selection step.
#'
#'@details For maximum cool factor, this package requires the version of \code{markdown} that is available from GitHub, which sadly has the same version number as the version from CRAN so it won't be detected through the usual means.
#' This function can reproduce the EDA step, the variable selection step, show an example about how to use \link{inferInterestRate} or do all of these steps.
#'
#'@return The directory lcollado754run inside your working directory with all the analysis steps reproduced.
#'
#'
#'@examples reproduceAnalysis(step="EDA")
#'@export
#'@references knitr bootstrap html format from https://github.com/jimhester/knitr_bootstrap

reproduceAnalysis <- function(step="all", verbose=TRUE, cores=4) {
	if(!step %in% c("all", "EDA", "VarSelect", "Infer")) stop("'step' is not a recognized option.")
	
	if(verbose) message("Setting up.")
	# Required packages	
	require(knitr)
	require(markdown)
			
	## For super-coolness, make sure that the user has the correct version of the markdown package.
	if(!"header" %in% formalArgs(markdownToHTML)) {
		stop("Your version of the markdown package is outdated. Please updated it using: library(devtools); install_github(username='rstudio', repo='markdown')")
	}	
	
	## Locate the nice header
	boot <- system.file("knitr_bootstrap.html", package="lcollado754")
	
	## Save the working directory
	wdir <- getwd()
	
	## For testing
	if(FALSE) {
		library(lcollado754)
		wdir <- "/Users/lcollado/Desktop"
	}
		
	## Copy script files to your current working directory
	root <- "lcollado754run"
	srcdir <- system.file(root, package="lcollado754")
	xx <- file.copy(from=srcdir, to=wdir, recursive=TRUE)
	if(!xx) stop("Copying files to your working directory did not work.")
		
	## EDA
	if(step %in% c("EDA", "all")) {
		if(verbose) message("Running EDA step.")
		eda <- file.path(wdir, root, "EDA")
		setwd(eda)
		knit2html("EDA.Rmd", header=boot)
		if (interactive()) browseURL("EDA.html")
	}
	
	## Variable selection
	if(step %in% c("VarSelect", "Infer", "all")) {
		if(verbose) message("Running variable selection step.")
		setwd(wdir)	
		varsel <- file.path(wdir, root, "VarSelect")
		setwd(varsel)
		knit2html("VarSelect.Rmd", header=boot)
		if (interactive()) browseURL("VarSelect.html")
	}
	
	## Infererence
	if(step %in% c("Infer", "all")) {
		if(verbose) message("Running an example of how to infer the interest values.")
		# Load required data
		setwd(wdir)	
		load("lcollado754run/VarSelect/model.RData")
		infer <- file.path(wdir, root, "Infer")
		setwd(infer)
		knit2html("Infer.Rmd", header=boot)
		if (interactive()) browseURL("Infer.html")
	}
	
	## Change back to the working directory
	setwd(wdir)	
	
	## Done =)
	if(verbose)	{
		message("Done!")
		print(proc.time())
	}
	
}

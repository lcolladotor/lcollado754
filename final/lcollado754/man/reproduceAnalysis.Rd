\name{reproduceAnalysis}
\alias{reproduceAnalysis}
\title{Reproduce the analysis}
\usage{
  reproduceAnalysis(step = "all", verbose = TRUE)
}
\arguments{
  \item{step}{If \code{all} then it reproduces all the
  steps.}

  \item{verbose}{If \code{TRUE} then progress messages will
  be printed along the way.}
}
\value{
  The directory lcollado754run inside your working
  directory with all the analysis steps reproduced.
}
\description{
  Reproduces all the analysis of Leonardo Collado-Torres'
  140.754 final project. To do so it will create a
  directory called lcollado754run in your working
  directory.
}
\details{
  For maximum cool factor, this package requires the
  version of \code{markdown} that is available from GitHub,
  which sadly has the same version number as the version
  from CRAN so it won't be detected through the usual
  means.
}
\examples{
reproduceAnalysis(step="eda")
}
\references{
  knitr bootstrap html format from
  https://github.com/jimhester/knitr_bootstrap
}


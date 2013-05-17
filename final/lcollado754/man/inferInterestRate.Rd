\name{inferInterestRate}
\alias{inferInterestRate}
\title{Infer interest rates.}
\usage{
  inferInterestRate(topred, model = NULL)
}
\arguments{
  \item{topred}{Must be a data.frame in the same format as
  the one obtained from \link{postprocess}.}

  \item{model}{If \code{NULL} the model result already
  included in the package will be used. Otherwise this
  option is meant to be used when running
  \link{reproduceAnalysis}.}
}
\value{
  A data.frame with two columns: Estimate and SE. The names
  are self explanatory.
}
\description{
  Infers the interest rates for a given data set.
}


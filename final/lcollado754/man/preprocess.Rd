\name{preprocess}
\alias{preprocess}
\title{Pre-process the raw data}
\usage{
  preprocess(data = NULL)
}
\arguments{
  \item{data}{If \code{NULL} then it merges the two default
  data sets (loansData, loansData2). Otherwise, it must be
  a data.frame with the same columns as those in the
  previously mentioned data sets.}
}
\value{
  A data.frame with 19 columns. The 17 columns from
  loansData and loansData2 properly merged and processed,
  plus the State Region code (01 to 10) according to the
  AQS data set and the State Coast (East or West).
}
\description{
  Merge the data from the two data sets and format
  appropriately for use in R.
}
\examples{
data <- preprocess()
head(data)
summary(data)
}
\seealso{
  \link{loansData} \link{loansData2} \link{states}
}


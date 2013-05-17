\name{postprocess}
\alias{postprocess}
\title{Process the initial data using insight from the EDA step.}
\usage{
  postprocess(data)
}
\arguments{
  \item{data}{A data set produced by \link{preprocess}.}
}
\value{
  A data.frame with 17 columns. Most are the same as the
  ones returned by \link{preprocess}. Others are
  modifications of previous variables. Some of the state
  information is dropped.
}
\description{
  Re-formats the data using insight gained from EDA.
}
\examples{
data <- preprocess()
ready <- postprocess(data)
head(ready)
summary(ready)
}
\seealso{
  \link{loansData} \link{loansData2} \link{states}
  \link{preprocess}
}


lm with cross-validation
=======================

Setup

```r
## Libs
library(cvTools)
```

```
## Loading required package: lattice
```

```
## Loading required package: robustbase
```

```
## Loading required package: methods
```

```r

## Data
load("../lm/lm.Rdata")
load("../lm/trainC.Rdata")

## Yelp's function to evaluate the error
rmlspe <- function(y, yHat, includeSE = FALSE) {
    rmspe(log(y + 1), log(yHat + 1), includeSE = includeSE)
}
```



Look at the predictions after truncating them to be greater or equal to 0






gbm with cross-validation
=======================

Setup

```r
## Libs
library(gbm)
```

```
## Loading required package: survival
```

```
## Loading required package: splines
```

```
## Loading required package: lattice
```

```
## Loaded gbm 2.0-8
```

```r
library(cvTools)
```

```
## Loading required package: robustbase
```

```
## Loading required package: methods
```

```
## Attaching package: 'robustbase'
```

```
## The following object is masked from 'package:survival':
## 
## heart
```

```r

## Data
load("../lm/trainC.Rdata")

test <- FALSE
if (test) {
    trainC <- trainC[1:1000, ]
}

rmlspe <- function(y, yHat, includeSE = FALSE) {
    rmspe(log(y + 1), log(yHat + 1), includeSE = includeSE)
}

# Remove date and transform the logical vectors into factors
fixStuff <- function(input) {
    res <- input[, !colnames(input) %in% "date"]
    res$business.categories.BeautySpas <- factor(res$business.categories.BeautySpas)
    res$business.categories.Food <- factor(res$business.categories.Food)
    res$business.categories.Restaurants <- factor(res$business.categories.Restaurants)
    res$business.categories.Shopping <- factor(res$business.categories.Shopping)
    res$business.open <- factor(res$business.open)
    return(res)
}
trainC <- fixStuff(trainC)
validateC <- fixStuff(validateC)
```



Just fit one gbm with 10-fold CV





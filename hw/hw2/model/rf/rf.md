rf with cross-validation
=======================

Setup

```r
## Libs
library(randomForest)
```

```
## randomForest 4.6-7
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```r
library(cvTools)
```

```
## Loading required package: lattice
```

```
## Loading required package: robustbase
```

```r

## Following
## http://stackoverflow.com/questions/7830255/suggestions-for-speeding-up-random-forests
library("foreach")
library("doSNOW")
```

```
## Loading required package: iterators
```

```
## Loading required package: snow
```

```r
cores <- 4
registerDoSNOW(makeCluster(cores, type = "SOCK"))
getDoParWorkers()
```

```
## [1] 4
```

```r

## Data
load("../lm/trainC.Rdata")

test <- TRUE
if (test) {
    trainC <- trainC[1:1000, ]
}

# Change column names to use randomForest
words <- grep("word.", colnames(trainC))
colnames(trainC)[words] <- paste0("word", 1:length(words))

words <- grep("word.", colnames(validateC))
colnames(validateC)[words] <- paste0("word", 1:length(words))


## Training set for CV
trainx <- trainC[, !colnames(trainC) %in% "votes.useful"]
trainy <- trainC$votes.useful

## Test set for running rf
valx <- validateC[, !colnames(validateC) %in% c("user_id", "business_id", "review_id", 
    "user.name", "business.city", "votes.useful")]
valy <- validateC$votes.useful
```



Just fit one big random forest (2000 trees)

```r

## Fit random forests
rf <- foreach(ntree = rep(100, cores), .combine = combine, .packages = "randomForest") %dopar% 
    randomForest(x = trainx, y = trainy, data = trainC, xtest = valx, ytest = valy, 
        importance = TRUE, keep.forest = TRUE, ntree = ntree)



## Evaluate with the training data
e.rf.train <- rmspe(trainy, rf$predicted, includeSE = TRUE)

## Evaluate with validation data set
e.rf.val <- rmspe(valy, predict(rf, valx), includeSE = TRUE)

## Combine
e.rf <- list(e.rf.train, e.rf.val)
unlist(lapply(e.rf, function(x) {
    x$rmspe
}))
```

```
## [1] 2.231 1.997
```

```r
unlist(lapply(e.rf, function(x) {
    x$se
}))
```

```
## [1] 0.20142 0.04219
```

```r

save(rf, e.rf, file = "rf.Rdata")
```


Reproducibility

```r
print(proc.time())
```

```
##    user  system elapsed 
##   8.583   0.490  17.313
```

```r
sessionInfo()
```

```
## R version 2.15.3 (2013-03-01)
## Platform: x86_64-apple-darwin9.8.0/x86_64 (64-bit)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] doSNOW_1.0.6       snow_0.3-12        iterators_1.0.6   
## [4] foreach_1.4.0      cvTools_0.3.2      robustbase_0.9-7  
## [7] lattice_0.20-15    randomForest_4.6-7 knitr_1.1         
## 
## loaded via a namespace (and not attached):
## [1] codetools_0.2-8 compiler_2.15.3 digest_0.6.3    evaluate_0.4.3 
## [5] formatR_0.7     grid_2.15.3     stringr_0.6.2   tools_2.15.3
```


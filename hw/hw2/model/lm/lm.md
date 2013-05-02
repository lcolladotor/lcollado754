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
load("../../data/subsetTrain/train.Rdata")
```

```
## Warning: cannot open compressed file '../../data/subsetTrain/train.Rdata',
## probable reason 'No such file or directory'
```

```
## Error: cannot open the connection
```

```r

test <- FALSE
if (test) {
    train <- train[1:1000, ]
}
train <- train[, !colnames(train) %in% c("user_id", "business_id", "review_id", 
    "user.name", "business.city")]
```

```
## Error: object 'train' not found
```

```r

## The actual function used by Yelp. Careful that this only works when
## using positive values (aka, it won't work for lm)
rmlspe <- function(y, yHat, includeSE = FALSE) {
    rmspe(log(y + 1), log(yHat + 1), includeSE = includeSE)
}

validateC <- validate[complete.cases(validate), ]
```

```
## Error: object 'validate' not found
```

```r
trainC <- train[complete.cases(train), ]
```

```
## Error: object 'train' not found
```

```r
save(trainC, validateC, file = "trainC.Rdata")
```

```
## Error: object 'trainC' not found
```



Fit 10-fold cross validation with 5 repetitions

```r
## Fit the models
fit.lm <- lm(votes.useful ~ ., data = trainC)
```

```
## Error: object 'trainC' not found
```

```r
fit.aic <- step(fit.lm, trace = 0)
```

```
## Error: object 'fit.lm' not found
```

```r
fit.bic <- step(fit.lm, trace = 0, k = log(nrow(trainC)))
```

```
## Error: object 'fit.lm' not found
```

```r
summary(fit.lm)
```

```
## Error: object 'fit.lm' not found
```

```r
summary(fit.aic)
```

```
## Error: object 'fit.aic' not found
```

```r
summary(fit.bic)
```

```
## Error: object 'fit.bic' not found
```

```r

## Determine the folds
seed <- 20130501
K <- 10
R <- 5
set.seed(seed)
folds <- cvFolds(nrow(trainC), K = K, R = R)
```

```
## Error: object 'trainC' not found
```

```r

## Run the cross-validation
cv.lm <- cvFit(fit.lm, y = trainC$votes.useful, data = trainC, cost = rmspe, 
    costArgs = list(includeSE = TRUE), folds = folds)
```

```
## Error: object 'fit.lm' not found
```

```r
cv.aic <- cvFit(fit.aic, y = trainC$votes.useful, data = trainC, cost = rmspe, 
    costArgs = list(includeSE = TRUE), folds = folds)
```

```
## Error: object 'fit.aic' not found
```

```r
cv.bic <- cvFit(fit.bic, y = trainC$votes.useful, data = trainC, cost = rmspe, 
    costArgs = list(includeSE = TRUE), folds = folds)
```

```
## Error: object 'fit.bic' not found
```

```r

cv.sel <- cvSelect(lm = cv.lm, aic = cv.aic, bic = cv.bic)
```

```
## Error: object 'cv.lm' not found
```

```r
cv.sel
```

```
## Error: object 'cv.sel' not found
```

```r
cv.sel$se
```

```
## Error: object 'cv.sel' not found
```

```r
cv.sel$reps
```

```
## Error: object 'cv.sel' not found
```

```r

e.lm <- rmspe(validateC$votes.useful, predict(fit.lm, validateC), includeSE = TRUE)
```

```
## Error: object 'validateC' not found
```

```r
e.aic <- rmspe(validateC$votes.useful, predict(fit.aic, validateC), includeSE = TRUE)
```

```
## Error: object 'validateC' not found
```

```r
e.bic <- rmspe(validateC$votes.useful, predict(fit.bic, validateC), includeSE = TRUE)
```

```
## Error: object 'validateC' not found
```

```r
e.lm
```

```
## Error: object 'e.lm' not found
```

```r
e.aic
```

```
## Error: object 'e.aic' not found
```

```r
e.bic
```

```
## Error: object 'e.bic' not found
```

```r

rm(validate, validateC, train, trainC, test, seed, K, R)
```

```
## Warning: object 'validate' not found
```

```
## Warning: object 'validateC' not found
```

```
## Warning: object 'train' not found
```

```
## Warning: object 'trainC' not found
```

```r
save.image(file = "lm.Rdata")
```


Reproducibility

```r
print(proc.time())
```

```
##    user  system elapsed 
##   0.847   0.183   2.324
```

```r
sessionInfo()
```

```
## R version 3.0.0 Patched (2013-04-30 r62698)
## Platform: x86_64-unknown-linux-gnu (64-bit)
## 
## locale:
##  [1] LC_CTYPE=en_US.iso885915       LC_NUMERIC=C                  
##  [3] LC_TIME=en_US.iso885915        LC_COLLATE=en_US.iso885915    
##  [5] LC_MONETARY=en_US.iso885915    LC_MESSAGES=en_US.iso885915   
##  [7] LC_PAPER=C                     LC_NAME=C                     
##  [9] LC_ADDRESS=C                   LC_TELEPHONE=C                
## [11] LC_MEASUREMENT=en_US.iso885915 LC_IDENTIFICATION=C           
## 
## attached base packages:
## [1] methods   stats     graphics  grDevices utils     datasets  base     
## 
## other attached packages:
## [1] cvTools_0.3.2    robustbase_0.9-7 lattice_0.20-15  knitr_1.2       
## 
## loaded via a namespace (and not attached):
## [1] digest_0.6.3   evaluate_0.4.3 formatR_0.7    grid_3.0.0    
## [5] stringr_0.6.2  tools_3.0.0
```


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


```r
pred.lm <- predict(fit.lm, validateC)
summary(pred.lm)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  -0.831   0.627   1.130   1.390   1.880  14.000
```

```r
pred.lm[pred.lm < 0] <- 0
summary(pred.lm)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   0.627   1.130   1.390   1.880  14.000
```

```r


pred.lm.aic <- predict(fit.aic, validateC)
summary(pred.lm.bic)
```

```
## Error: object 'pred.lm.bic' not found
```

```r
pred.lm.aic[pred.lm.aic < 0] <- 0
summary(pred.lm.bic)
```

```
## Error: object 'pred.lm.bic' not found
```

```r


pred.lm.bic <- predict(fit.bic, validateC)
summary(pred.lm.bic)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  -0.815   0.627   1.130   1.390   1.880  14.200
```

```r
pred.lm.bic[pred.lm.bic < 0] <- 0
summary(pred.lm.bic)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   0.627   1.130   1.390   1.880  14.200
```

```r


e.l.lm <- rmlspe(validateC$votes.useful, pred.lm, includeSE = TRUE)
e.l.aic <- rmlspe(validateC$votes.useful, pred.lm.aic, includeSE = TRUE)
e.l.bic <- rmlspe(validateC$votes.useful, pred.lm.bic, includeSE = TRUE)

e.eval.lm <- list(e.l.lm, e.l.aic, e.l.bic)
unlist(lapply(e.eval.lm, function(x) {
    x$rmspe
}))
```

```
## [1] 0.5907 0.5907 0.5908
```

```r
unlist(lapply(e.eval.lm, function(x) {
    x$se
}))
```

```
## [1] 0.001635 0.001635 0.001635
```

```r

save(e.eval.lm, file = "eval-lm.Rdata")
```


Reproducibility

```r
print(proc.time())
```

```
##    user  system elapsed 
##   7.057   1.555   8.669
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


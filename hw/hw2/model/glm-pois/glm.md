glm-pois with cross-validation
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
load("../lm/trainC.Rdata")

test <- TRUE
if (test) {
    trainC <- trainC[1:1000, ]
}
```



Fit 10-fold cross validation with 5 repetitions

```r
## Fit poisson the models
fit.pois <- glm(votes.useful ~ ., data = trainC, family = poisson)
fit.pois.aic <- step(fit.pois, trace = 0)
fit.pois.bic <- step(fit.pois, trace = 0, k = log(nrow(trainC)))
summary(fit.pois)
```

```
## 
## Call:
## glm(formula = votes.useful ~ ., family = poisson, data = trainC)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
##  -3.72   -1.23   -0.40    0.48    7.27  
## 
## Coefficients:
##                                      Estimate Std. Error z value Pr(>|z|)
## (Intercept)                          6.52e+01   4.64e+01    1.41  0.16001
## stars                               -3.97e-02   3.00e-02   -1.32  0.18563
## date                                -4.99e-04   5.08e-05   -9.82  < 2e-16
## `word. `                             5.14e-03   1.60e-03    3.21  0.00134
## word..                               3.06e-03   4.75e-03    0.64  0.51902
## word.the                            -5.53e-02   9.86e-03   -5.61  2.0e-08
## `word.,`                            -1.11e-02   6.84e-03   -1.63  0.10330
## word.and                             6.77e-03   1.13e-02    0.60  0.55099
## word.I                              -5.36e-03   1.02e-02   -0.53  0.59868
## word.a                              -1.55e-02   1.21e-02   -1.28  0.20046
## `word.\\n`                           2.20e-02   6.04e-03    3.64  0.00027
## word.to                             -9.39e-03   1.39e-02   -0.68  0.49780
## word.of                              2.90e-02   1.50e-02    1.93  0.05403
## word.was                            -9.58e-03   1.62e-02   -0.59  0.55416
## word.is                             -1.64e-02   1.93e-02   -0.85  0.39503
## word.for                             2.70e-02   2.08e-02    1.30  0.19469
## word.it                             -4.08e-02   1.84e-02   -2.22  0.02618
## word.in                              5.83e-03   1.97e-02    0.30  0.76778
## `word.!`                             9.00e-03   9.35e-03    0.96  0.33554
## word.The                            -3.11e-02   2.11e-02   -1.47  0.14054
## word.that                            3.20e-03   2.30e-02    0.14  0.88930
## word.with                           -2.79e-02   2.32e-02   -1.20  0.22923
## word.but                            -1.52e-02   2.54e-02   -0.60  0.55036
## word.you                            -2.38e-02   1.81e-02   -1.31  0.18870
## word.my                              1.17e-02   2.45e-02    0.48  0.63297
## word.on                              1.74e-02   2.47e-02    0.70  0.48134
## word.have                            6.76e-02   2.29e-02    2.95  0.00314
## word.this                           -2.68e-02   2.63e-02   -1.02  0.30756
## word.had                             2.94e-02   2.84e-02    1.03  0.30151
## word.are                             5.56e-02   2.97e-02    1.87  0.06165
## word.they                           -5.69e-02   2.51e-02   -2.27  0.02305
## word.not                            -4.69e-02   3.12e-02   -1.50  0.13280
## `word.)`                            -7.56e-03   8.29e-02   -0.09  0.92736
## word.place                          -5.35e-02   3.07e-02   -1.74  0.08134
## word.at                              1.11e-01   2.95e-02    3.78  0.00016
## word.good                           -5.49e-02   3.16e-02   -1.74  0.08263
## word.were                            3.98e-02   3.01e-02    1.32  0.18636
## `word.(`                            -1.45e-02   8.77e-02   -0.17  0.86894
## word.food                            3.38e-02   3.57e-02    0.95  0.34365
## `word."`                             5.09e-02   1.35e-02    3.78  0.00015
## word.we                             -1.27e-01   2.03e-02   -6.27  3.6e-10
## word.so                             -3.37e-02   3.09e-02   -1.09  0.27571
## `word.-`                            -2.20e-02   1.73e-02   -1.27  0.20500
## word.be                              1.11e-01   3.42e-02    3.25  0.00114
## word.as                             -9.68e-02   3.10e-02   -3.12  0.00180
## word.like                           -6.24e-02   3.79e-02   -1.64  0.10000
## word.me                              7.99e-02   2.96e-02    2.70  0.00701
## word.out                             7.41e-02   3.20e-02    2.31  0.02071
## word.there                          -7.95e-02   3.58e-02   -2.22  0.02659
## word.here                           -7.10e-03   3.67e-02   -0.19  0.84675
## word.just                            1.88e-02   3.66e-02    0.51  0.60735
## word.great                          -1.08e-03   3.97e-02   -0.03  0.97829
## word.all                             8.22e-03   3.72e-02    0.22  0.82493
## user.average_stars                   1.87e-01   6.34e-02    2.94  0.00324
## user.review_count                    1.17e-03   8.87e-05   13.18  < 2e-16
## business.categories.BeautySpasTRUE  -3.26e-01   2.18e-01   -1.49  0.13607
## business.categories.FoodTRUE        -5.77e-02   8.62e-02   -0.67  0.50323
## business.categories.RestaurantsTRUE -1.35e-02   8.41e-02   -0.16  0.87277
## business.categories.ShoppingTRUE    -2.85e-01   1.51e-01   -1.89  0.05941
## business.categories.total            5.93e-02   2.33e-02    2.55  0.01076
## business.latitude                   -4.28e-02   3.15e-01   -0.14  0.89204
## business.longitude                  -2.24e-01   2.99e-01   -0.75  0.45326
## business.openTRUE                    3.61e-02   1.11e-01    0.33  0.74474
## business.review_count                3.62e-04   2.21e-04    1.64  0.10120
## business.stars                       1.95e-01   6.45e-02    3.03  0.00248
## business.zipcode                    -9.78e-04   2.67e-04   -3.67  0.00024
## checkin.total                        2.74e-06   1.67e-05    0.16  0.86961
##                                        
## (Intercept)                            
## stars                                  
## date                                ***
## `word. `                            ** 
## word..                                 
## word.the                            ***
## `word.,`                               
## word.and                               
## word.I                                 
## word.a                                 
## `word.\\n`                          ***
## word.to                                
## word.of                             .  
## word.was                               
## word.is                                
## word.for                               
## word.it                             *  
## word.in                                
## `word.!`                               
## word.The                               
## word.that                              
## word.with                              
## word.but                               
## word.you                               
## word.my                                
## word.on                                
## word.have                           ** 
## word.this                              
## word.had                               
## word.are                            .  
## word.they                           *  
## word.not                               
## `word.)`                               
## word.place                          .  
## word.at                             ***
## word.good                           .  
## word.were                              
## `word.(`                               
## word.food                              
## `word."`                            ***
## word.we                             ***
## word.so                                
## `word.-`                               
## word.be                             ** 
## word.as                             ** 
## word.like                           .  
## word.me                             ** 
## word.out                            *  
## word.there                          *  
## word.here                              
## word.just                              
## word.great                             
## word.all                               
## user.average_stars                  ** 
## user.review_count                   ***
## business.categories.BeautySpasTRUE     
## business.categories.FoodTRUE           
## business.categories.RestaurantsTRUE    
## business.categories.ShoppingTRUE    .  
## business.categories.total           *  
## business.latitude                      
## business.longitude                     
## business.openTRUE                      
## business.review_count                  
## business.stars                      ** 
## business.zipcode                    ***
## checkin.total                          
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 2751.5  on 999  degrees of freedom
## Residual deviance: 1658.1  on 933  degrees of freedom
## AIC: 3331
## 
## Number of Fisher Scoring iterations: 6
```

```r
summary(fit.pois.aic)
```

```
## 
## Call:
## glm(formula = votes.useful ~ date + `word. ` + word.the + `word.,` + 
##     word.a + `word.\n` + word.of + word.for + word.it + word.The + 
##     word.you + word.have + word.are + word.they + word.not + 
##     word.place + word.at + word.were + `word."` + word.we + `word.-` + 
##     word.be + word.as + word.like + word.me + word.out + word.there + 
##     user.average_stars + user.review_count + business.categories.Shopping + 
##     business.categories.total + business.review_count + business.stars + 
##     business.zipcode, family = poisson, data = trainC)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -3.806  -1.226  -0.441   0.427   7.544  
## 
## Coefficients:
##                                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                       9.55e+01   1.95e+01    4.90  9.8e-07 ***
## date                             -5.05e-04   4.83e-05  -10.46  < 2e-16 ***
## `word. `                          4.39e-03   9.55e-04    4.60  4.3e-06 ***
## word.the                         -4.74e-02   9.11e-03   -5.20  2.0e-07 ***
## `word.,`                         -1.51e-02   6.05e-03   -2.50  0.01241 *  
## word.a                           -2.05e-02   1.03e-02   -2.00  0.04575 *  
## `word.\\n`                        2.34e-02   5.05e-03    4.63  3.7e-06 ***
## word.of                           3.43e-02   1.42e-02    2.42  0.01548 *  
## word.for                          3.90e-02   1.89e-02    2.07  0.03891 *  
## word.it                          -4.18e-02   1.61e-02   -2.60  0.00930 ** 
## word.The                         -2.89e-02   1.82e-02   -1.58  0.11303    
## word.you                         -2.72e-02   1.53e-02   -1.78  0.07569 .  
## word.have                         5.59e-02   2.03e-02    2.76  0.00575 ** 
## word.are                          4.76e-02   2.66e-02    1.79  0.07392 .  
## word.they                        -3.57e-02   2.18e-02   -1.64  0.10143    
## word.not                         -4.88e-02   2.88e-02   -1.69  0.09080 .  
## word.place                       -6.77e-02   2.61e-02   -2.60  0.00944 ** 
## word.at                           1.10e-01   2.67e-02    4.11  4.0e-05 ***
## word.were                         4.20e-02   2.57e-02    1.64  0.10164    
## `word."`                          5.42e-02   1.14e-02    4.74  2.2e-06 ***
## word.we                          -1.10e-01   1.73e-02   -6.36  2.1e-10 ***
## `word.-`                         -2.91e-02   1.62e-02   -1.79  0.07365 .  
## word.be                           1.13e-01   3.19e-02    3.53  0.00042 ***
## word.as                          -9.56e-02   2.78e-02   -3.44  0.00059 ***
## word.like                        -4.87e-02   3.37e-02   -1.45  0.14784    
## word.me                           7.04e-02   2.36e-02    2.98  0.00287 ** 
## word.out                          6.99e-02   2.99e-02    2.34  0.01950 *  
## word.there                       -8.44e-02   3.30e-02   -2.56  0.01052 *  
## user.average_stars                1.65e-01   5.84e-02    2.82  0.00476 ** 
## user.review_count                 1.19e-03   7.92e-05   15.00  < 2e-16 ***
## business.categories.ShoppingTRUE -2.56e-01   1.38e-01   -1.86  0.06254 .  
## business.categories.total         4.82e-02   2.15e-02    2.24  0.02513 *  
## business.review_count             4.21e-04   1.67e-04    2.53  0.01157 *  
## business.stars                    1.53e-01   5.38e-02    2.84  0.00449 ** 
## business.zipcode                 -1.05e-03   2.29e-04   -4.59  4.4e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 2751.5  on 999  degrees of freedom
## Residual deviance: 1676.9  on 965  degrees of freedom
## AIC: 3286
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(fit.pois.bic)
```

```
## 
## Call:
## glm(formula = votes.useful ~ date + `word. ` + word.the + `word.\n` + 
##     word.of + word.have + word.place + word.at + `word."` + word.we + 
##     word.be + word.as + word.me + user.average_stars + user.review_count + 
##     business.review_count + business.stars + business.zipcode, 
##     family = poisson, data = trainC)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -3.596  -1.251  -0.420   0.443   7.404  
## 
## Coefficients:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            8.88e+01   1.90e+01    4.67  3.0e-06 ***
## date                  -4.84e-04   4.74e-05  -10.21  < 2e-16 ***
## `word. `               1.89e-03   5.88e-04    3.22  0.00129 ** 
## word.the              -3.91e-02   8.37e-03   -4.67  3.0e-06 ***
## `word.\\n`             2.03e-02   4.62e-03    4.39  1.1e-05 ***
## word.of                4.11e-02   1.37e-02    3.00  0.00270 ** 
## word.have              7.46e-02   1.89e-02    3.95  7.7e-05 ***
## word.place            -7.21e-02   2.45e-02   -2.94  0.00327 ** 
## word.at                1.03e-01   2.59e-02    3.97  7.1e-05 ***
## `word."`               4.93e-02   1.06e-02    4.63  3.6e-06 ***
## word.we               -8.59e-02   1.54e-02   -5.58  2.4e-08 ***
## word.be                1.05e-01   2.93e-02    3.59  0.00033 ***
## word.as               -1.09e-01   2.71e-02   -4.03  5.6e-05 ***
## word.me                7.02e-02   2.04e-02    3.44  0.00057 ***
## user.average_stars     1.67e-01   5.79e-02    2.87  0.00404 ** 
## user.review_count      1.19e-03   7.47e-05   15.90  < 2e-16 ***
## business.review_count  5.50e-04   1.58e-04    3.47  0.00051 ***
## business.stars         1.85e-01   5.30e-02    3.49  0.00049 ***
## business.zipcode      -9.77e-04   2.23e-04   -4.38  1.2e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 2751.5  on 999  degrees of freedom
## Residual deviance: 1732.0  on 981  degrees of freedom
## AIC: 3309
## 
## Number of Fisher Scoring iterations: 5
```

```r

## Determine the folds
seed <- 20130501
K <- 10
R <- 5
set.seed(seed)
folds <- cvFolds(nrow(trainC), K = K, R = R)

## Run the cross-validation
cv.pois <- cvFit(fit.pois, y = trainC$votes.useful, data = trainC, cost = rmspe, 
    costArgs = list(includeSE = TRUE), folds = folds)
cv.pois.aic <- cvFit(fit.pois.aic, y = trainC$votes.useful, data = trainC, cost = rmspe, 
    costArgs = list(includeSE = TRUE), folds = folds)
cv.pois.bic <- cvFit(fit.pois.bic, y = trainC$votes.useful, data = trainC, cost = rmspe, 
    costArgs = list(includeSE = TRUE), folds = folds)

cv.sel <- cvSelect(pois = cv.pois, pois.aic = cv.pois.aic, pois.bic = cv.pois.bic)
cv.sel
```

```
## 
## 10-fold CV results:
##        Fit    CV
## 1     pois 2.727
## 2 pois.aic 2.693
## 3 pois.bic 2.682
## 
## Best model:
##         CV 
## "pois.bic"
```

```r
cv.sel$se
```

```
##        Fit       CV
## 1     pois 0.005491
## 2 pois.aic 0.006879
## 3 pois.bic 0.003670
```

```r
cv.sel$reps
```

```
##         Fit    CV
## 1      pois 2.719
## 2      pois 2.733
## 3      pois 2.726
## 4      pois 2.730
## 5      pois 2.727
## 6  pois.aic 2.690
## 7  pois.aic 2.704
## 8  pois.aic 2.686
## 9  pois.aic 2.693
## 10 pois.aic 2.691
## 11 pois.bic 2.677
## 12 pois.bic 2.687
## 13 pois.bic 2.681
## 14 pois.bic 2.684
## 15 pois.bic 2.680
```

```r

## Evaluate with validation data set
e.pois <- rmspe(validateC$votes.useful, predict(fit.pois, validateC), includeSE = TRUE)
e.pois.aic <- rmspe(validateC$votes.useful, predict(fit.pois.aic, validateC), 
    includeSE = TRUE)
e.pois.bic <- rmspe(validateC$votes.useful, predict(fit.pois.bic, validateC), 
    includeSE = TRUE)
e.glm <- list(e.pois, e.pois.aic, e.pois.bic)

unlist(lapply(e.glm, function(x) {
    x$rmspe
}))
```

```
## [1] 2.466 2.467 2.453
```

```r
unlist(lapply(e.glm, function(x) {
    x$se
}))
```

```
## [1] 0.03894 0.03936 0.03921
```

```r

rm(validateC, trainC, test, seed, K, R)
save.image(file = "glm-pois.Rdata")
```


Reproducibility

```r
print(proc.time())
```

```
##    user  system elapsed 
## 217.171   3.178 220.815
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


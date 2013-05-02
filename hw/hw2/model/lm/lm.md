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

test <- FALSE
if (test) {
    train <- train[1:1000, ]
}
train <- train[, !colnames(train) %in% c("user_id", "business_id", "review_id", 
    "user.name", "business.city")]

## The actual function used by Yelp. Careful that this only works when
## using positive values (aka, it won't work for lm)
rmlspe <- function(y, yHat, includeSE = FALSE) {
    rmspe(log(y + 1), log(yHat + 1), includeSE = includeSE)
}

validateC <- validate[complete.cases(validate), ]
trainC <- train[complete.cases(train), ]
save(trainC, validateC, file = "trainC.Rdata")
```



Fit 10-fold cross validation with 5 repetitions

```r
## Fit the models
fit.lm <- lm(votes.useful ~ ., data = trainC)
fit.aic <- step(fit.lm, trace = 0)
fit.bic <- step(fit.lm, trace = 0, k = log(nrow(trainC)))
summary(fit.lm)
```

```
## 
## Call:
## lm(formula = votes.useful ~ ., data = trainC)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -14.29  -0.92  -0.32   0.54  81.11 
## 
## Coefficients:
##                                      Estimate Std. Error t value Pr(>|t|)
## (Intercept)                         -5.26e+01   5.13e+00  -10.26  < 2e-16
## stars                               -7.20e-02   5.72e-03  -12.59  < 2e-16
## date                                -6.00e-04   1.06e-05  -56.61  < 2e-16
## `word. `                             4.12e-03   3.46e-04   11.90  < 2e-16
## word..                               9.23e-03   9.51e-04    9.71  < 2e-16
## word.the                            -2.02e-02   2.10e-03   -9.62  < 2e-16
## `word.,`                             1.99e-02   1.41e-03   14.13  < 2e-16
## word.and                             6.16e-03   2.54e-03    2.43  0.01520
## word.I                               1.93e-02   2.24e-03    8.62  < 2e-16
## word.a                              -2.46e-03   2.79e-03   -0.88  0.37737
## `word.\\n`                           6.40e-02   1.51e-03   42.26  < 2e-16
## word.to                             -2.13e-03   2.99e-03   -0.71  0.47698
## word.of                              2.20e-02   3.50e-03    6.27  3.7e-10
## word.was                            -3.77e-02   3.22e-03  -11.73  < 2e-16
## word.is                             -1.96e-02   3.73e-03   -5.25  1.5e-07
## word.for                            -1.78e-02   4.15e-03   -4.29  1.8e-05
## word.it                             -2.00e-02   3.84e-03   -5.20  2.0e-07
## word.in                              2.77e-03   4.20e-03    0.66  0.51015
## `word.!`                             3.87e-02   2.26e-03   17.09  < 2e-16
## word.The                            -2.58e-02   4.51e-03   -5.72  1.0e-08
## word.that                            5.71e-03   4.31e-03    1.32  0.18521
## word.with                           -8.76e-03   4.70e-03   -1.87  0.06215
## word.but                            -7.04e-02   5.34e-03  -13.18  < 2e-16
## word.you                             2.22e-02   4.15e-03    5.35  9.1e-08
## word.my                              1.47e-02   4.84e-03    3.04  0.00236
## word.on                             -6.59e-03   5.31e-03   -1.24  0.21435
## word.have                           -2.64e-02   5.22e-03   -5.05  4.3e-07
## word.this                            2.12e-03   5.77e-03    0.37  0.71389
## word.had                            -3.65e-02   5.61e-03   -6.49  8.4e-11
## word.are                            -4.03e-02   5.75e-03   -7.00  2.5e-12
## word.they                           -2.92e-02   5.47e-03   -5.34  9.2e-08
## word.not                            -3.22e-02   5.81e-03   -5.54  3.1e-08
## `word.)`                             8.15e-02   1.17e-02    6.99  2.9e-12
## word.place                          -3.70e-02   6.22e-03   -5.95  2.7e-09
## word.at                              9.54e-03   6.12e-03    1.56  0.11873
## word.good                           -6.67e-02   5.92e-03  -11.26  < 2e-16
## word.were                           -1.06e-02   6.06e-03   -1.75  0.07992
## `word.(`                            -8.54e-02   1.28e-02   -6.69  2.2e-11
## word.food                           -1.39e-02   6.11e-03   -2.28  0.02286
## `word."`                             2.63e-02   3.54e-03    7.42  1.2e-13
## word.we                             -9.92e-03   4.97e-03   -2.00  0.04578
## word.so                              6.42e-03   6.54e-03    0.98  0.32654
## `word.-`                            -1.44e-02   2.78e-03   -5.17  2.3e-07
## word.be                              1.47e-02   6.94e-03    2.12  0.03432
## word.as                             -9.78e-03   5.93e-03   -1.65  0.09924
## word.like                            9.82e-04   6.87e-03    0.14  0.88638
## word.me                              3.98e-02   6.82e-03    5.83  5.5e-09
## word.out                            -2.99e-02   7.43e-03   -4.02  5.9e-05
## word.there                          -4.32e-02   7.13e-03   -6.06  1.4e-09
## word.here                           -2.56e-02   7.19e-03   -3.56  0.00037
## word.just                           -1.29e-02   7.51e-03   -1.72  0.08608
## word.great                          -3.98e-02   7.21e-03   -5.52  3.3e-08
## word.all                             1.15e-02   7.71e-03    1.49  0.13677
## user.average_stars                   1.01e-01   1.02e-02    9.89  < 2e-16
## user.review_count                    2.75e-03   2.65e-05  103.86  < 2e-16
## business.categories.BeautySpasTRUE  -1.00e-01   3.89e-02   -2.57  0.01005
## business.categories.FoodTRUE        -3.56e-02   1.64e-02   -2.17  0.03016
## business.categories.RestaurantsTRUE -3.18e-02   1.55e-02   -2.05  0.03989
## business.categories.ShoppingTRUE    -6.86e-02   2.54e-02   -2.70  0.00695
## business.categories.total            7.53e-03   4.50e-03    1.67  0.09431
## business.latitude                    1.04e-01   5.24e-02    1.99  0.04649
## business.longitude                  -5.98e-01   4.62e-02  -12.94  < 2e-16
## business.openTRUE                   -2.42e-02   2.20e-02   -1.10  0.27039
## business.review_count                4.79e-04   4.73e-05   10.13  < 2e-16
## business.stars                       1.18e-01   1.10e-02   10.76  < 2e-16
## business.zipcode                    -1.05e-04   1.63e-05   -6.44  1.2e-10
## checkin.total                       -4.45e-05   4.31e-06  -10.33  < 2e-16
##                                        
## (Intercept)                         ***
## stars                               ***
## date                                ***
## `word. `                            ***
## word..                              ***
## word.the                            ***
## `word.,`                            ***
## word.and                            *  
## word.I                              ***
## word.a                                 
## `word.\\n`                          ***
## word.to                                
## word.of                             ***
## word.was                            ***
## word.is                             ***
## word.for                            ***
## word.it                             ***
## word.in                                
## `word.!`                            ***
## word.The                            ***
## word.that                              
## word.with                           .  
## word.but                            ***
## word.you                            ***
## word.my                             ** 
## word.on                                
## word.have                           ***
## word.this                              
## word.had                            ***
## word.are                            ***
## word.they                           ***
## word.not                            ***
## `word.)`                            ***
## word.place                          ***
## word.at                                
## word.good                           ***
## word.were                           .  
## `word.(`                            ***
## word.food                           *  
## `word."`                            ***
## word.we                             *  
## word.so                                
## `word.-`                            ***
## word.be                             *  
## word.as                             .  
## word.like                              
## word.me                             ***
## word.out                            ***
## word.there                          ***
## word.here                           ***
## word.just                           .  
## word.great                          ***
## word.all                               
## user.average_stars                  ***
## user.review_count                   ***
## business.categories.BeautySpasTRUE  *  
## business.categories.FoodTRUE        *  
## business.categories.RestaurantsTRUE *  
## business.categories.ShoppingTRUE    ** 
## business.categories.total           .  
## business.latitude                   *  
## business.longitude                  ***
## business.openTRUE                      
## business.review_count               ***
## business.stars                      ***
## business.zipcode                    ***
## checkin.total                       ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.95 on 140479 degrees of freedom
## Multiple R-squared:  0.23,	Adjusted R-squared:  0.23 
## F-statistic:  636 on 66 and 140479 DF,  p-value: <2e-16
```

```r
summary(fit.aic)
```

```
## 
## Call:
## lm(formula = votes.useful ~ stars + date + `word. ` + word.. + 
##     word.the + `word.,` + word.and + word.I + `word.\n` + word.of + 
##     word.was + word.is + word.for + word.it + `word.!` + word.The + 
##     word.with + word.but + word.you + word.my + word.have + word.had + 
##     word.are + word.they + word.not + `word.)` + word.place + 
##     word.at + word.good + word.were + `word.(` + word.food + 
##     `word."` + word.we + `word.-` + word.be + word.as + word.me + 
##     word.out + word.there + word.here + word.just + word.great + 
##     word.all + user.average_stars + user.review_count + business.categories.BeautySpas + 
##     business.categories.Food + business.categories.Restaurants + 
##     business.categories.Shopping + business.categories.total + 
##     business.latitude + business.longitude + business.review_count + 
##     business.stars + business.zipcode + checkin.total, data = trainC)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -14.29  -0.92  -0.32   0.54  81.10 
## 
## Coefficients:
##                                      Estimate Std. Error t value Pr(>|t|)
## (Intercept)                         -5.25e+01   5.13e+00  -10.25  < 2e-16
## stars                               -7.19e-02   5.71e-03  -12.61  < 2e-16
## date                                -6.02e-04   1.05e-05  -57.11  < 2e-16
## `word. `                             4.03e-03   2.77e-04   14.53  < 2e-16
## word..                               9.25e-03   9.45e-04    9.78  < 2e-16
## word.the                            -2.01e-02   2.04e-03   -9.82  < 2e-16
## `word.,`                             1.99e-02   1.40e-03   14.25  < 2e-16
## word.and                             5.95e-03   2.50e-03    2.37  0.01756
## word.I                               1.99e-02   2.21e-03    9.00  < 2e-16
## `word.\\n`                           6.40e-02   1.51e-03   42.27  < 2e-16
## word.of                              2.20e-02   3.46e-03    6.36  2.0e-10
## word.was                            -3.74e-02   3.19e-03  -11.70  < 2e-16
## word.is                             -1.88e-02   3.69e-03   -5.08  3.7e-07
## word.for                            -1.82e-02   4.11e-03   -4.43  9.6e-06
## word.it                             -1.96e-02   3.81e-03   -5.13  2.9e-07
## `word.!`                             3.89e-02   2.25e-03   17.28  < 2e-16
## word.The                            -2.61e-02   4.47e-03   -5.85  5.0e-09
## word.with                           -8.98e-03   4.64e-03   -1.94  0.05263
## word.but                            -7.06e-02   5.32e-03  -13.27  < 2e-16
## word.you                             2.25e-02   4.14e-03    5.44  5.4e-08
## word.my                              1.47e-02   4.81e-03    3.07  0.00216
## word.have                           -2.65e-02   5.20e-03   -5.09  3.5e-07
## word.had                            -3.64e-02   5.60e-03   -6.50  8.2e-11
## word.are                            -3.97e-02   5.74e-03   -6.92  4.6e-12
## word.they                           -2.84e-02   5.44e-03   -5.23  1.7e-07
## word.not                            -3.16e-02   5.80e-03   -5.45  5.1e-08
## `word.)`                             8.15e-02   1.17e-02    6.98  3.0e-12
## word.place                          -3.64e-02   5.84e-03   -6.24  4.5e-10
## word.at                              9.46e-03   6.10e-03    1.55  0.12123
## word.good                           -6.68e-02   5.89e-03  -11.33  < 2e-16
## word.were                           -1.02e-02   6.05e-03   -1.69  0.09153
## `word.(`                            -8.52e-02   1.28e-02   -6.68  2.4e-11
## word.food                           -1.32e-02   6.08e-03   -2.16  0.03048
## `word."`                             2.65e-02   3.54e-03    7.48  7.3e-14
## word.we                             -9.19e-03   4.92e-03   -1.87  0.06198
## `word.-`                            -1.43e-02   2.77e-03   -5.16  2.4e-07
## word.be                              1.48e-02   6.89e-03    2.15  0.03150
## word.as                             -9.82e-03   5.92e-03   -1.66  0.09729
## word.me                              4.03e-02   6.78e-03    5.94  2.9e-09
## word.out                            -3.00e-02   7.42e-03   -4.04  5.4e-05
## word.there                          -4.29e-02   7.12e-03   -6.02  1.7e-09
## word.here                           -2.53e-02   7.17e-03   -3.53  0.00042
## word.just                           -1.22e-02   7.49e-03   -1.63  0.10400
## word.great                          -4.07e-02   7.18e-03   -5.67  1.4e-08
## word.all                             1.24e-02   7.69e-03    1.61  0.10661
## user.average_stars                   1.01e-01   1.02e-02    9.89  < 2e-16
## user.review_count                    2.75e-03   2.64e-05  103.94  < 2e-16
## business.categories.BeautySpasTRUE  -1.01e-01   3.88e-02   -2.61  0.00909
## business.categories.FoodTRUE        -3.41e-02   1.64e-02   -2.08  0.03748
## business.categories.RestaurantsTRUE -3.06e-02   1.54e-02   -1.99  0.04670
## business.categories.ShoppingTRUE    -6.74e-02   2.54e-02   -2.65  0.00793
## business.categories.total            6.98e-03   4.49e-03    1.55  0.12007
## business.latitude                    1.05e-01   5.24e-02    2.01  0.04489
## business.longitude                  -5.97e-01   4.62e-02  -12.93  < 2e-16
## business.review_count                4.74e-04   4.70e-05   10.07  < 2e-16
## business.stars                       1.18e-01   1.09e-02   10.76  < 2e-16
## business.zipcode                    -1.05e-04   1.63e-05   -6.44  1.2e-10
## checkin.total                       -4.45e-05   4.31e-06  -10.34  < 2e-16
##                                        
## (Intercept)                         ***
## stars                               ***
## date                                ***
## `word. `                            ***
## word..                              ***
## word.the                            ***
## `word.,`                            ***
## word.and                            *  
## word.I                              ***
## `word.\\n`                          ***
## word.of                             ***
## word.was                            ***
## word.is                             ***
## word.for                            ***
## word.it                             ***
## `word.!`                            ***
## word.The                            ***
## word.with                           .  
## word.but                            ***
## word.you                            ***
## word.my                             ** 
## word.have                           ***
## word.had                            ***
## word.are                            ***
## word.they                           ***
## word.not                            ***
## `word.)`                            ***
## word.place                          ***
## word.at                                
## word.good                           ***
## word.were                           .  
## `word.(`                            ***
## word.food                           *  
## `word."`                            ***
## word.we                             .  
## `word.-`                            ***
## word.be                             *  
## word.as                             .  
## word.me                             ***
## word.out                            ***
## word.there                          ***
## word.here                           ***
## word.just                              
## word.great                          ***
## word.all                               
## user.average_stars                  ***
## user.review_count                   ***
## business.categories.BeautySpasTRUE  ** 
## business.categories.FoodTRUE        *  
## business.categories.RestaurantsTRUE *  
## business.categories.ShoppingTRUE    ** 
## business.categories.total              
## business.latitude                   *  
## business.longitude                  ***
## business.review_count               ***
## business.stars                      ***
## business.zipcode                    ***
## checkin.total                       ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.95 on 140488 degrees of freedom
## Multiple R-squared:  0.23,	Adjusted R-squared:  0.23 
## F-statistic:  736 on 57 and 140488 DF,  p-value: <2e-16
```

```r
summary(fit.bic)
```

```
## 
## Call:
## lm(formula = votes.useful ~ stars + date + `word. ` + word.. + 
##     word.the + `word.,` + word.I + `word.\n` + word.of + word.was + 
##     word.is + word.for + word.it + `word.!` + word.The + word.but + 
##     word.you + word.have + word.had + word.are + word.they + 
##     word.not + `word.)` + word.place + word.good + `word.(` + 
##     `word."` + `word.-` + word.me + word.out + word.there + word.here + 
##     word.great + user.average_stars + user.review_count + business.longitude + 
##     business.review_count + business.stars + business.zipcode + 
##     checkin.total, data = trainC)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -14.30  -0.92  -0.33   0.54  81.08 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           -5.30e+01   5.12e+00  -10.36  < 2e-16 ***
## stars                 -7.07e-02   5.65e-03  -12.50  < 2e-16 ***
## date                  -6.03e-04   1.05e-05  -57.33  < 2e-16 ***
## `word. `               4.16e-03   2.18e-04   19.06  < 2e-16 ***
## word..                 9.10e-03   9.40e-04    9.69  < 2e-16 ***
## word.the              -2.02e-02   2.02e-03   -9.97  < 2e-16 ***
## `word.,`               1.99e-02   1.39e-03   14.26  < 2e-16 ***
## word.I                 2.26e-02   2.04e-03   11.06  < 2e-16 ***
## `word.\\n`             6.40e-02   1.51e-03   42.40  < 2e-16 ***
## word.of                2.20e-02   3.41e-03    6.44  1.2e-10 ***
## word.was              -3.83e-02   3.16e-03  -12.12  < 2e-16 ***
## word.is               -1.78e-02   3.59e-03   -4.96  7.1e-07 ***
## word.for              -1.85e-02   4.09e-03   -4.53  5.8e-06 ***
## word.it               -2.00e-02   3.78e-03   -5.28  1.3e-07 ***
## `word.!`               3.88e-02   2.24e-03   17.32  < 2e-16 ***
## word.The              -2.79e-02   4.38e-03   -6.36  2.0e-10 ***
## word.but              -7.29e-02   5.27e-03  -13.84  < 2e-16 ***
## word.you               2.34e-02   4.04e-03    5.79  7.2e-09 ***
## word.have             -2.66e-02   5.18e-03   -5.14  2.7e-07 ***
## word.had              -3.85e-02   5.54e-03   -6.95  3.6e-12 ***
## word.are              -3.70e-02   5.67e-03   -6.52  7.0e-11 ***
## word.they             -2.99e-02   5.38e-03   -5.56  2.8e-08 ***
## word.not              -3.28e-02   5.78e-03   -5.67  1.5e-08 ***
## `word.)`               8.20e-02   1.17e-02    7.03  2.1e-12 ***
## word.place            -3.76e-02   5.80e-03   -6.48  9.5e-11 ***
## word.good             -7.01e-02   5.81e-03  -12.05  < 2e-16 ***
## `word.(`              -8.57e-02   1.28e-02   -6.72  1.8e-11 ***
## `word."`               2.68e-02   3.53e-03    7.58  3.5e-14 ***
## `word.-`              -1.42e-02   2.77e-03   -5.15  2.7e-07 ***
## word.me                4.67e-02   6.60e-03    7.07  1.5e-12 ***
## word.out              -2.98e-02   7.40e-03   -4.03  5.7e-05 ***
## word.there            -4.36e-02   7.06e-03   -6.18  6.5e-10 ***
## word.here             -2.58e-02   7.16e-03   -3.60  0.00032 ***
## word.great            -4.07e-02   7.15e-03   -5.69  1.3e-08 ***
## user.average_stars     1.00e-01   1.02e-02    9.86  < 2e-16 ***
## user.review_count      2.75e-03   2.63e-05  104.62  < 2e-16 ***
## business.longitude    -6.34e-01   4.26e-02  -14.87  < 2e-16 ***
## business.review_count  4.62e-04   4.42e-05   10.44  < 2e-16 ***
## business.stars         1.17e-01   1.08e-02   10.89  < 2e-16 ***
## business.zipcode      -1.06e-04   1.63e-05   -6.51  7.7e-11 ***
## checkin.total         -4.28e-05   4.19e-06  -10.20  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.95 on 140505 degrees of freedom
## Multiple R-squared:  0.23,	Adjusted R-squared:  0.229 
## F-statistic: 1.05e+03 on 40 and 140505 DF,  p-value: <2e-16
```

```r

## Determine the folds
seed <- 20130501
K <- 10
R <- 5
set.seed(seed)
folds <- cvFolds(nrow(trainC), K = K, R = R)

## Run the cross-validation
cv.lm <- cvFit(fit.lm, y = trainC$votes.useful, data = trainC, cost = rmspe, 
    costArgs = list(includeSE = TRUE), folds = folds)
cv.aic <- cvFit(fit.aic, y = trainC$votes.useful, data = trainC, cost = rmspe, 
    costArgs = list(includeSE = TRUE), folds = folds)
cv.bic <- cvFit(fit.bic, y = trainC$votes.useful, data = trainC, cost = rmspe, 
    costArgs = list(includeSE = TRUE), folds = folds)

cv.sel <- cvSelect(lm = cv.lm, aic = cv.aic, bic = cv.bic)
cv.sel
```

```
## 
## 10-fold CV results:
##   Fit    CV
## 1  lm 1.948
## 2 aic 1.947
## 3 bic 1.947
## 
## Best model:
##    CV 
## "aic"
```

```r
cv.sel$se
```

```
##   Fit        CV
## 1  lm 0.0001481
## 2 aic 0.0001353
## 3 bic 0.0001120
```

```r
cv.sel$reps
```

```
##    Fit    CV
## 1   lm 1.948
## 2   lm 1.948
## 3   lm 1.947
## 4   lm 1.948
## 5   lm 1.948
## 6  aic 1.947
## 7  aic 1.947
## 8  aic 1.947
## 9  aic 1.947
## 10 aic 1.947
## 11 bic 1.947
## 12 bic 1.948
## 13 bic 1.947
## 14 bic 1.947
## 15 bic 1.948
```

```r

e.lm <- rmspe(validateC$votes.useful, predict(fit.lm, validateC), includeSE = TRUE)
e.aic <- rmspe(validateC$votes.useful, predict(fit.aic, validateC), includeSE = TRUE)
e.bic <- rmspe(validateC$votes.useful, predict(fit.bic, validateC), includeSE = TRUE)
e.lm
```

```
## $rmspe
## [1] 1.978
## 
## $se
## [1] 0.04313
```

```r
e.aic
```

```
## $rmspe
## [1] 1.978
## 
## $se
## [1] 0.04313
```

```r
e.bic
```

```
## $rmspe
## [1] 1.978
## 
## $se
## [1] 0.04313
```

```r

rm(validate, validateC, train, trainC, test, seed, K, R)
save.image(file = "lm.Rdata")
```


Reproducibility

```r
print(proc.time())
```

```
##    user  system elapsed 
##  5401.9   236.4  5643.4
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


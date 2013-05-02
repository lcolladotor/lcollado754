glm with cross-validation
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

```r

## Data
load("../lm/trainC.Rdata")

test <- TRUE
if (test) {
    trainC <- trainC[1:100, ]
}
```



Fit 10-fold cross validation with 5 repetitions

```r
## Fit binomial the models
fit.bin <- glm(votes.useful ~ ., data = trainC)
fit.bin.aic <- step(fit.bin, trace = 0)
fit.bin.bic <- step(fit.bin, trace = 0, k = log(nrow(trainC)))
summary(fit.bin)
```

```
## 
## Call:
## glm(formula = votes.useful ~ ., data = trainC)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -3.659  -0.701  -0.042   0.669   3.050  
## 
## Coefficients:
##                                      Estimate Std. Error t value Pr(>|t|)
## (Intercept)                          1.00e+02   4.10e+02    0.24   0.8083
## stars                                8.95e-01   4.19e-01    2.14   0.0402
## date                                -1.85e-03   7.82e-04   -2.37   0.0240
## `word. `                            -1.13e-03   2.92e-02   -0.04   0.9694
## word..                              -4.74e-02   1.09e-01   -0.44   0.6656
## word.the                            -8.05e-02   1.85e-01   -0.43   0.6667
## `word.,`                            -1.14e-01   9.07e-02   -1.25   0.2190
## word.and                             1.29e-01   1.95e-01    0.66   0.5126
## word.I                              -3.07e-01   1.74e-01   -1.76   0.0869
## word.a                              -1.87e-01   1.95e-01   -0.96   0.3442
## `word.\\n`                           5.97e-02   1.23e-01    0.48   0.6319
## word.to                             -6.42e-01   2.54e-01   -2.53   0.0165
## word.of                              2.50e-02   2.56e-01    0.10   0.9229
## word.was                            -2.94e-01   2.47e-01   -1.19   0.2436
## word.is                             -3.02e-01   3.05e-01   -0.99   0.3281
## word.for                             2.75e-01   3.94e-01    0.70   0.4900
## word.it                              6.42e-02   2.55e-01    0.25   0.8030
## word.in                              2.46e-01   2.86e-01    0.86   0.3962
## `word.!`                             1.29e-01   1.65e-01    0.78   0.4394
## word.The                             1.89e-01   3.19e-01    0.59   0.5579
## word.that                            4.38e-01   3.76e-01    1.16   0.2531
## word.with                            3.50e-02   4.11e-01    0.09   0.9326
## word.but                             3.61e-01   3.78e-01    0.96   0.3463
## word.you                            -1.10e+00   4.04e-01   -2.73   0.0100
## word.my                              2.07e-01   4.35e-01    0.48   0.6372
## word.on                              1.36e+00   4.08e-01    3.32   0.0022
## word.have                            8.83e-01   4.23e-01    2.09   0.0449
## word.this                           -1.70e-01   5.50e-01   -0.31   0.7592
## word.had                             3.59e-01   4.79e-01    0.75   0.4585
## word.are                             5.23e-01   5.09e-01    1.03   0.3111
## word.they                           -3.39e-01   4.53e-01   -0.75   0.4598
## word.not                             6.79e-01   5.03e-01    1.35   0.1864
## `word.)`                            -5.80e-02   1.34e+00   -0.04   0.9658
## word.place                          -4.50e-01   4.80e-01   -0.94   0.3556
## word.at                              3.71e-01   4.31e-01    0.86   0.3953
## word.good                           -3.99e-01   5.47e-01   -0.73   0.4708
## word.were                            9.44e-02   4.38e-01    0.22   0.8305
## `word.(`                             3.68e-01   1.59e+00    0.23   0.8183
## word.food                            1.32e-01   5.01e-01    0.26   0.7932
## `word."`                             1.09e-01   3.80e-01    0.29   0.7766
## word.we                             -1.79e-01   3.62e-01   -0.49   0.6250
## word.so                              4.79e-01   6.05e-01    0.79   0.4335
## `word.-`                            -3.25e-01   5.11e-01   -0.64   0.5293
## word.be                              5.65e-01   4.67e-01    1.21   0.2347
## word.as                              4.96e-01   4.37e-01    1.13   0.2653
## word.like                            4.91e-01   5.96e-01    0.82   0.4162
## word.me                              1.55e+00   5.48e-01    2.82   0.0081
## word.out                             1.15e-01   5.63e-01    0.20   0.8392
## word.there                          -8.28e-01   5.81e-01   -1.43   0.1634
## word.here                           -2.52e-01   6.00e-01   -0.42   0.6775
## word.just                           -7.35e-01   6.91e-01   -1.06   0.2954
## word.great                          -7.74e-01   6.07e-01   -1.27   0.2112
## word.all                             6.76e-01   8.48e-01    0.80   0.4312
## user.average_stars                  -6.75e-01   6.18e-01   -1.09   0.2827
## user.review_count                    4.73e-03   2.55e-03    1.85   0.0729
## business.categories.BeautySpasTRUE  -1.26e+00   2.42e+00   -0.52   0.6063
## business.categories.FoodTRUE        -2.52e+00   1.22e+00   -2.07   0.0462
## business.categories.RestaurantsTRUE -3.40e-01   8.41e-01   -0.40   0.6888
## business.categories.ShoppingTRUE     3.15e-01   1.38e+00    0.23   0.8213
## business.categories.total            1.76e-01   2.85e-01    0.62   0.5401
## business.latitude                    1.74e+00   4.25e+00    0.41   0.6852
## business.longitude                  -4.43e-01   3.11e+00   -0.14   0.8876
## business.openTRUE                    9.25e-01   9.94e-01    0.93   0.3588
## business.review_count                1.07e-03   2.89e-03    0.37   0.7133
## business.stars                      -3.53e-01   6.29e-01   -0.56   0.5784
## business.zipcode                    -2.11e-03   2.62e-03   -0.81   0.4264
## checkin.total                       -2.86e-05   1.73e-04   -0.17   0.8699
##                                       
## (Intercept)                           
## stars                               * 
## date                                * 
## `word. `                              
## word..                                
## word.the                              
## `word.,`                              
## word.and                              
## word.I                              . 
## word.a                                
## `word.\\n`                            
## word.to                             * 
## word.of                               
## word.was                              
## word.is                               
## word.for                              
## word.it                               
## word.in                               
## `word.!`                              
## word.The                              
## word.that                             
## word.with                             
## word.but                              
## word.you                            * 
## word.my                               
## word.on                             **
## word.have                           * 
## word.this                             
## word.had                              
## word.are                              
## word.they                             
## word.not                              
## `word.)`                              
## word.place                            
## word.at                               
## word.good                             
## word.were                             
## `word.(`                              
## word.food                             
## `word."`                              
## word.we                               
## word.so                               
## `word.-`                              
## word.be                               
## word.as                               
## word.like                             
## word.me                             **
## word.out                              
## word.there                            
## word.here                             
## word.just                             
## word.great                            
## word.all                              
## user.average_stars                    
## user.review_count                   . 
## business.categories.BeautySpasTRUE    
## business.categories.FoodTRUE        * 
## business.categories.RestaurantsTRUE   
## business.categories.ShoppingTRUE      
## business.categories.total             
## business.latitude                     
## business.longitude                    
## business.openTRUE                     
## business.review_count                 
## business.stars                        
## business.zipcode                      
## checkin.total                         
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## (Dispersion parameter for gaussian family taken to be 3.936)
## 
##     Null deviance: 770.36  on 99  degrees of freedom
## Residual deviance: 129.88  on 33  degrees of freedom
## AIC: 445.9
## 
## Number of Fisher Scoring iterations: 2
```

```r
summary(fit.bin.aic)
```

```
## 
## Call:
## glm(formula = votes.useful ~ stars + date + `word.,` + word.and + 
##     word.I + word.a + `word.\n` + word.to + word.was + word.is + 
##     word.for + word.in + word.The + word.that + word.but + word.you + 
##     word.on + word.have + word.not + word.place + word.at + word.good + 
##     word.be + word.as + word.me + word.great + user.average_stars + 
##     user.review_count + business.categories.Food + business.open + 
##     business.stars + business.zipcode, data = trainC)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -2.983  -0.805   0.004   0.645   4.034  
## 
## Coefficients:
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   2.92e+02   1.19e+02    2.46  0.01649 *  
## stars                         9.25e-01   2.22e-01    4.17  8.9e-05 ***
## date                         -2.21e-03   4.28e-04   -5.16  2.4e-06 ***
## `word.,`                     -9.56e-02   4.64e-02   -2.06  0.04319 *  
## word.and                      1.85e-01   7.58e-02    2.44  0.01746 *  
## word.I                       -4.03e-01   8.41e-02   -4.80  9.3e-06 ***
## word.a                       -2.51e-01   9.66e-02   -2.60  0.01156 *  
## `word.\\n`                    8.18e-02   5.79e-02    1.41  0.16201    
## word.to                      -7.14e-01   1.06e-01   -6.71  5.0e-09 ***
## word.was                     -2.13e-01   1.00e-01   -2.12  0.03776 *  
## word.is                      -3.12e-01   1.45e-01   -2.14  0.03583 *  
## word.for                      3.73e-01   1.79e-01    2.09  0.04064 *  
## word.in                       1.79e-01   1.47e-01    1.21  0.22956    
## word.The                      2.07e-01   1.75e-01    1.18  0.24041    
## word.that                     2.59e-01   1.89e-01    1.37  0.17466    
## word.but                      3.47e-01   2.06e-01    1.69  0.09654 .  
## word.you                     -9.97e-01   1.96e-01   -5.09  3.1e-06 ***
## word.on                       1.24e+00   2.11e-01    5.90  1.3e-07 ***
## word.have                     1.00e+00   2.02e-01    4.97  5.0e-06 ***
## word.not                      7.83e-01   2.32e-01    3.38  0.00122 ** 
## word.place                   -3.95e-01   2.31e-01   -1.71  0.09189 .  
## word.at                       5.49e-01   2.17e-01    2.53  0.01369 *  
## word.good                    -5.29e-01   3.42e-01   -1.55  0.12630    
## word.be                       4.89e-01   2.65e-01    1.85  0.06943 .  
## word.as                       7.17e-01   2.15e-01    3.33  0.00140 ** 
## word.me                       1.34e+00   3.06e-01    4.39  4.1e-05 ***
## word.great                   -6.26e-01   2.80e-01   -2.23  0.02879 *  
## user.average_stars           -4.80e-01   3.37e-01   -1.43  0.15862    
## user.review_count             5.12e-03   1.26e-03    4.05  0.00014 ***
## business.categories.FoodTRUE -2.58e+00   5.65e-01   -4.57  2.1e-05 ***
## business.openTRUE             8.85e-01   5.57e-01    1.59  0.11698    
## business.stars               -4.82e-01   3.63e-01   -1.33  0.18867    
## business.zipcode             -3.03e-03   1.39e-03   -2.18  0.03312 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## (Dispersion parameter for gaussian family taken to be 2.322)
## 
##     Null deviance: 770.36  on 99  degrees of freedom
## Residual deviance: 155.57  on 67  degrees of freedom
## AIC: 396
## 
## Number of Fisher Scoring iterations: 2
```

```r
summary(fit.bin.bic)
```

```
## 
## Call:
## glm(formula = votes.useful ~ stars + date + word.I + word.a + 
##     word.to + word.for + word.you + word.on + word.have + word.not + 
##     word.at + word.as + word.me + user.review_count + business.categories.Food + 
##     business.zipcode, data = trainC)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -3.503  -1.015   0.063   0.776   6.391  
## 
## Coefficients:
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   3.00e+02   1.21e+02    2.49  0.01491 *  
## stars                         5.63e-01   1.52e-01    3.70  0.00038 ***
## date                         -2.12e-03   4.14e-04   -5.11  2.0e-06 ***
## word.I                       -4.12e-01   7.63e-02   -5.40  6.3e-07 ***
## word.a                       -2.09e-01   8.23e-02   -2.54  0.01284 *  
## word.to                      -6.22e-01   9.10e-02   -6.84  1.3e-09 ***
## word.for                      4.69e-01   1.64e-01    2.87  0.00527 ** 
## word.you                     -9.60e-01   1.83e-01   -5.26  1.1e-06 ***
## word.on                       1.16e+00   1.91e-01    6.08  3.5e-08 ***
## word.have                     1.16e+00   1.90e-01    6.12  3.0e-08 ***
## word.not                      7.24e-01   2.35e-01    3.08  0.00281 ** 
## word.at                       4.48e-01   2.14e-01    2.09  0.03948 *  
## word.as                       7.61e-01   2.12e-01    3.60  0.00055 ***
## word.me                       1.47e+00   2.91e-01    5.06  2.5e-06 ***
## user.review_count             4.95e-03   1.26e-03    3.93  0.00017 ***
## business.categories.FoodTRUE -3.19e+00   5.49e-01   -5.81  1.1e-07 ***
## business.zipcode             -3.16e-03   1.41e-03   -2.24  0.02786 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## (Dispersion parameter for gaussian family taken to be 2.861)
## 
##     Null deviance: 770.36  on 99  degrees of freedom
## Residual deviance: 237.47  on 83  degrees of freedom
## AIC: 406.3
## 
## Number of Fisher Scoring iterations: 2
```

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
## -1.413  -0.496  -0.118   0.136   2.337  
## 
## Coefficients:
##                                      Estimate Std. Error z value Pr(>|z|)
## (Intercept)                         -4.53e+01   3.14e+02   -0.14   0.8854
## stars                                2.88e-01   4.52e-01    0.64   0.5235
## date                                -1.80e-03   7.33e-04   -2.46   0.0140
## `word. `                            -5.38e-02   2.75e-02   -1.95   0.0507
## word..                              -7.67e-02   1.13e-01   -0.68   0.4973
## word.the                             4.70e-01   2.35e-01    2.00   0.0459
## `word.,`                             1.25e-01   8.73e-02    1.43   0.1532
## word.and                             2.59e-01   1.70e-01    1.52   0.1278
## word.I                              -4.65e-01   1.81e-01   -2.57   0.0103
## word.a                              -1.52e-01   1.67e-01   -0.91   0.3632
## `word.\\n`                           5.72e-01   1.94e-01    2.94   0.0032
## word.to                              8.49e-02   2.30e-01    0.37   0.7115
## word.of                              4.53e-01   3.12e-01    1.45   0.1460
## word.was                            -1.32e-01   1.81e-01   -0.73   0.4634
## word.is                             -5.59e-01   3.29e-01   -1.70   0.0896
## word.for                             2.52e-01   3.60e-01    0.70   0.4840
## word.it                              7.28e-01   2.76e-01    2.64   0.0083
## word.in                             -2.95e-01   3.10e-01   -0.95   0.3403
## `word.!`                            -1.76e-01   1.52e-01   -1.16   0.2467
## word.The                             5.65e-01   4.16e-01    1.36   0.1743
## word.that                            2.91e-01   2.88e-01    1.01   0.3123
## word.with                           -2.07e-01   3.62e-01   -0.57   0.5673
## word.but                             2.47e-01   3.82e-01    0.65   0.5181
## word.you                            -4.49e-01   3.65e-01   -1.23   0.2183
## word.my                             -8.51e-02   4.43e-01   -0.19   0.8478
## word.on                              7.37e-01   3.74e-01    1.97   0.0487
## word.have                            2.52e-01   3.99e-01    0.63   0.5275
## word.this                            1.08e+00   6.52e-01    1.66   0.0971
## word.had                            -6.78e-01   6.02e-01   -1.13   0.2601
## word.are                            -4.63e-01   4.83e-01   -0.96   0.3372
## word.they                            7.05e-01   4.42e-01    1.59   0.1107
## word.not                             7.46e-01   4.55e-01    1.64   0.1013
## `word.)`                            -4.11e-01   8.90e-01   -0.46   0.6443
## word.place                           3.38e-01   5.24e-01    0.65   0.5185
## word.at                              6.13e-01   5.50e-01    1.12   0.2647
## word.good                           -3.73e-01   7.06e-01   -0.53   0.5971
## word.were                           -1.64e-01   3.69e-01   -0.44   0.6579
## `word.(`                             3.06e-02   1.07e+00    0.03   0.9771
## word.food                           -7.66e-01   6.47e-01   -1.18   0.2364
## `word."`                            -7.56e-01   4.35e-01   -1.74   0.0817
## word.we                              4.78e-02   3.49e-01    0.14   0.8911
## word.so                              7.16e-02   4.46e-01    0.16   0.8724
## `word.-`                             8.39e-01   5.19e-01    1.62   0.1063
## word.be                             -8.20e-01   4.03e-01   -2.04   0.0416
## word.as                              8.09e-01   5.63e-01    1.44   0.1507
## word.like                           -4.88e-01   5.68e-01   -0.86   0.3905
## word.me                              1.39e-01   6.85e-01    0.20   0.8393
## word.out                            -2.12e-01   4.89e-01   -0.43   0.6652
## word.there                           1.22e+00   6.43e-01    1.90   0.0579
## word.here                            4.65e-01   6.09e-01    0.76   0.4451
## word.just                            3.15e-02   7.41e-01    0.04   0.9661
## word.great                          -1.84e-01   5.64e-01   -0.33   0.7443
## word.all                            -1.40e+00   8.42e-01   -1.67   0.0952
## user.average_stars                  -1.15e-01   5.53e-01   -0.21   0.8349
## user.review_count                    9.12e-04   1.93e-03    0.47   0.6362
## business.categories.BeautySpasTRUE   1.86e+00   2.11e+00    0.88   0.3795
## business.categories.FoodTRUE        -6.69e-01   1.08e+00   -0.62   0.5366
## business.categories.RestaurantsTRUE -7.90e-01   7.79e-01   -1.01   0.3106
## business.categories.ShoppingTRUE     1.66e-01   1.05e+00    0.16   0.8743
## business.categories.total            2.46e-01   2.21e-01    1.11   0.2654
## business.latitude                   -7.87e+00   4.94e+00   -1.59   0.1111
## business.longitude                  -5.27e+00   3.28e+00   -1.61   0.1081
## business.openTRUE                   -9.44e-01   1.14e+00   -0.83   0.4075
## business.review_count               -4.19e-03   3.24e-03   -1.29   0.1954
## business.stars                       1.31e-01   5.69e-01    0.23   0.8174
## business.zipcode                    -2.99e-03   2.34e-03   -1.28   0.2021
## checkin.total                        1.07e-04   1.36e-04    0.79   0.4317
##                                       
## (Intercept)                           
## stars                                 
## date                                * 
## `word. `                            . 
## word..                                
## word.the                            * 
## `word.,`                              
## word.and                              
## word.I                              * 
## word.a                                
## `word.\\n`                          **
## word.to                               
## word.of                               
## word.was                              
## word.is                             . 
## word.for                              
## word.it                             **
## word.in                               
## `word.!`                              
## word.The                              
## word.that                             
## word.with                             
## word.but                              
## word.you                              
## word.my                               
## word.on                             * 
## word.have                             
## word.this                           . 
## word.had                              
## word.are                              
## word.they                             
## word.not                              
## `word.)`                              
## word.place                            
## word.at                               
## word.good                             
## word.were                             
## `word.(`                              
## word.food                             
## `word."`                            . 
## word.we                               
## word.so                               
## `word.-`                              
## word.be                             * 
## word.as                               
## word.like                             
## word.me                               
## word.out                              
## word.there                          . 
## word.here                             
## word.just                             
## word.great                            
## word.all                            . 
## user.average_stars                    
## user.review_count                     
## business.categories.BeautySpasTRUE    
## business.categories.FoodTRUE          
## business.categories.RestaurantsTRUE   
## business.categories.ShoppingTRUE      
## business.categories.total             
## business.latitude                     
## business.longitude                    
## business.openTRUE                     
## business.review_count                 
## business.stars                        
## business.zipcode                      
## checkin.total                         
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 300.415  on 99  degrees of freedom
## Residual deviance:  34.858  on 33  degrees of freedom
## AIC: 309.1
## 
## Number of Fisher Scoring iterations: 7
```

```r
summary(fit.pois.aic)
```

```
## 
## Call:
## glm(formula = votes.useful ~ date + `word. ` + word.the + word.and + 
##     word.I + `word.\n` + word.of + word.is + word.it + word.with + 
##     word.you + word.on + word.this + word.had + word.they + `word."` + 
##     `word.-` + word.be + word.as + word.all + user.review_count + 
##     business.categories.BeautySpas + business.longitude, family = poisson, 
##     data = trainC)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -2.168  -0.713  -0.232   0.491   1.560  
## 
## Coefficients:
##                                     Estimate Std. Error z value Pr(>|z|)
## (Intercept)                        -2.46e+02   1.18e+02   -2.08  0.03751
## date                               -1.02e-03   2.71e-04   -3.74  0.00018
## `word. `                           -1.28e-02   4.98e-03   -2.58  0.00984
## word.the                            8.65e-02   5.28e-02    1.64  0.10146
## word.and                            1.61e-01   4.93e-02    3.27  0.00107
## word.I                             -2.17e-01   5.59e-02   -3.88  0.00011
## `word.\\n`                          2.49e-01   3.78e-02    6.58  4.6e-11
## word.of                             1.39e-01   7.64e-02    1.82  0.06893
## word.is                            -2.89e-01   9.96e-02   -2.90  0.00375
## word.it                             2.22e-01   8.04e-02    2.76  0.00579
## word.with                          -2.78e-01   1.26e-01   -2.21  0.02728
## word.you                           -1.96e-01   1.14e-01   -1.71  0.08635
## word.on                             5.88e-01   1.37e-01    4.29  1.8e-05
## word.this                           5.11e-01   1.96e-01    2.61  0.00917
## word.had                           -2.85e-01   1.46e-01   -1.96  0.05038
## word.they                           3.14e-01   1.20e-01    2.61  0.00902
## `word."`                           -4.34e-01   1.06e-01   -4.11  3.9e-05
## `word.-`                            4.97e-01   1.45e-01    3.43  0.00060
## word.be                            -2.88e-01   1.71e-01   -1.68  0.09278
## word.as                             3.92e-01   1.39e-01    2.82  0.00487
## word.all                           -8.68e-01   2.65e-01   -3.27  0.00108
## user.review_count                   2.06e-03   6.48e-04    3.18  0.00146
## business.categories.BeautySpasTRUE  1.05e+00   6.25e-01    1.68  0.09368
## business.longitude                 -2.33e+00   1.06e+00   -2.21  0.02737
##                                       
## (Intercept)                        *  
## date                               ***
## `word. `                           ** 
## word.the                              
## word.and                           ** 
## word.I                             ***
## `word.\\n`                         ***
## word.of                            .  
## word.is                            ** 
## word.it                            ** 
## word.with                          *  
## word.you                           .  
## word.on                            ***
## word.this                          ** 
## word.had                           .  
## word.they                          ** 
## `word."`                           ***
## `word.-`                           ***
## word.be                            .  
## word.as                            ** 
## word.all                           ** 
## user.review_count                  ** 
## business.categories.BeautySpasTRUE .  
## business.longitude                 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 300.415  on 99  degrees of freedom
## Residual deviance:  64.633  on 76  degrees of freedom
## AIC: 252.9
## 
## Number of Fisher Scoring iterations: 5
```

```r
summary(fit.pois.bic)
```

```
## 
## Call:
## glm(formula = votes.useful ~ date + word.and + word.I + `word.\n` + 
##     word.is + word.with + word.on + word.had + `word."` + `word.-` + 
##     word.all + user.review_count, family = poisson, data = trainC)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -2.321  -0.869  -0.537   0.589   2.425  
## 
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       13.506368   3.548971    3.81  0.00014 ***
## date              -0.000939   0.000238   -3.95  7.8e-05 ***
## word.and           0.122757   0.040196    3.05  0.00226 ** 
## word.I            -0.147274   0.038980   -3.78  0.00016 ***
## `word.\\n`         0.164209   0.030260    5.43  5.7e-08 ***
## word.is           -0.239525   0.072438   -3.31  0.00094 ***
## word.with         -0.262521   0.092926   -2.83  0.00473 ** 
## word.on            0.400497   0.118161    3.39  0.00070 ***
## word.had          -0.208328   0.098783   -2.11  0.03495 *  
## `word."`          -0.283916   0.093626   -3.03  0.00243 ** 
## `word.-`           0.308591   0.122646    2.52  0.01187 *  
## word.all          -0.731054   0.206611   -3.54  0.00040 ***
## user.review_count  0.003518   0.000525    6.70  2.1e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 300.415  on 99  degrees of freedom
## Residual deviance:  91.146  on 87  degrees of freedom
## AIC: 257.4
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
cv.bin <- cvFit(fit.bin, y = trainC$votes.useful, data = trainC, cost = rmlspe, 
    costArgs = list(includeSE = TRUE), folds = folds)
```

```
## Error: object 'rmlspe' not found
```

```r
cv.bin.aic <- cvFit(fit.bin.aic, y = trainC$votes.useful, data = trainC, cost = rmlspe, 
    costArgs = list(includeSE = TRUE), folds = folds)
```

```
## Error: object 'rmlspe' not found
```

```r
cv.bin.bic <- cvFit(fit.bin.bic, y = trainC$votes.useful, data = trainC, cost = rmlspe, 
    costArgs = list(includeSE = TRUE), folds = folds)
```

```
## Error: object 'rmlspe' not found
```

```r
cv.pois <- cvFit(fit.pois, y = trainC$votes.useful, data = trainC, cost = rmlspe, 
    costArgs = list(includeSE = TRUE), folds = folds)
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted rates numerically 0 occurred
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted rates numerically 0 occurred
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted rates numerically 0 occurred
```

```
## Error: object 'rmlspe' not found
```

```r
cv.pois.aic <- cvFit(fit.pois.aic, y = trainC$votes.useful, data = trainC, cost = rmlspe, 
    costArgs = list(includeSE = TRUE), folds = folds)
```

```
## Error: object 'rmlspe' not found
```

```r
cv.pois.bic <- cvFit(fit.pois.bic, y = trainC$votes.useful, data = trainC, cost = rmlspe, 
    costArgs = list(includeSE = TRUE), folds = folds)
```

```
## Error: object 'rmlspe' not found
```

```r

cv.sel <- cvSelect(bin = cv.bin, bin.aic = cv.bin.aic, bin.bic = cv.bin.bic, 
    pois = cv.pois, pois.aic = cv.pois.aic, pois.bic = cv.pois.bic)
```

```
## Error: object 'cv.bin' not found
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

## Evaluate with validation data set
e.bin <- rmlspe(validateC$votes.useful, predict(fit.bin, validateC), includeSE = TRUE)
```

```
## Error: could not find function "rmlspe"
```

```r
e.bin.aic <- rmlspe(validateC$votes.useful, predict(fit.bin.aic, validateC), 
    includeSE = TRUE)
```

```
## Error: could not find function "rmlspe"
```

```r
e.bin.bic <- rmlspe(validateC$votes.useful, predict(fit.bin.bic, validateC), 
    includeSE = TRUE)
```

```
## Error: could not find function "rmlspe"
```

```r
e.pois <- rmlspe(validateC$votes.useful, predict(fit.pois, validateC), includeSE = TRUE)
```

```
## Error: could not find function "rmlspe"
```

```r
e.pois.aic <- rmlspe(validateC$votes.useful, predict(fit.pois.aic, validateC), 
    includeSE = TRUE)
```

```
## Error: could not find function "rmlspe"
```

```r
e.pois.bic <- rmlspe(validateC$votes.useful, predict(fit.pois.bic, validateC), 
    includeSE = TRUE)
```

```
## Error: could not find function "rmlspe"
```

```r
e.glm <- list(e.bin, e.bin.aic, e.bin.bic, e.pois, e.pois.aic, e.pois.bic)
```

```
## Error: object 'e.bin' not found
```

```r
names(e.glm) <- as.character(cv.sel$cv$Fit)
```

```
## Error: object 'cv.sel' not found
```

```r
unlist(lapply(e.glm, function(x) {
    x$rmspe
}))
```

```
## Error: object 'e.glm' not found
```

```r

rm(validateC, trainC, test, seed, K, R)
save.image(file = "glm.Rdata")
```


Reproducibility

```r
print(proc.time())
```

```
##    user  system elapsed 
##  37.759   2.524  40.464
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
## [1] cvTools_0.3.2    robustbase_0.9-7 lattice_0.20-15  knitr_1.1       
## 
## loaded via a namespace (and not attached):
## [1] digest_0.6.3   evaluate_0.4.3 formatR_0.7    grid_2.15.3   
## [5] stringr_0.6.2  tools_2.15.3
```


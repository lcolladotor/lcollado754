$n = 20$, Design I, Correlation Exchangeable or AR-1, $\alpha = 0.5, 0.9$
============================================================

# Emily and Leo

Define the x and x beta.

```r
X <- c(8, 10, 12, 14)
beta0 <- 0
beta1 <- 0.6
xbeta <- beta0 + beta1 * X
```



Determine the correlation matrix assuming S is the identity matrix.

```r
alpha <- 0.5
V <- matrix(alpha, ncol = 4, nrow = 4)
diag(V) <- 1
```



Generate Ys.

```r
library(mvtnorm)
set.seed(20130418)
data <- rmvnorm(n = 20, mean = xbeta, sigma = V)

## Explore the result
head(data)
```

```
##       [,1]  [,2]  [,3]  [,4]
## [1,] 5.553 6.044 7.864 8.977
## [2,] 5.620 7.567 8.451 8.186
## [3,] 3.963 4.774 5.588 8.292
## [4,] 4.651 5.250 4.418 6.737
## [5,] 5.444 6.810 7.449 8.830
## [6,] 3.279 4.985 6.473 7.069
```

```r

## Check the result
colMeans(data)
```

```
## [1] 4.786 5.803 6.976 8.114
```

```r
xbeta
```

```
## [1] 4.8 6.0 7.2 8.4
```


Group the data for using gee()

```r
## Group the data
df <- data.frame(X = rep(X, each = 20), Y = as.vector(data), id = rep(1:20, 
    4))
## Remember to sort it!
df.sorted <- df[order(df$id), ]
head(df.sorted)
```

```
##     X     Y id
## 1   8 5.553  1
## 21 10 6.044  1
## 41 12 7.864  1
## 61 14 8.977  1
## 2   8 5.620  2
## 22 10 7.567  2
```


Fit the GEE

```r
library(gee)
fit <- gee(Y ~ X, id = id, data = df.sorted, corstr = "exchangeable")
```

```
## Beginning Cgee S-function, @(#) geeformula.q 4.13 98/01/27
```

```
## running glm to get initial regression estimate
```

```
## (Intercept)           X 
##      0.2838      0.5578
```

```r
summary(fit)
```

```
## 
##  GEE:  GENERALIZED LINEAR MODELS FOR DEPENDENT DATA
##  gee S-function, version 4.13 modified 98/01/27 (1998) 
## 
## Model:
##  Link:                      Identity 
##  Variance to Mean Relation: Gaussian 
##  Correlation Structure:     Exchangeable 
## 
## Call:
## gee(formula = Y ~ X, id = id, data = df.sorted, corstr = "exchangeable")
## 
## Summary of Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.5593 -0.7371  0.1963  0.7249  1.7324 
## 
## 
## Coefficients:
##             Estimate Naive S.E. Naive z Robust S.E. Robust z
## (Intercept)   0.2838    0.37828  0.7502      0.3427   0.8281
## X             0.5578    0.03041 18.3432      0.0289  19.3016
## 
## Estimated Scale Parameter:  0.9015
## Number of Iterations:  1
## 
## Working Correlation
##        [,1]   [,2]   [,3]   [,4]
## [1,] 1.0000 0.5897 0.5897 0.5897
## [2,] 0.5897 1.0000 0.5897 0.5897
## [3,] 0.5897 0.5897 1.0000 0.5897
## [4,] 0.5897 0.5897 0.5897 1.0000
```


Find the coverage

```r
est <- fit$coefficients[2]
sErr <- diag(sqrt(fit$robust.variance))[2]
```

```
## Warning: NaNs produced
```

```r
cov <- (est + 1.96 * sErr > 0.6) & (est - 1.96 * sErr < 0.6)
cov
```

```
##    X 
## TRUE
```



Lets make it a function

```r
runOne <- function(wcorr, xbeta, V) {
    ## Generate data at random
    data <- rmvnorm(n = 20, mean = xbeta, sigma = V)
    
    ## Organize it
    df <- data.frame(X = rep(X, each = 20), Y = as.vector(data), id = rep(1:20, 
        4))
    ## Remember to sort it!
    df.sorted <- df[order(df$id), ]
    
    ## Fit the GEE
    if (wcorr == "AR-M") {
        fit <- gee(Y ~ X, id = id, data = df.sorted, corstr = wcorr, Mv = 1, 
            silent = TRUE)
    } else {
        fit <- gee(Y ~ X, id = id, data = df.sorted, corstr = wcorr, silent = TRUE)
    }
    
    est <- fit$coefficients[2]
    
    ## Find if it's covered
    sErr <- diag(sqrt(fit$robust.variance))[2]
    cov <- (est + 1.96 * sErr > 0.6) & (est - 1.96 * sErr < 0.6)
    
    ## Done
    return(c(cov, sErr^2))
}
vMat <- function(x, alpha) {
    nObs <- length(x)
    vMat <- matrix(NA, nrow = nObs, ncol = nObs)
    for (i in 1:nObs) {
        vMat[i, ] <- alpha^abs((x - x[i])/2)
    }
    return(vMat)
}
runAll <- function(tcorr = "exchangeable", wcorr = "independence", alpha, xbeta, 
    n = 100) {
    ## Make the sigma matrix
    if (tcorr == "exchangeable") {
        V <- matrix(alpha, ncol = 4, nrow = 4)
        diag(V) <- 1
    } else {
        V <- vMat(xbeta, alpha)
    }
    
    
    ## Random data + GEE
    res <- sapply(1:n, function(i) {
        runOne(wcorr = wcorr, xbeta = xbeta, V = V)
    })
    
    ## Calculate the percent covered
    final <- rowMeans(res)
    return(final)
}
```


Now lets run the function


```r
x1 <- matrix(NA, ncol = 2, nrow = 12)
set.seed(20130425)
x1[1, ] <- runAll(alpha = 0.5, xbeta = xbeta)
```

```
## (Intercept)           X 
##     0.08497     0.59678
```

```
## (Intercept)           X 
##     -0.7481      0.6430
```

```
## (Intercept)           X 
##      -1.017       0.680
```

```
## (Intercept)           X 
##      0.3844      0.5768
```

```
## (Intercept)           X 
##     -0.3120      0.6211
```

```
## (Intercept)           X 
##    -0.08453     0.61465
```

```
## (Intercept)           X 
##     -0.3238      0.6333
```

```
## (Intercept)           X 
##      0.3791      0.5740
```

```
## (Intercept)           X 
##      0.4420      0.5755
```

```
## (Intercept)           X 
##      0.9508      0.5045
```

```
## (Intercept)           X 
##    -0.03696     0.60277
```

```
## (Intercept)           X 
##      0.2860      0.5607
```

```
## (Intercept)           X 
##      0.2349      0.5794
```

```
## (Intercept)           X 
##      0.5050      0.5726
```

```
## (Intercept)           X 
##     -0.2465      0.6090
```

```
## (Intercept)           X 
##      0.3798      0.5662
```

```
## (Intercept)           X 
##    -0.02786     0.57373
```

```
## (Intercept)           X 
##     -0.5381      0.6379
```

```
## (Intercept)           X 
##      0.1753      0.5786
```

```
## (Intercept)           X 
##     -0.2881      0.6220
```

```
## (Intercept)           X 
##    -0.09144     0.56858
```

```
## (Intercept)           X 
##    -0.09423     0.60896
```

```
## (Intercept)           X 
##     -0.7464      0.6826
```

```
## (Intercept)           X 
##      0.0607      0.5916
```

```
## (Intercept)           X 
##     -0.7962      0.6432
```

```
## (Intercept)           X 
##      0.3794      0.5728
```

```
## (Intercept)           X 
##      0.2840      0.5445
```

```
## (Intercept)           X 
##     -0.1582      0.6091
```

```
## (Intercept)           X 
##     -0.1942      0.5942
```

```
## (Intercept)           X 
##     -0.3263      0.6345
```

```
## (Intercept)           X 
##     -0.8257      0.6687
```

```
## (Intercept)           X 
##      0.2644      0.5706
```

```
## (Intercept)           X 
##      0.1921      0.5956
```

```
## (Intercept)           X 
##     0.07294     0.59513
```

```
## (Intercept)           X 
##      0.5387      0.5553
```

```
## (Intercept)           X 
##    -0.07865     0.62126
```

```
## (Intercept)           X 
##      0.3934      0.5556
```

```
## (Intercept)           X 
##     -0.3654      0.6330
```

```
## (Intercept)           X 
##      0.2487      0.5747
```

```
## (Intercept)           X 
##      0.4900      0.5424
```

```
## (Intercept)           X 
##     -0.2724      0.6205
```

```
## (Intercept)           X 
##    -0.09089     0.60542
```

```
## (Intercept)           X 
##     0.04283     0.58202
```

```
## (Intercept)           X 
##     -0.5455      0.6191
```

```
## (Intercept)           X 
##     -0.3957      0.6290
```

```
## (Intercept)           X 
##      0.9851      0.5070
```

```
## (Intercept)           X 
##      0.3338      0.5836
```

```
## (Intercept)           X 
##      0.4747      0.5692
```

```
## (Intercept)           X 
##     -0.4169      0.6329
```

```
## (Intercept)           X 
##     -0.2145      0.6062
```

```
## (Intercept)           X 
##      0.1520      0.5744
```

```
## (Intercept)           X 
##     -0.6365      0.6276
```

```
## (Intercept)           X 
##      0.7018      0.5392
```

```
## (Intercept)           X 
##      0.8345      0.5689
```

```
## (Intercept)           X 
##      0.2615      0.5764
```

```
## (Intercept)           X 
##     -0.1561      0.5931
```

```
## (Intercept)           X 
##     -0.2243      0.6099
```

```
## (Intercept)           X 
##     -0.2250      0.6071
```

```
## (Intercept)           X 
##      0.5044      0.5604
```

```
## (Intercept)           X 
##      0.3425      0.5820
```

```
## (Intercept)           X 
##      0.4051      0.5654
```

```
## (Intercept)           X 
##     -0.5886      0.6226
```

```
## (Intercept)           X 
##     -0.2863      0.6228
```

```
## (Intercept)           X 
##      0.2746      0.5582
```

```
## (Intercept)           X 
##      0.2206      0.5772
```

```
## (Intercept)           X 
##      0.4214      0.5591
```

```
## (Intercept)           X 
##     -0.1198      0.5947
```

```
## (Intercept)           X 
##      0.5429      0.5808
```

```
## (Intercept)           X 
##     -0.1835      0.6130
```

```
## (Intercept)           X 
##     -0.8078      0.6560
```

```
## (Intercept)           X 
##     -0.4656      0.6475
```

```
## (Intercept)           X 
##      0.1679      0.5801
```

```
## (Intercept)           X 
##     -0.1266      0.6268
```

```
## (Intercept)           X 
##     -0.5655      0.6583
```

```
## (Intercept)           X 
##     -0.6266      0.6657
```

```
## (Intercept)           X 
##     -0.9008      0.6663
```

```
## (Intercept)           X 
##      1.2283      0.5085
```

```
## (Intercept)           X 
##      0.4653      0.5714
```

```
## (Intercept)           X 
##      0.5477      0.5599
```

```
## (Intercept)           X 
##     -0.9504      0.6775
```

```
## (Intercept)           X 
##     -0.6110      0.6449
```

```
## (Intercept)           X 
##     -0.1945      0.5998
```

```
## (Intercept)           X 
##     0.04006     0.60808
```

```
## (Intercept)           X 
##      0.9256      0.5450
```

```
## (Intercept)           X 
##     -0.4545      0.6317
```

```
## (Intercept)           X 
##    -0.02342     0.58966
```

```
## (Intercept)           X 
##     -0.5753      0.6184
```

```
## (Intercept)           X 
##     -0.7771      0.6739
```

```
## (Intercept)           X 
##     -0.2378      0.6123
```

```
## (Intercept)           X 
##      0.9810      0.5321
```

```
## (Intercept)           X 
##     -0.6217      0.6443
```

```
## (Intercept)           X 
##     -0.8709      0.6851
```

```
## (Intercept)           X 
##      0.1795      0.6148
```

```
## (Intercept)           X 
##      0.2185      0.5958
```

```
## (Intercept)           X 
##     -0.3984      0.6086
```

```
## (Intercept)           X 
##     -0.2524      0.6211
```

```
## (Intercept)           X 
##     -0.4336      0.6242
```

```
## (Intercept)           X 
##     -0.7681      0.6613
```

```
## (Intercept)           X 
##     0.08405     0.61070
```

```
## (Intercept)           X 
##     -0.1985      0.6305
```

```r
set.seed(20130425)
x1[2, ] <- runAll(alpha = 0.5, xbeta = xbeta, wcorr = "exchangeable")
```

```
## (Intercept)           X 
##     0.08497     0.59678
```

```
## (Intercept)           X 
##     -0.7481      0.6430
```

```
## (Intercept)           X 
##      -1.017       0.680
```

```
## (Intercept)           X 
##      0.3844      0.5768
```

```
## (Intercept)           X 
##     -0.3120      0.6211
```

```
## (Intercept)           X 
##    -0.08453     0.61465
```

```
## (Intercept)           X 
##     -0.3238      0.6333
```

```
## (Intercept)           X 
##      0.3791      0.5740
```

```
## (Intercept)           X 
##      0.4420      0.5755
```

```
## (Intercept)           X 
##      0.9508      0.5045
```

```
## (Intercept)           X 
##    -0.03696     0.60277
```

```
## (Intercept)           X 
##      0.2860      0.5607
```

```
## (Intercept)           X 
##      0.2349      0.5794
```

```
## (Intercept)           X 
##      0.5050      0.5726
```

```
## (Intercept)           X 
##     -0.2465      0.6090
```

```
## (Intercept)           X 
##      0.3798      0.5662
```

```
## (Intercept)           X 
##    -0.02786     0.57373
```

```
## (Intercept)           X 
##     -0.5381      0.6379
```

```
## (Intercept)           X 
##      0.1753      0.5786
```

```
## (Intercept)           X 
##     -0.2881      0.6220
```

```
## (Intercept)           X 
##    -0.09144     0.56858
```

```
## (Intercept)           X 
##    -0.09423     0.60896
```

```
## (Intercept)           X 
##     -0.7464      0.6826
```

```
## (Intercept)           X 
##      0.0607      0.5916
```

```
## (Intercept)           X 
##     -0.7962      0.6432
```

```
## (Intercept)           X 
##      0.3794      0.5728
```

```
## (Intercept)           X 
##      0.2840      0.5445
```

```
## (Intercept)           X 
##     -0.1582      0.6091
```

```
## (Intercept)           X 
##     -0.1942      0.5942
```

```
## (Intercept)           X 
##     -0.3263      0.6345
```

```
## (Intercept)           X 
##     -0.8257      0.6687
```

```
## (Intercept)           X 
##      0.2644      0.5706
```

```
## (Intercept)           X 
##      0.1921      0.5956
```

```
## (Intercept)           X 
##     0.07294     0.59513
```

```
## (Intercept)           X 
##      0.5387      0.5553
```

```
## (Intercept)           X 
##    -0.07865     0.62126
```

```
## (Intercept)           X 
##      0.3934      0.5556
```

```
## (Intercept)           X 
##     -0.3654      0.6330
```

```
## (Intercept)           X 
##      0.2487      0.5747
```

```
## (Intercept)           X 
##      0.4900      0.5424
```

```
## (Intercept)           X 
##     -0.2724      0.6205
```

```
## (Intercept)           X 
##    -0.09089     0.60542
```

```
## (Intercept)           X 
##     0.04283     0.58202
```

```
## (Intercept)           X 
##     -0.5455      0.6191
```

```
## (Intercept)           X 
##     -0.3957      0.6290
```

```
## (Intercept)           X 
##      0.9851      0.5070
```

```
## (Intercept)           X 
##      0.3338      0.5836
```

```
## (Intercept)           X 
##      0.4747      0.5692
```

```
## (Intercept)           X 
##     -0.4169      0.6329
```

```
## (Intercept)           X 
##     -0.2145      0.6062
```

```
## (Intercept)           X 
##      0.1520      0.5744
```

```
## (Intercept)           X 
##     -0.6365      0.6276
```

```
## (Intercept)           X 
##      0.7018      0.5392
```

```
## (Intercept)           X 
##      0.8345      0.5689
```

```
## (Intercept)           X 
##      0.2615      0.5764
```

```
## (Intercept)           X 
##     -0.1561      0.5931
```

```
## (Intercept)           X 
##     -0.2243      0.6099
```

```
## (Intercept)           X 
##     -0.2250      0.6071
```

```
## (Intercept)           X 
##      0.5044      0.5604
```

```
## (Intercept)           X 
##      0.3425      0.5820
```

```
## (Intercept)           X 
##      0.4051      0.5654
```

```
## (Intercept)           X 
##     -0.5886      0.6226
```

```
## (Intercept)           X 
##     -0.2863      0.6228
```

```
## (Intercept)           X 
##      0.2746      0.5582
```

```
## (Intercept)           X 
##      0.2206      0.5772
```

```
## (Intercept)           X 
##      0.4214      0.5591
```

```
## (Intercept)           X 
##     -0.1198      0.5947
```

```
## (Intercept)           X 
##      0.5429      0.5808
```

```
## (Intercept)           X 
##     -0.1835      0.6130
```

```
## (Intercept)           X 
##     -0.8078      0.6560
```

```
## (Intercept)           X 
##     -0.4656      0.6475
```

```
## (Intercept)           X 
##      0.1679      0.5801
```

```
## (Intercept)           X 
##     -0.1266      0.6268
```

```
## (Intercept)           X 
##     -0.5655      0.6583
```

```
## (Intercept)           X 
##     -0.6266      0.6657
```

```
## (Intercept)           X 
##     -0.9008      0.6663
```

```
## (Intercept)           X 
##      1.2283      0.5085
```

```
## (Intercept)           X 
##      0.4653      0.5714
```

```
## (Intercept)           X 
##      0.5477      0.5599
```

```
## (Intercept)           X 
##     -0.9504      0.6775
```

```
## (Intercept)           X 
##     -0.6110      0.6449
```

```
## (Intercept)           X 
##     -0.1945      0.5998
```

```
## (Intercept)           X 
##     0.04006     0.60808
```

```
## (Intercept)           X 
##      0.9256      0.5450
```

```
## (Intercept)           X 
##     -0.4545      0.6317
```

```
## (Intercept)           X 
##    -0.02342     0.58966
```

```
## (Intercept)           X 
##     -0.5753      0.6184
```

```
## (Intercept)           X 
##     -0.7771      0.6739
```

```
## (Intercept)           X 
##     -0.2378      0.6123
```

```
## (Intercept)           X 
##      0.9810      0.5321
```

```
## (Intercept)           X 
##     -0.6217      0.6443
```

```
## (Intercept)           X 
##     -0.8709      0.6851
```

```
## (Intercept)           X 
##      0.1795      0.6148
```

```
## (Intercept)           X 
##      0.2185      0.5958
```

```
## (Intercept)           X 
##     -0.3984      0.6086
```

```
## (Intercept)           X 
##     -0.2524      0.6211
```

```
## (Intercept)           X 
##     -0.4336      0.6242
```

```
## (Intercept)           X 
##     -0.7681      0.6613
```

```
## (Intercept)           X 
##     0.08405     0.61070
```

```
## (Intercept)           X 
##     -0.1985      0.6305
```

```r
set.seed(20130425)
x1[3, ] <- runAll(alpha = 0.5, xbeta = xbeta, wcorr = "AR-M")
```

```
## (Intercept)           X 
##     0.08497     0.59678
```

```
## (Intercept)           X 
##     -0.7481      0.6430
```

```
## (Intercept)           X 
##      -1.017       0.680
```

```
## (Intercept)           X 
##      0.3844      0.5768
```

```
## (Intercept)           X 
##     -0.3120      0.6211
```

```
## (Intercept)           X 
##    -0.08453     0.61465
```

```
## (Intercept)           X 
##     -0.3238      0.6333
```

```
## (Intercept)           X 
##      0.3791      0.5740
```

```
## (Intercept)           X 
##      0.4420      0.5755
```

```
## (Intercept)           X 
##      0.9508      0.5045
```

```
## (Intercept)           X 
##    -0.03696     0.60277
```

```
## (Intercept)           X 
##      0.2860      0.5607
```

```
## (Intercept)           X 
##      0.2349      0.5794
```

```
## (Intercept)           X 
##      0.5050      0.5726
```

```
## (Intercept)           X 
##     -0.2465      0.6090
```

```
## (Intercept)           X 
##      0.3798      0.5662
```

```
## (Intercept)           X 
##    -0.02786     0.57373
```

```
## (Intercept)           X 
##     -0.5381      0.6379
```

```
## (Intercept)           X 
##      0.1753      0.5786
```

```
## (Intercept)           X 
##     -0.2881      0.6220
```

```
## (Intercept)           X 
##    -0.09144     0.56858
```

```
## (Intercept)           X 
##    -0.09423     0.60896
```

```
## (Intercept)           X 
##     -0.7464      0.6826
```

```
## (Intercept)           X 
##      0.0607      0.5916
```

```
## (Intercept)           X 
##     -0.7962      0.6432
```

```
## (Intercept)           X 
##      0.3794      0.5728
```

```
## (Intercept)           X 
##      0.2840      0.5445
```

```
## (Intercept)           X 
##     -0.1582      0.6091
```

```
## (Intercept)           X 
##     -0.1942      0.5942
```

```
## (Intercept)           X 
##     -0.3263      0.6345
```

```
## (Intercept)           X 
##     -0.8257      0.6687
```

```
## (Intercept)           X 
##      0.2644      0.5706
```

```
## (Intercept)           X 
##      0.1921      0.5956
```

```
## (Intercept)           X 
##     0.07294     0.59513
```

```
## (Intercept)           X 
##      0.5387      0.5553
```

```
## (Intercept)           X 
##    -0.07865     0.62126
```

```
## (Intercept)           X 
##      0.3934      0.5556
```

```
## (Intercept)           X 
##     -0.3654      0.6330
```

```
## (Intercept)           X 
##      0.2487      0.5747
```

```
## (Intercept)           X 
##      0.4900      0.5424
```

```
## (Intercept)           X 
##     -0.2724      0.6205
```

```
## (Intercept)           X 
##    -0.09089     0.60542
```

```
## (Intercept)           X 
##     0.04283     0.58202
```

```
## (Intercept)           X 
##     -0.5455      0.6191
```

```
## (Intercept)           X 
##     -0.3957      0.6290
```

```
## (Intercept)           X 
##      0.9851      0.5070
```

```
## (Intercept)           X 
##      0.3338      0.5836
```

```
## (Intercept)           X 
##      0.4747      0.5692
```

```
## (Intercept)           X 
##     -0.4169      0.6329
```

```
## (Intercept)           X 
##     -0.2145      0.6062
```

```
## (Intercept)           X 
##      0.1520      0.5744
```

```
## (Intercept)           X 
##     -0.6365      0.6276
```

```
## (Intercept)           X 
##      0.7018      0.5392
```

```
## (Intercept)           X 
##      0.8345      0.5689
```

```
## (Intercept)           X 
##      0.2615      0.5764
```

```
## (Intercept)           X 
##     -0.1561      0.5931
```

```
## (Intercept)           X 
##     -0.2243      0.6099
```

```
## (Intercept)           X 
##     -0.2250      0.6071
```

```
## (Intercept)           X 
##      0.5044      0.5604
```

```
## (Intercept)           X 
##      0.3425      0.5820
```

```
## (Intercept)           X 
##      0.4051      0.5654
```

```
## (Intercept)           X 
##     -0.5886      0.6226
```

```
## (Intercept)           X 
##     -0.2863      0.6228
```

```
## (Intercept)           X 
##      0.2746      0.5582
```

```
## (Intercept)           X 
##      0.2206      0.5772
```

```
## (Intercept)           X 
##      0.4214      0.5591
```

```
## (Intercept)           X 
##     -0.1198      0.5947
```

```
## (Intercept)           X 
##      0.5429      0.5808
```

```
## (Intercept)           X 
##     -0.1835      0.6130
```

```
## (Intercept)           X 
##     -0.8078      0.6560
```

```
## (Intercept)           X 
##     -0.4656      0.6475
```

```
## (Intercept)           X 
##      0.1679      0.5801
```

```
## (Intercept)           X 
##     -0.1266      0.6268
```

```
## (Intercept)           X 
##     -0.5655      0.6583
```

```
## (Intercept)           X 
##     -0.6266      0.6657
```

```
## (Intercept)           X 
##     -0.9008      0.6663
```

```
## (Intercept)           X 
##      1.2283      0.5085
```

```
## (Intercept)           X 
##      0.4653      0.5714
```

```
## (Intercept)           X 
##      0.5477      0.5599
```

```
## (Intercept)           X 
##     -0.9504      0.6775
```

```
## (Intercept)           X 
##     -0.6110      0.6449
```

```
## (Intercept)           X 
##     -0.1945      0.5998
```

```
## (Intercept)           X 
##     0.04006     0.60808
```

```
## (Intercept)           X 
##      0.9256      0.5450
```

```
## (Intercept)           X 
##     -0.4545      0.6317
```

```
## (Intercept)           X 
##    -0.02342     0.58966
```

```
## (Intercept)           X 
##     -0.5753      0.6184
```

```
## (Intercept)           X 
##     -0.7771      0.6739
```

```
## (Intercept)           X 
##     -0.2378      0.6123
```

```
## (Intercept)           X 
##      0.9810      0.5321
```

```
## (Intercept)           X 
##     -0.6217      0.6443
```

```
## (Intercept)           X 
##     -0.8709      0.6851
```

```
## (Intercept)           X 
##      0.1795      0.6148
```

```
## (Intercept)           X 
##      0.2185      0.5958
```

```
## (Intercept)           X 
##     -0.3984      0.6086
```

```
## (Intercept)           X 
##     -0.2524      0.6211
```

```
## (Intercept)           X 
##     -0.4336      0.6242
```

```
## (Intercept)           X 
##     -0.7681      0.6613
```

```
## (Intercept)           X 
##     0.08405     0.61070
```

```
## (Intercept)           X 
##     -0.1985      0.6305
```

```r
set.seed(20130425)
x1[4, ] <- runAll(alpha = 0.9, xbeta = xbeta)
```

```
## (Intercept)           X 
##     0.07617     0.59856
```

```
## (Intercept)           X 
##     -0.5464      0.6192
```

```
## (Intercept)           X 
##     -0.5609      0.6358
```

```
## (Intercept)           X 
##      0.2717      0.5896
```

```
## (Intercept)           X 
##     -0.2006      0.6095
```

```
## (Intercept)           X 
##     0.02111     0.60655
```

```
## (Intercept)           X 
##     -0.1121      0.6149
```

```
## (Intercept)           X 
##      0.2408      0.5884
```

```
## (Intercept)           X 
##      0.3301      0.5890
```

```
## (Intercept)           X 
##      0.3482      0.5573
```

```
## (Intercept)           X 
##    -0.02155     0.60124
```

```
## (Intercept)           X 
##     0.01549     0.58243
```

```
## (Intercept)           X 
##      0.1111      0.5908
```

```
## (Intercept)           X 
##      0.3829      0.5878
```

```
## (Intercept)           X 
##     -0.2237      0.6040
```

```
## (Intercept)           X 
##      0.1763      0.5849
```

```
## (Intercept)           X 
##     -0.2562      0.5883
```

```
## (Intercept)           X 
##     -0.3336      0.6170
```

```
## (Intercept)           X 
##     0.03218     0.59043
```

```
## (Intercept)           X 
##     -0.1641      0.6098
```

```
## (Intercept)           X 
##     -0.3772      0.5859
```

```
## (Intercept)           X 
##    -0.03878     0.60401
```

```
## (Intercept)           X 
##     -0.2089      0.6369
```

```
## (Intercept)           X 
##    0.002555    0.596233
```

```
## (Intercept)           X 
##     -0.6031      0.6193
```

```
## (Intercept)           X 
##      0.2314      0.5878
```

```
## (Intercept)           X 
##     -0.1242      0.5752
```

```
## (Intercept)           X 
##     -0.1152      0.6041
```

```
## (Intercept)           X 
##     -0.2851      0.5974
```

```
## (Intercept)           X 
##     -0.1048      0.6154
```

```
## (Intercept)           X 
##     -0.4227      0.6307
```

```
## (Intercept)           X 
##     0.07248     0.58683
```

```
## (Intercept)           X 
##      0.1969      0.5981
```

```
## (Intercept)           X 
##     0.04756     0.59782
```

```
## (Intercept)           X 
##      0.2773      0.5800
```

```
## (Intercept)           X 
##      0.0842      0.6095
```

```
## (Intercept)           X 
##      0.1033      0.5802
```

```
## (Intercept)           X 
##     -0.1651      0.6148
```

```
## (Intercept)           X 
##     0.08814     0.58867
```

```
## (Intercept)           X 
##      0.1083      0.5742
```

```
## (Intercept)           X 
##     -0.1577      0.6092
```

```
## (Intercept)           X 
##    -0.06467     0.60243
```

```
## (Intercept)           X 
##     -0.1001      0.5920
```

```
## (Intercept)           X 
##     -0.5022      0.6085
```

```
## (Intercept)           X 
##      -0.236       0.613
```

```
## (Intercept)           X 
##      0.4118      0.5584
```

```
## (Intercept)           X 
##      0.2674      0.5927
```

```
## (Intercept)           X 
##      0.3172      0.5862
```

```
## (Intercept)           X 
##     -0.2287      0.6147
```

```
## (Intercept)           X 
##     -0.2084      0.6028
```

```
## (Intercept)           X 
##    -0.03174     0.58855
```

```
## (Intercept)           X 
##     -0.5409      0.6123
```

```
## (Intercept)           X 
##      0.3390      0.5728
```

```
## (Intercept)           X 
##      0.7520      0.5861
```

```
## (Intercept)           X 
##      0.1181      0.5894
```

```
## (Intercept)           X 
##     -0.2482      0.5969
```

```
## (Intercept)           X 
##     -0.1894      0.6044
```

```
## (Intercept)           X 
##     -0.2138      0.6032
```

```
## (Intercept)           X 
##      0.2784      0.5823
```

```
## (Intercept)           X 
##      0.2645      0.5920
```

```
## (Intercept)           X 
##      0.1998      0.5845
```

```
## (Intercept)           X 
##     -0.5248      0.6101
```

```
## (Intercept)           X 
##     -0.1553      0.6102
```

```
## (Intercept)           X 
##    -0.01936     0.58132
```

```
## (Intercept)           X 
##     0.07509     0.58979
```

```
## (Intercept)           X 
##      0.1668      0.5817
```

```
## (Intercept)           X 
##     -0.1909      0.5976
```

```
## (Intercept)           X 
##      0.4980      0.5914
```

```
## (Intercept)           X 
##     -0.1129      0.6058
```

```
## (Intercept)           X 
##     -0.5092      0.6250
```

```
## (Intercept)           X 
##     -0.1647      0.6212
```

```
## (Intercept)           X 
##     0.03614     0.59111
```

```
## (Intercept)           X 
##     0.07289     0.61199
```

```
## (Intercept)           X 
##     -0.1949      0.6261
```

```
## (Intercept)           X 
##     -0.2065      0.6294
```

```
## (Intercept)           X 
##     -0.5348      0.6296
```

```
## (Intercept)           X 
##      0.7201      0.5591
```

```
## (Intercept)           X 
##      0.3243      0.5872
```

```
## (Intercept)           X 
##      0.3268      0.5821
```

```
## (Intercept)           X 
##     -0.5003      0.6347
```

```
## (Intercept)           X 
##     -0.3633      0.6201
```

```
## (Intercept)           X 
##     -0.2385      0.5999
```

```
## (Intercept)           X 
##      0.1171      0.6036
```

```
## (Intercept)           X 
##      0.6608      0.5754
```

```
## (Intercept)           X 
##     -0.2847      0.6142
```

```
## (Intercept)           X 
##     -0.1160      0.5954
```

```
## (Intercept)           X 
##     -0.5444      0.6082
```

```
## (Intercept)           X 
##     -0.3196      0.6331
```

```
## (Intercept)           X 
##     -0.1850      0.6055
```

```
## (Intercept)           X 
##      0.6186      0.5696
```

```
## (Intercept)           X 
##     -0.3813      0.6198
```

```
## (Intercept)           X 
##     -0.3394      0.6381
```

```
## (Intercept)           X 
##      0.3439      0.6066
```

```
## (Intercept)           X 
##      0.2303      0.5981
```

```
## (Intercept)           X 
##     -0.4117      0.6039
```

```
## (Intercept)           X 
##     -0.1284      0.6094
```

```
## (Intercept)           X 
##     -0.3225      0.6108
```

```
## (Intercept)           X 
##     -0.4161      0.6274
```

```
## (Intercept)           X 
##      0.1928      0.6048
```

```
## (Intercept)           X 
##      0.0170      0.6137
```

```r
set.seed(20130425)
x1[5, ] <- runAll(alpha = 0.9, xbeta = xbeta, wcorr = "exchangeable")
```

```
## (Intercept)           X 
##     0.07617     0.59856
```

```
## (Intercept)           X 
##     -0.5464      0.6192
```

```
## (Intercept)           X 
##     -0.5609      0.6358
```

```
## (Intercept)           X 
##      0.2717      0.5896
```

```
## (Intercept)           X 
##     -0.2006      0.6095
```

```
## (Intercept)           X 
##     0.02111     0.60655
```

```
## (Intercept)           X 
##     -0.1121      0.6149
```

```
## (Intercept)           X 
##      0.2408      0.5884
```

```
## (Intercept)           X 
##      0.3301      0.5890
```

```
## (Intercept)           X 
##      0.3482      0.5573
```

```
## (Intercept)           X 
##    -0.02155     0.60124
```

```
## (Intercept)           X 
##     0.01549     0.58243
```

```
## (Intercept)           X 
##      0.1111      0.5908
```

```
## (Intercept)           X 
##      0.3829      0.5878
```

```
## (Intercept)           X 
##     -0.2237      0.6040
```

```
## (Intercept)           X 
##      0.1763      0.5849
```

```
## (Intercept)           X 
##     -0.2562      0.5883
```

```
## (Intercept)           X 
##     -0.3336      0.6170
```

```
## (Intercept)           X 
##     0.03218     0.59043
```

```
## (Intercept)           X 
##     -0.1641      0.6098
```

```
## (Intercept)           X 
##     -0.3772      0.5859
```

```
## (Intercept)           X 
##    -0.03878     0.60401
```

```
## (Intercept)           X 
##     -0.2089      0.6369
```

```
## (Intercept)           X 
##    0.002555    0.596233
```

```
## (Intercept)           X 
##     -0.6031      0.6193
```

```
## (Intercept)           X 
##      0.2314      0.5878
```

```
## (Intercept)           X 
##     -0.1242      0.5752
```

```
## (Intercept)           X 
##     -0.1152      0.6041
```

```
## (Intercept)           X 
##     -0.2851      0.5974
```

```
## (Intercept)           X 
##     -0.1048      0.6154
```

```
## (Intercept)           X 
##     -0.4227      0.6307
```

```
## (Intercept)           X 
##     0.07248     0.58683
```

```
## (Intercept)           X 
##      0.1969      0.5981
```

```
## (Intercept)           X 
##     0.04756     0.59782
```

```
## (Intercept)           X 
##      0.2773      0.5800
```

```
## (Intercept)           X 
##      0.0842      0.6095
```

```
## (Intercept)           X 
##      0.1033      0.5802
```

```
## (Intercept)           X 
##     -0.1651      0.6148
```

```
## (Intercept)           X 
##     0.08814     0.58867
```

```
## (Intercept)           X 
##      0.1083      0.5742
```

```
## (Intercept)           X 
##     -0.1577      0.6092
```

```
## (Intercept)           X 
##    -0.06467     0.60243
```

```
## (Intercept)           X 
##     -0.1001      0.5920
```

```
## (Intercept)           X 
##     -0.5022      0.6085
```

```
## (Intercept)           X 
##      -0.236       0.613
```

```
## (Intercept)           X 
##      0.4118      0.5584
```

```
## (Intercept)           X 
##      0.2674      0.5927
```

```
## (Intercept)           X 
##      0.3172      0.5862
```

```
## (Intercept)           X 
##     -0.2287      0.6147
```

```
## (Intercept)           X 
##     -0.2084      0.6028
```

```
## (Intercept)           X 
##    -0.03174     0.58855
```

```
## (Intercept)           X 
##     -0.5409      0.6123
```

```
## (Intercept)           X 
##      0.3390      0.5728
```

```
## (Intercept)           X 
##      0.7520      0.5861
```

```
## (Intercept)           X 
##      0.1181      0.5894
```

```
## (Intercept)           X 
##     -0.2482      0.5969
```

```
## (Intercept)           X 
##     -0.1894      0.6044
```

```
## (Intercept)           X 
##     -0.2138      0.6032
```

```
## (Intercept)           X 
##      0.2784      0.5823
```

```
## (Intercept)           X 
##      0.2645      0.5920
```

```
## (Intercept)           X 
##      0.1998      0.5845
```

```
## (Intercept)           X 
##     -0.5248      0.6101
```

```
## (Intercept)           X 
##     -0.1553      0.6102
```

```
## (Intercept)           X 
##    -0.01936     0.58132
```

```
## (Intercept)           X 
##     0.07509     0.58979
```

```
## (Intercept)           X 
##      0.1668      0.5817
```

```
## (Intercept)           X 
##     -0.1909      0.5976
```

```
## (Intercept)           X 
##      0.4980      0.5914
```

```
## (Intercept)           X 
##     -0.1129      0.6058
```

```
## (Intercept)           X 
##     -0.5092      0.6250
```

```
## (Intercept)           X 
##     -0.1647      0.6212
```

```
## (Intercept)           X 
##     0.03614     0.59111
```

```
## (Intercept)           X 
##     0.07289     0.61199
```

```
## (Intercept)           X 
##     -0.1949      0.6261
```

```
## (Intercept)           X 
##     -0.2065      0.6294
```

```
## (Intercept)           X 
##     -0.5348      0.6296
```

```
## (Intercept)           X 
##      0.7201      0.5591
```

```
## (Intercept)           X 
##      0.3243      0.5872
```

```
## (Intercept)           X 
##      0.3268      0.5821
```

```
## (Intercept)           X 
##     -0.5003      0.6347
```

```
## (Intercept)           X 
##     -0.3633      0.6201
```

```
## (Intercept)           X 
##     -0.2385      0.5999
```

```
## (Intercept)           X 
##      0.1171      0.6036
```

```
## (Intercept)           X 
##      0.6608      0.5754
```

```
## (Intercept)           X 
##     -0.2847      0.6142
```

```
## (Intercept)           X 
##     -0.1160      0.5954
```

```
## (Intercept)           X 
##     -0.5444      0.6082
```

```
## (Intercept)           X 
##     -0.3196      0.6331
```

```
## (Intercept)           X 
##     -0.1850      0.6055
```

```
## (Intercept)           X 
##      0.6186      0.5696
```

```
## (Intercept)           X 
##     -0.3813      0.6198
```

```
## (Intercept)           X 
##     -0.3394      0.6381
```

```
## (Intercept)           X 
##      0.3439      0.6066
```

```
## (Intercept)           X 
##      0.2303      0.5981
```

```
## (Intercept)           X 
##     -0.4117      0.6039
```

```
## (Intercept)           X 
##     -0.1284      0.6094
```

```
## (Intercept)           X 
##     -0.3225      0.6108
```

```
## (Intercept)           X 
##     -0.4161      0.6274
```

```
## (Intercept)           X 
##      0.1928      0.6048
```

```
## (Intercept)           X 
##      0.0170      0.6137
```

```r
set.seed(20130425)
x1[6, ] <- runAll(alpha = 0.9, xbeta = xbeta, wcorr = "AR-M")
```

```
## (Intercept)           X 
##     0.07617     0.59856
```

```
## (Intercept)           X 
##     -0.5464      0.6192
```

```
## (Intercept)           X 
##     -0.5609      0.6358
```

```
## (Intercept)           X 
##      0.2717      0.5896
```

```
## (Intercept)           X 
##     -0.2006      0.6095
```

```
## (Intercept)           X 
##     0.02111     0.60655
```

```
## (Intercept)           X 
##     -0.1121      0.6149
```

```
## (Intercept)           X 
##      0.2408      0.5884
```

```
## (Intercept)           X 
##      0.3301      0.5890
```

```
## (Intercept)           X 
##      0.3482      0.5573
```

```
## (Intercept)           X 
##    -0.02155     0.60124
```

```
## (Intercept)           X 
##     0.01549     0.58243
```

```
## (Intercept)           X 
##      0.1111      0.5908
```

```
## (Intercept)           X 
##      0.3829      0.5878
```

```
## (Intercept)           X 
##     -0.2237      0.6040
```

```
## (Intercept)           X 
##      0.1763      0.5849
```

```
## (Intercept)           X 
##     -0.2562      0.5883
```

```
## (Intercept)           X 
##     -0.3336      0.6170
```

```
## (Intercept)           X 
##     0.03218     0.59043
```

```
## (Intercept)           X 
##     -0.1641      0.6098
```

```
## (Intercept)           X 
##     -0.3772      0.5859
```

```
## (Intercept)           X 
##    -0.03878     0.60401
```

```
## (Intercept)           X 
##     -0.2089      0.6369
```

```
## (Intercept)           X 
##    0.002555    0.596233
```

```
## (Intercept)           X 
##     -0.6031      0.6193
```

```
## (Intercept)           X 
##      0.2314      0.5878
```

```
## (Intercept)           X 
##     -0.1242      0.5752
```

```
## (Intercept)           X 
##     -0.1152      0.6041
```

```
## (Intercept)           X 
##     -0.2851      0.5974
```

```
## (Intercept)           X 
##     -0.1048      0.6154
```

```
## (Intercept)           X 
##     -0.4227      0.6307
```

```
## (Intercept)           X 
##     0.07248     0.58683
```

```
## (Intercept)           X 
##      0.1969      0.5981
```

```
## (Intercept)           X 
##     0.04756     0.59782
```

```
## (Intercept)           X 
##      0.2773      0.5800
```

```
## (Intercept)           X 
##      0.0842      0.6095
```

```
## (Intercept)           X 
##      0.1033      0.5802
```

```
## (Intercept)           X 
##     -0.1651      0.6148
```

```
## (Intercept)           X 
##     0.08814     0.58867
```

```
## (Intercept)           X 
##      0.1083      0.5742
```

```
## (Intercept)           X 
##     -0.1577      0.6092
```

```
## (Intercept)           X 
##    -0.06467     0.60243
```

```
## (Intercept)           X 
##     -0.1001      0.5920
```

```
## (Intercept)           X 
##     -0.5022      0.6085
```

```
## (Intercept)           X 
##      -0.236       0.613
```

```
## (Intercept)           X 
##      0.4118      0.5584
```

```
## (Intercept)           X 
##      0.2674      0.5927
```

```
## (Intercept)           X 
##      0.3172      0.5862
```

```
## (Intercept)           X 
##     -0.2287      0.6147
```

```
## (Intercept)           X 
##     -0.2084      0.6028
```

```
## (Intercept)           X 
##    -0.03174     0.58855
```

```
## (Intercept)           X 
##     -0.5409      0.6123
```

```
## (Intercept)           X 
##      0.3390      0.5728
```

```
## (Intercept)           X 
##      0.7520      0.5861
```

```
## (Intercept)           X 
##      0.1181      0.5894
```

```
## (Intercept)           X 
##     -0.2482      0.5969
```

```
## (Intercept)           X 
##     -0.1894      0.6044
```

```
## (Intercept)           X 
##     -0.2138      0.6032
```

```
## (Intercept)           X 
##      0.2784      0.5823
```

```
## (Intercept)           X 
##      0.2645      0.5920
```

```
## (Intercept)           X 
##      0.1998      0.5845
```

```
## (Intercept)           X 
##     -0.5248      0.6101
```

```
## (Intercept)           X 
##     -0.1553      0.6102
```

```
## (Intercept)           X 
##    -0.01936     0.58132
```

```
## (Intercept)           X 
##     0.07509     0.58979
```

```
## (Intercept)           X 
##      0.1668      0.5817
```

```
## (Intercept)           X 
##     -0.1909      0.5976
```

```
## (Intercept)           X 
##      0.4980      0.5914
```

```
## (Intercept)           X 
##     -0.1129      0.6058
```

```
## (Intercept)           X 
##     -0.5092      0.6250
```

```
## (Intercept)           X 
##     -0.1647      0.6212
```

```
## (Intercept)           X 
##     0.03614     0.59111
```

```
## (Intercept)           X 
##     0.07289     0.61199
```

```
## (Intercept)           X 
##     -0.1949      0.6261
```

```
## (Intercept)           X 
##     -0.2065      0.6294
```

```
## (Intercept)           X 
##     -0.5348      0.6296
```

```
## (Intercept)           X 
##      0.7201      0.5591
```

```
## (Intercept)           X 
##      0.3243      0.5872
```

```
## (Intercept)           X 
##      0.3268      0.5821
```

```
## (Intercept)           X 
##     -0.5003      0.6347
```

```
## (Intercept)           X 
##     -0.3633      0.6201
```

```
## (Intercept)           X 
##     -0.2385      0.5999
```

```
## (Intercept)           X 
##      0.1171      0.6036
```

```
## (Intercept)           X 
##      0.6608      0.5754
```

```
## (Intercept)           X 
##     -0.2847      0.6142
```

```
## (Intercept)           X 
##     -0.1160      0.5954
```

```
## (Intercept)           X 
##     -0.5444      0.6082
```

```
## (Intercept)           X 
##     -0.3196      0.6331
```

```
## (Intercept)           X 
##     -0.1850      0.6055
```

```
## (Intercept)           X 
##      0.6186      0.5696
```

```
## (Intercept)           X 
##     -0.3813      0.6198
```

```
## (Intercept)           X 
##     -0.3394      0.6381
```

```
## (Intercept)           X 
##      0.3439      0.6066
```

```
## (Intercept)           X 
##      0.2303      0.5981
```

```
## (Intercept)           X 
##     -0.4117      0.6039
```

```
## (Intercept)           X 
##     -0.1284      0.6094
```

```
## (Intercept)           X 
##     -0.3225      0.6108
```

```
## (Intercept)           X 
##     -0.4161      0.6274
```

```
## (Intercept)           X 
##      0.1928      0.6048
```

```
## (Intercept)           X 
##      0.0170      0.6137
```

```r
set.seed(20130425)
x1[7, ] <- runAll(tcorr = "ar1", alpha = 0.5, xbeta = xbeta)
```

```
## (Intercept)           X 
##     0.05047     0.59820
```

```
## (Intercept)           X 
##     -0.8915      0.6564
```

```
## (Intercept)           X 
##     -1.1868      0.6957
```

```
## (Intercept)           X 
##      0.4821      0.5678
```

```
## (Intercept)           X 
##     -0.3899      0.6277
```

```
## (Intercept)           X 
##     -0.1039      0.6162
```

```
## (Intercept)           X 
##     -0.4364      0.6427
```

```
## (Intercept)           X 
##      0.4503      0.5652
```

```
## (Intercept)           X 
##      0.5026      0.5699
```

```
## (Intercept)           X 
##      1.2476      0.4785
```

```
## (Intercept)           X 
##    -0.05872     0.60489
```

```
## (Intercept)           X 
##      0.4120      0.5501
```

```
## (Intercept)           X 
##      0.2895      0.5731
```

```
## (Intercept)           X 
##      0.5867      0.5654
```

```
## (Intercept)           X 
##     -0.2575      0.6100
```

```
## (Intercept)           X 
##      0.4784      0.5582
```

```
## (Intercept)           X 
##     0.09225     0.56249
```

```
## (Intercept)           X 
##     -0.6489      0.6477
```

```
## (Intercept)           X 
##      0.2423      0.5722
```

```
## (Intercept)           X 
##     -0.3648      0.6272
```

```
## (Intercept)           X 
##    -0.01681     0.56053
```

```
## (Intercept)           X 
##     -0.1157      0.6107
```

```
## (Intercept)           X 
##     -1.0084      0.7061
```

```
## (Intercept)           X 
##     0.07816     0.58887
```

```
## (Intercept)           X 
##     -0.9261      0.6549
```

```
## (Intercept)           X 
##      0.4771      0.5651
```

```
## (Intercept)           X 
##      0.4795      0.5257
```

```
## (Intercept)           X 
##     -0.2316      0.6155
```

```
## (Intercept)           X 
##     -0.1866      0.5940
```

```
## (Intercept)           X 
##     -0.4348      0.6447
```

```
## (Intercept)           X 
##     -1.0025      0.6855
```

```
## (Intercept)           X 
##      0.3724      0.5602
```

```
## (Intercept)           X 
##      0.2167      0.5952
```

```
## (Intercept)           X 
##      0.0885      0.5938
```

```
## (Intercept)           X 
##      0.6400      0.5469
```

```
## (Intercept)           X 
##     -0.1260      0.6253
```

```
## (Intercept)           X 
##      0.5427      0.5430
```

```
## (Intercept)           X 
##     -0.4388      0.6388
```

```
## (Intercept)           X 
##      0.3518      0.5653
```

```
## (Intercept)           X 
##      0.7040      0.5232
```

```
## (Intercept)           X 
##     -0.3644      0.6276
```

```
## (Intercept)           X 
##     -0.1088      0.6060
```

```
## (Intercept)           X 
##     0.08013     0.57949
```

```
## (Intercept)           X 
##     -0.6255      0.6268
```

```
## (Intercept)           X 
##     -0.5134      0.6393
```

```
## (Intercept)           X 
##      1.2478      0.4832
```

```
## (Intercept)           X 
##      0.3569      0.5818
```

```
## (Intercept)           X 
##      0.6301      0.5559
```

```
## (Intercept)           X 
##      -0.542       0.645
```

```
## (Intercept)           X 
##     -0.2072      0.6057
```

```
## (Intercept)           X 
##      0.2309      0.5669
```

```
## (Intercept)           X 
##     -0.7218      0.6344
```

```
## (Intercept)           X 
##      0.8558      0.5242
```

```
## (Intercept)           X 
##      0.9620      0.5577
```

```
## (Intercept)           X 
##      0.3382      0.5675
```

```
## (Intercept)           X 
##     -0.1276      0.5900
```

```
## (Intercept)           X 
##     -0.3052      0.6154
```

```
## (Intercept)           X 
##     -0.2783      0.6113
```

```
## (Intercept)           X 
##      0.6557      0.5473
```

```
## (Intercept)           X 
##      0.3558      0.5801
```

```
## (Intercept)           X 
##      0.5162      0.5551
```

```
## (Intercept)           X 
##     -0.6217      0.6261
```

```
## (Intercept)           X 
##     -0.3159      0.6249
```

```
## (Intercept)           X 
##      0.3919      0.5466
```

```
## (Intercept)           X 
##      0.2424      0.5748
```

```
## (Intercept)           X 
##      0.6089      0.5428
```

```
## (Intercept)           X 
##     -0.1225      0.5952
```

```
## (Intercept)           X 
##      0.5712      0.5781
```

```
## (Intercept)           X 
##     -0.2671      0.6184
```

```
## (Intercept)           X 
##     -0.9518      0.6679
```

```
## (Intercept)           X 
##     -0.5977      0.6595
```

```
## (Intercept)           X 
##      0.2302      0.5747
```

```
## (Intercept)           X 
##     -0.1839      0.6340
```

```
## (Intercept)           X 
##     -0.7805      0.6763
```

```
## (Intercept)           X 
##     -0.8254      0.6837
```

```
## (Intercept)           X 
##     -1.0786      0.6817
```

```
## (Intercept)           X 
##      1.4790      0.4859
```

```
## (Intercept)           X 
##      0.4863      0.5688
```

```
## (Intercept)           X 
##      0.6791      0.5478
```

```
## (Intercept)           X 
##     -1.1738      0.6977
```

```
## (Intercept)           X 
##     -0.7604      0.6588
```

```
## (Intercept)           X 
##     -0.1867      0.5988
```

```
## (Intercept)           X 
##   -0.004738    0.611389
```

```
## (Intercept)           X 
##      1.0781      0.5309
```

```
## (Intercept)           X 
##     -0.5015      0.6363
```

```
## (Intercept)           X 
##     0.04024     0.58315
```

```
## (Intercept)           X 
##     -0.6632      0.6253
```

```
## (Intercept)           X 
##     -0.9898      0.6933
```

```
## (Intercept)           X 
##     -0.2389      0.6143
```

```
## (Intercept)           X 
##      1.1616      0.5146
```

```
## (Intercept)           X 
##     -0.7411      0.6557
```

```
## (Intercept)           X 
##     -1.1292      0.7077
```

```
## (Intercept)           X 
##      0.0846      0.6237
```

```
## (Intercept)           X 
##      0.2209      0.5972
```

```
## (Intercept)           X 
##     -0.4298      0.6123
```

```
## (Intercept)           X 
##      -0.269       0.622
```

```
## (Intercept)           X 
##     -0.5067      0.6297
```

```
## (Intercept)           X 
##     -0.9907      0.6813
```

```
## (Intercept)           X 
##     0.04065     0.61631
```

```
## (Intercept)           X 
##     -0.2086      0.6324
```

```r
set.seed(20130425)
x1[8, ] <- runAll(tcorr = "ar1", alpha = 0.5, xbeta = xbeta, wcorr = "exchangeable")
```

```
## (Intercept)           X 
##     0.05047     0.59820
```

```
## (Intercept)           X 
##     -0.8915      0.6564
```

```
## (Intercept)           X 
##     -1.1868      0.6957
```

```
## (Intercept)           X 
##      0.4821      0.5678
```

```
## (Intercept)           X 
##     -0.3899      0.6277
```

```
## (Intercept)           X 
##     -0.1039      0.6162
```

```
## (Intercept)           X 
##     -0.4364      0.6427
```

```
## (Intercept)           X 
##      0.4503      0.5652
```

```
## (Intercept)           X 
##      0.5026      0.5699
```

```
## (Intercept)           X 
##      1.2476      0.4785
```

```
## (Intercept)           X 
##    -0.05872     0.60489
```

```
## (Intercept)           X 
##      0.4120      0.5501
```

```
## (Intercept)           X 
##      0.2895      0.5731
```

```
## (Intercept)           X 
##      0.5867      0.5654
```

```
## (Intercept)           X 
##     -0.2575      0.6100
```

```
## (Intercept)           X 
##      0.4784      0.5582
```

```
## (Intercept)           X 
##     0.09225     0.56249
```

```
## (Intercept)           X 
##     -0.6489      0.6477
```

```
## (Intercept)           X 
##      0.2423      0.5722
```

```
## (Intercept)           X 
##     -0.3648      0.6272
```

```
## (Intercept)           X 
##    -0.01681     0.56053
```

```
## (Intercept)           X 
##     -0.1157      0.6107
```

```
## (Intercept)           X 
##     -1.0084      0.7061
```

```
## (Intercept)           X 
##     0.07816     0.58887
```

```
## (Intercept)           X 
##     -0.9261      0.6549
```

```
## (Intercept)           X 
##      0.4771      0.5651
```

```
## (Intercept)           X 
##      0.4795      0.5257
```

```
## (Intercept)           X 
##     -0.2316      0.6155
```

```
## (Intercept)           X 
##     -0.1866      0.5940
```

```
## (Intercept)           X 
##     -0.4348      0.6447
```

```
## (Intercept)           X 
##     -1.0025      0.6855
```

```
## (Intercept)           X 
##      0.3724      0.5602
```

```
## (Intercept)           X 
##      0.2167      0.5952
```

```
## (Intercept)           X 
##      0.0885      0.5938
```

```
## (Intercept)           X 
##      0.6400      0.5469
```

```
## (Intercept)           X 
##     -0.1260      0.6253
```

```
## (Intercept)           X 
##      0.5427      0.5430
```

```
## (Intercept)           X 
##     -0.4388      0.6388
```

```
## (Intercept)           X 
##      0.3518      0.5653
```

```
## (Intercept)           X 
##      0.7040      0.5232
```

```
## (Intercept)           X 
##     -0.3644      0.6276
```

```
## (Intercept)           X 
##     -0.1088      0.6060
```

```
## (Intercept)           X 
##     0.08013     0.57949
```

```
## (Intercept)           X 
##     -0.6255      0.6268
```

```
## (Intercept)           X 
##     -0.5134      0.6393
```

```
## (Intercept)           X 
##      1.2478      0.4832
```

```
## (Intercept)           X 
##      0.3569      0.5818
```

```
## (Intercept)           X 
##      0.6301      0.5559
```

```
## (Intercept)           X 
##      -0.542       0.645
```

```
## (Intercept)           X 
##     -0.2072      0.6057
```

```
## (Intercept)           X 
##      0.2309      0.5669
```

```
## (Intercept)           X 
##     -0.7218      0.6344
```

```
## (Intercept)           X 
##      0.8558      0.5242
```

```
## (Intercept)           X 
##      0.9620      0.5577
```

```
## (Intercept)           X 
##      0.3382      0.5675
```

```
## (Intercept)           X 
##     -0.1276      0.5900
```

```
## (Intercept)           X 
##     -0.3052      0.6154
```

```
## (Intercept)           X 
##     -0.2783      0.6113
```

```
## (Intercept)           X 
##      0.6557      0.5473
```

```
## (Intercept)           X 
##      0.3558      0.5801
```

```
## (Intercept)           X 
##      0.5162      0.5551
```

```
## (Intercept)           X 
##     -0.6217      0.6261
```

```
## (Intercept)           X 
##     -0.3159      0.6249
```

```
## (Intercept)           X 
##      0.3919      0.5466
```

```
## (Intercept)           X 
##      0.2424      0.5748
```

```
## (Intercept)           X 
##      0.6089      0.5428
```

```
## (Intercept)           X 
##     -0.1225      0.5952
```

```
## (Intercept)           X 
##      0.5712      0.5781
```

```
## (Intercept)           X 
##     -0.2671      0.6184
```

```
## (Intercept)           X 
##     -0.9518      0.6679
```

```
## (Intercept)           X 
##     -0.5977      0.6595
```

```
## (Intercept)           X 
##      0.2302      0.5747
```

```
## (Intercept)           X 
##     -0.1839      0.6340
```

```
## (Intercept)           X 
##     -0.7805      0.6763
```

```
## (Intercept)           X 
##     -0.8254      0.6837
```

```
## (Intercept)           X 
##     -1.0786      0.6817
```

```
## (Intercept)           X 
##      1.4790      0.4859
```

```
## (Intercept)           X 
##      0.4863      0.5688
```

```
## (Intercept)           X 
##      0.6791      0.5478
```

```
## (Intercept)           X 
##     -1.1738      0.6977
```

```
## (Intercept)           X 
##     -0.7604      0.6588
```

```
## (Intercept)           X 
##     -0.1867      0.5988
```

```
## (Intercept)           X 
##   -0.004738    0.611389
```

```
## (Intercept)           X 
##      1.0781      0.5309
```

```
## (Intercept)           X 
##     -0.5015      0.6363
```

```
## (Intercept)           X 
##     0.04024     0.58315
```

```
## (Intercept)           X 
##     -0.6632      0.6253
```

```
## (Intercept)           X 
##     -0.9898      0.6933
```

```
## (Intercept)           X 
##     -0.2389      0.6143
```

```
## (Intercept)           X 
##      1.1616      0.5146
```

```
## (Intercept)           X 
##     -0.7411      0.6557
```

```
## (Intercept)           X 
##     -1.1292      0.7077
```

```
## (Intercept)           X 
##      0.0846      0.6237
```

```
## (Intercept)           X 
##      0.2209      0.5972
```

```
## (Intercept)           X 
##     -0.4298      0.6123
```

```
## (Intercept)           X 
##      -0.269       0.622
```

```
## (Intercept)           X 
##     -0.5067      0.6297
```

```
## (Intercept)           X 
##     -0.9907      0.6813
```

```
## (Intercept)           X 
##     0.04065     0.61631
```

```
## (Intercept)           X 
##     -0.2086      0.6324
```

```r
set.seed(20130425)
x1[9, ] <- runAll(tcorr = "ar1", alpha = 0.5, xbeta = xbeta, wcorr = "AR-M")
```

```
## (Intercept)           X 
##     0.05047     0.59820
```

```
## (Intercept)           X 
##     -0.8915      0.6564
```

```
## (Intercept)           X 
##     -1.1868      0.6957
```

```
## (Intercept)           X 
##      0.4821      0.5678
```

```
## (Intercept)           X 
##     -0.3899      0.6277
```

```
## (Intercept)           X 
##     -0.1039      0.6162
```

```
## (Intercept)           X 
##     -0.4364      0.6427
```

```
## (Intercept)           X 
##      0.4503      0.5652
```

```
## (Intercept)           X 
##      0.5026      0.5699
```

```
## (Intercept)           X 
##      1.2476      0.4785
```

```
## (Intercept)           X 
##    -0.05872     0.60489
```

```
## (Intercept)           X 
##      0.4120      0.5501
```

```
## (Intercept)           X 
##      0.2895      0.5731
```

```
## (Intercept)           X 
##      0.5867      0.5654
```

```
## (Intercept)           X 
##     -0.2575      0.6100
```

```
## (Intercept)           X 
##      0.4784      0.5582
```

```
## (Intercept)           X 
##     0.09225     0.56249
```

```
## (Intercept)           X 
##     -0.6489      0.6477
```

```
## (Intercept)           X 
##      0.2423      0.5722
```

```
## (Intercept)           X 
##     -0.3648      0.6272
```

```
## (Intercept)           X 
##    -0.01681     0.56053
```

```
## (Intercept)           X 
##     -0.1157      0.6107
```

```
## (Intercept)           X 
##     -1.0084      0.7061
```

```
## (Intercept)           X 
##     0.07816     0.58887
```

```
## (Intercept)           X 
##     -0.9261      0.6549
```

```
## (Intercept)           X 
##      0.4771      0.5651
```

```
## (Intercept)           X 
##      0.4795      0.5257
```

```
## (Intercept)           X 
##     -0.2316      0.6155
```

```
## (Intercept)           X 
##     -0.1866      0.5940
```

```
## (Intercept)           X 
##     -0.4348      0.6447
```

```
## (Intercept)           X 
##     -1.0025      0.6855
```

```
## (Intercept)           X 
##      0.3724      0.5602
```

```
## (Intercept)           X 
##      0.2167      0.5952
```

```
## (Intercept)           X 
##      0.0885      0.5938
```

```
## (Intercept)           X 
##      0.6400      0.5469
```

```
## (Intercept)           X 
##     -0.1260      0.6253
```

```
## (Intercept)           X 
##      0.5427      0.5430
```

```
## (Intercept)           X 
##     -0.4388      0.6388
```

```
## (Intercept)           X 
##      0.3518      0.5653
```

```
## (Intercept)           X 
##      0.7040      0.5232
```

```
## (Intercept)           X 
##     -0.3644      0.6276
```

```
## (Intercept)           X 
##     -0.1088      0.6060
```

```
## (Intercept)           X 
##     0.08013     0.57949
```

```
## (Intercept)           X 
##     -0.6255      0.6268
```

```
## (Intercept)           X 
##     -0.5134      0.6393
```

```
## (Intercept)           X 
##      1.2478      0.4832
```

```
## (Intercept)           X 
##      0.3569      0.5818
```

```
## (Intercept)           X 
##      0.6301      0.5559
```

```
## (Intercept)           X 
##      -0.542       0.645
```

```
## (Intercept)           X 
##     -0.2072      0.6057
```

```
## (Intercept)           X 
##      0.2309      0.5669
```

```
## (Intercept)           X 
##     -0.7218      0.6344
```

```
## (Intercept)           X 
##      0.8558      0.5242
```

```
## (Intercept)           X 
##      0.9620      0.5577
```

```
## (Intercept)           X 
##      0.3382      0.5675
```

```
## (Intercept)           X 
##     -0.1276      0.5900
```

```
## (Intercept)           X 
##     -0.3052      0.6154
```

```
## (Intercept)           X 
##     -0.2783      0.6113
```

```
## (Intercept)           X 
##      0.6557      0.5473
```

```
## (Intercept)           X 
##      0.3558      0.5801
```

```
## (Intercept)           X 
##      0.5162      0.5551
```

```
## (Intercept)           X 
##     -0.6217      0.6261
```

```
## (Intercept)           X 
##     -0.3159      0.6249
```

```
## (Intercept)           X 
##      0.3919      0.5466
```

```
## (Intercept)           X 
##      0.2424      0.5748
```

```
## (Intercept)           X 
##      0.6089      0.5428
```

```
## (Intercept)           X 
##     -0.1225      0.5952
```

```
## (Intercept)           X 
##      0.5712      0.5781
```

```
## (Intercept)           X 
##     -0.2671      0.6184
```

```
## (Intercept)           X 
##     -0.9518      0.6679
```

```
## (Intercept)           X 
##     -0.5977      0.6595
```

```
## (Intercept)           X 
##      0.2302      0.5747
```

```
## (Intercept)           X 
##     -0.1839      0.6340
```

```
## (Intercept)           X 
##     -0.7805      0.6763
```

```
## (Intercept)           X 
##     -0.8254      0.6837
```

```
## (Intercept)           X 
##     -1.0786      0.6817
```

```
## (Intercept)           X 
##      1.4790      0.4859
```

```
## (Intercept)           X 
##      0.4863      0.5688
```

```
## (Intercept)           X 
##      0.6791      0.5478
```

```
## (Intercept)           X 
##     -1.1738      0.6977
```

```
## (Intercept)           X 
##     -0.7604      0.6588
```

```
## (Intercept)           X 
##     -0.1867      0.5988
```

```
## (Intercept)           X 
##   -0.004738    0.611389
```

```
## (Intercept)           X 
##      1.0781      0.5309
```

```
## (Intercept)           X 
##     -0.5015      0.6363
```

```
## (Intercept)           X 
##     0.04024     0.58315
```

```
## (Intercept)           X 
##     -0.6632      0.6253
```

```
## (Intercept)           X 
##     -0.9898      0.6933
```

```
## (Intercept)           X 
##     -0.2389      0.6143
```

```
## (Intercept)           X 
##      1.1616      0.5146
```

```
## (Intercept)           X 
##     -0.7411      0.6557
```

```
## (Intercept)           X 
##     -1.1292      0.7077
```

```
## (Intercept)           X 
##      0.0846      0.6237
```

```
## (Intercept)           X 
##      0.2209      0.5972
```

```
## (Intercept)           X 
##     -0.4298      0.6123
```

```
## (Intercept)           X 
##      -0.269       0.622
```

```
## (Intercept)           X 
##     -0.5067      0.6297
```

```
## (Intercept)           X 
##     -0.9907      0.6813
```

```
## (Intercept)           X 
##     0.04065     0.61631
```

```
## (Intercept)           X 
##     -0.2086      0.6324
```

```r
set.seed(20130425)
x1[10, ] <- runAll(tcorr = "ar1", alpha = 0.9, xbeta = xbeta)
```

```
## (Intercept)           X 
##     0.06638     0.59891
```

```
## (Intercept)           X 
##     -0.6364      0.6276
```

```
## (Intercept)           X 
##     -0.6899      0.6476
```

```
## (Intercept)           X 
##      0.3284      0.5844
```

```
## (Intercept)           X 
##     -0.2473      0.6136
```

```
## (Intercept)           X 
##     0.00218     0.60818
```

```
## (Intercept)           X 
##     -0.1821      0.6210
```

```
## (Intercept)           X 
##      0.2915      0.5830
```

```
## (Intercept)           X 
##      0.3727      0.5851
```

```
## (Intercept)           X 
##      0.5398      0.5402
```

```
## (Intercept)           X 
##    -0.03259     0.60229
```

```
## (Intercept)           X 
##     0.09571     0.57544
```

```
## (Intercept)           X 
##      0.1502      0.5868
```

```
## (Intercept)           X 
##      0.4357      0.5830
```

```
## (Intercept)           X 
##     -0.2345      0.6050
```

```
## (Intercept)           X 
##      0.2407      0.5793
```

```
## (Intercept)           X 
##     -0.1864      0.5819
```

```
## (Intercept)           X 
##     -0.4064      0.6235
```

```
## (Intercept)           X 
##     0.07607     0.58636
```

```
## (Intercept)           X 
##     -0.2097      0.6135
```

```
## (Intercept)           X 
##     -0.3203      0.5805
```

```
## (Intercept)           X 
##    -0.05406     0.60534
```

```
## (Intercept)           X 
##     -0.3776      0.6521
```

```
## (Intercept)           X 
##     0.01733     0.59456
```

```
## (Intercept)           X 
##     -0.6871      0.6270
```

```
## (Intercept)           X 
##      0.2900      0.5828
```

```
## (Intercept)           X 
##  -0.0009626   0.5637720
```

```
## (Intercept)           X 
##     -0.1512      0.6073
```

```
## (Intercept)           X 
##     -0.2773      0.5969
```

```
## (Intercept)           X 
##     -0.1753      0.6219
```

```
## (Intercept)           X 
##     -0.5467      0.6422
```

```
## (Intercept)           X 
##      0.1392      0.5806
```

```
## (Intercept)           X 
##      0.2077      0.5976
```

```
## (Intercept)           X 
##     0.05735     0.59696
```

```
## (Intercept)           X 
##      0.3511      0.5735
```

```
## (Intercept)           X 
##     0.04875     0.61260
```

```
## (Intercept)           X 
##      0.1965      0.5720
```

```
## (Intercept)           X 
##     -0.2188      0.6194
```

```
## (Intercept)           X 
##      0.1491      0.5831
```

```
## (Intercept)           X 
##      0.2386      0.5625
```

```
## (Intercept)           X 
##     -0.2089      0.6134
```

```
## (Intercept)           X 
##    -0.07473     0.60302
```

```
## (Intercept)           X 
##    -0.07181     0.58970
```

```
## (Intercept)           X 
##     -0.5484      0.6130
```

```
## (Intercept)           X 
##     -0.3048      0.6191
```

```
## (Intercept)           X 
##      0.5884      0.5424
```

```
## (Intercept)           X 
##      0.2884      0.5908
```

```
## (Intercept)           X 
##      0.4020      0.5787
```

```
## (Intercept)           X 
##     -0.3048      0.6219
```

```
## (Intercept)           X 
##     -0.2098      0.6030
```

```
## (Intercept)           X 
##     0.02033     0.58376
```

```
## (Intercept)           X 
##     -0.5942      0.6170
```

```
## (Intercept)           X 
##      0.4483      0.5626
```

```
## (Intercept)           X 
##      0.8251      0.5794
```

```
## (Intercept)           X 
##      0.1691      0.5842
```

```
## (Intercept)           X 
##     -0.2301      0.5952
```

```
## (Intercept)           X 
##     -0.2272      0.6073
```

```
## (Intercept)           X 
##     -0.2396      0.6053
```

```
## (Intercept)           X 
##      0.3685      0.5743
```

```
## (Intercept)           X 
##      0.2838      0.5899
```

```
## (Intercept)           X 
##      0.2711      0.5780
```

```
## (Intercept)           X 
##     -0.5547      0.6131
```

```
## (Intercept)           X 
##     -0.1838      0.6126
```

```
## (Intercept)           X 
##     0.06137     0.57374
```

```
## (Intercept)           X 
##      0.1018      0.5872
```

```
## (Intercept)           X 
##      0.2729      0.5723
```

```
## (Intercept)           X 
##     -0.1877      0.5975
```

```
## (Intercept)           X 
##      0.5231      0.5890
```

```
## (Intercept)           X 
##     -0.1540      0.6089
```

```
## (Intercept)           X 
##     -0.6079      0.6337
```

```
## (Intercept)           X 
##     -0.2542      0.6294
```

```
## (Intercept)           X 
##     0.07627     0.58755
```

```
## (Intercept)           X 
##     0.02673     0.61674
```

```
## (Intercept)           X 
##     -0.3245      0.6373
```

```
## (Intercept)           X 
##     -0.3368      0.6412
```

```
## (Intercept)           X 
##     -0.6555      0.6404
```

```
## (Intercept)           X 
##      0.8898      0.5436
```

```
## (Intercept)           X 
##      0.3547      0.5842
```

```
## (Intercept)           X 
##      0.4101      0.5744
```

```
## (Intercept)           X 
##     -0.6489      0.6482
```

```
## (Intercept)           X 
##     -0.4577      0.6288
```

```
## (Intercept)           X 
##     -0.2342      0.5995
```

```
## (Intercept)           X 
##     0.09331     0.60551
```

```
## (Intercept)           X 
##      0.7633      0.5659
```

```
## (Intercept)           X 
##     -0.3277      0.6182
```

```
## (Intercept)           X 
##    -0.08118     0.59203
```

```
## (Intercept)           X 
##     -0.5914      0.6123
```

```
## (Intercept)           X 
##     -0.4617      0.6460
```

```
## (Intercept)           X 
##     -0.1965      0.6072
```

```
## (Intercept)           X 
##      0.7434      0.5579
```

```
## (Intercept)           X 
##     -0.4633      0.6275
```

```
## (Intercept)           X 
##     -0.5075      0.6531
```

```
## (Intercept)           X 
##      0.2929      0.6112
```

```
## (Intercept)           X 
##      0.2321      0.5984
```

```
## (Intercept)           X 
##     -0.4307      0.6059
```

```
## (Intercept)           X 
##     -0.1504      0.6113
```

```
## (Intercept)           X 
##     -0.3686      0.6147
```

```
## (Intercept)           X 
##     -0.5519      0.6397
```

```
## (Intercept)           X 
##      0.1647      0.6078
```

```
## (Intercept)           X 
##    -0.01174     0.61653
```

```r
set.seed(20130425)
x1[11, ] <- runAll(tcorr = "ar1", alpha = 0.9, xbeta = xbeta, wcorr = "exchangeable")
```

```
## (Intercept)           X 
##     0.06638     0.59891
```

```
## (Intercept)           X 
##     -0.6364      0.6276
```

```
## (Intercept)           X 
##     -0.6899      0.6476
```

```
## (Intercept)           X 
##      0.3284      0.5844
```

```
## (Intercept)           X 
##     -0.2473      0.6136
```

```
## (Intercept)           X 
##     0.00218     0.60818
```

```
## (Intercept)           X 
##     -0.1821      0.6210
```

```
## (Intercept)           X 
##      0.2915      0.5830
```

```
## (Intercept)           X 
##      0.3727      0.5851
```

```
## (Intercept)           X 
##      0.5398      0.5402
```

```
## (Intercept)           X 
##    -0.03259     0.60229
```

```
## (Intercept)           X 
##     0.09571     0.57544
```

```
## (Intercept)           X 
##      0.1502      0.5868
```

```
## (Intercept)           X 
##      0.4357      0.5830
```

```
## (Intercept)           X 
##     -0.2345      0.6050
```

```
## (Intercept)           X 
##      0.2407      0.5793
```

```
## (Intercept)           X 
##     -0.1864      0.5819
```

```
## (Intercept)           X 
##     -0.4064      0.6235
```

```
## (Intercept)           X 
##     0.07607     0.58636
```

```
## (Intercept)           X 
##     -0.2097      0.6135
```

```
## (Intercept)           X 
##     -0.3203      0.5805
```

```
## (Intercept)           X 
##    -0.05406     0.60534
```

```
## (Intercept)           X 
##     -0.3776      0.6521
```

```
## (Intercept)           X 
##     0.01733     0.59456
```

```
## (Intercept)           X 
##     -0.6871      0.6270
```

```
## (Intercept)           X 
##      0.2900      0.5828
```

```
## (Intercept)           X 
##  -0.0009626   0.5637720
```

```
## (Intercept)           X 
##     -0.1512      0.6073
```

```
## (Intercept)           X 
##     -0.2773      0.5969
```

```
## (Intercept)           X 
##     -0.1753      0.6219
```

```
## (Intercept)           X 
##     -0.5467      0.6422
```

```
## (Intercept)           X 
##      0.1392      0.5806
```

```
## (Intercept)           X 
##      0.2077      0.5976
```

```
## (Intercept)           X 
##     0.05735     0.59696
```

```
## (Intercept)           X 
##      0.3511      0.5735
```

```
## (Intercept)           X 
##     0.04875     0.61260
```

```
## (Intercept)           X 
##      0.1965      0.5720
```

```
## (Intercept)           X 
##     -0.2188      0.6194
```

```
## (Intercept)           X 
##      0.1491      0.5831
```

```
## (Intercept)           X 
##      0.2386      0.5625
```

```
## (Intercept)           X 
##     -0.2089      0.6134
```

```
## (Intercept)           X 
##    -0.07473     0.60302
```

```
## (Intercept)           X 
##    -0.07181     0.58970
```

```
## (Intercept)           X 
##     -0.5484      0.6130
```

```
## (Intercept)           X 
##     -0.3048      0.6191
```

```
## (Intercept)           X 
##      0.5884      0.5424
```

```
## (Intercept)           X 
##      0.2884      0.5908
```

```
## (Intercept)           X 
##      0.4020      0.5787
```

```
## (Intercept)           X 
##     -0.3048      0.6219
```

```
## (Intercept)           X 
##     -0.2098      0.6030
```

```
## (Intercept)           X 
##     0.02033     0.58376
```

```
## (Intercept)           X 
##     -0.5942      0.6170
```

```
## (Intercept)           X 
##      0.4483      0.5626
```

```
## (Intercept)           X 
##      0.8251      0.5794
```

```
## (Intercept)           X 
##      0.1691      0.5842
```

```
## (Intercept)           X 
##     -0.2301      0.5952
```

```
## (Intercept)           X 
##     -0.2272      0.6073
```

```
## (Intercept)           X 
##     -0.2396      0.6053
```

```
## (Intercept)           X 
##      0.3685      0.5743
```

```
## (Intercept)           X 
##      0.2838      0.5899
```

```
## (Intercept)           X 
##      0.2711      0.5780
```

```
## (Intercept)           X 
##     -0.5547      0.6131
```

```
## (Intercept)           X 
##     -0.1838      0.6126
```

```
## (Intercept)           X 
##     0.06137     0.57374
```

```
## (Intercept)           X 
##      0.1018      0.5872
```

```
## (Intercept)           X 
##      0.2729      0.5723
```

```
## (Intercept)           X 
##     -0.1877      0.5975
```

```
## (Intercept)           X 
##      0.5231      0.5890
```

```
## (Intercept)           X 
##     -0.1540      0.6089
```

```
## (Intercept)           X 
##     -0.6079      0.6337
```

```
## (Intercept)           X 
##     -0.2542      0.6294
```

```
## (Intercept)           X 
##     0.07627     0.58755
```

```
## (Intercept)           X 
##     0.02673     0.61674
```

```
## (Intercept)           X 
##     -0.3245      0.6373
```

```
## (Intercept)           X 
##     -0.3368      0.6412
```

```
## (Intercept)           X 
##     -0.6555      0.6404
```

```
## (Intercept)           X 
##      0.8898      0.5436
```

```
## (Intercept)           X 
##      0.3547      0.5842
```

```
## (Intercept)           X 
##      0.4101      0.5744
```

```
## (Intercept)           X 
##     -0.6489      0.6482
```

```
## (Intercept)           X 
##     -0.4577      0.6288
```

```
## (Intercept)           X 
##     -0.2342      0.5995
```

```
## (Intercept)           X 
##     0.09331     0.60551
```

```
## (Intercept)           X 
##      0.7633      0.5659
```

```
## (Intercept)           X 
##     -0.3277      0.6182
```

```
## (Intercept)           X 
##    -0.08118     0.59203
```

```
## (Intercept)           X 
##     -0.5914      0.6123
```

```
## (Intercept)           X 
##     -0.4617      0.6460
```

```
## (Intercept)           X 
##     -0.1965      0.6072
```

```
## (Intercept)           X 
##      0.7434      0.5579
```

```
## (Intercept)           X 
##     -0.4633      0.6275
```

```
## (Intercept)           X 
##     -0.5075      0.6531
```

```
## (Intercept)           X 
##      0.2929      0.6112
```

```
## (Intercept)           X 
##      0.2321      0.5984
```

```
## (Intercept)           X 
##     -0.4307      0.6059
```

```
## (Intercept)           X 
##     -0.1504      0.6113
```

```
## (Intercept)           X 
##     -0.3686      0.6147
```

```
## (Intercept)           X 
##     -0.5519      0.6397
```

```
## (Intercept)           X 
##      0.1647      0.6078
```

```
## (Intercept)           X 
##    -0.01174     0.61653
```

```r
set.seed(20130425)
x1[12, ] <- runAll(tcorr = "ar1", alpha = 0.9, xbeta = xbeta, wcorr = "AR-M")
```

```
## (Intercept)           X 
##     0.06638     0.59891
```

```
## (Intercept)           X 
##     -0.6364      0.6276
```

```
## (Intercept)           X 
##     -0.6899      0.6476
```

```
## (Intercept)           X 
##      0.3284      0.5844
```

```
## (Intercept)           X 
##     -0.2473      0.6136
```

```
## (Intercept)           X 
##     0.00218     0.60818
```

```
## (Intercept)           X 
##     -0.1821      0.6210
```

```
## (Intercept)           X 
##      0.2915      0.5830
```

```
## (Intercept)           X 
##      0.3727      0.5851
```

```
## (Intercept)           X 
##      0.5398      0.5402
```

```
## (Intercept)           X 
##    -0.03259     0.60229
```

```
## (Intercept)           X 
##     0.09571     0.57544
```

```
## (Intercept)           X 
##      0.1502      0.5868
```

```
## (Intercept)           X 
##      0.4357      0.5830
```

```
## (Intercept)           X 
##     -0.2345      0.6050
```

```
## (Intercept)           X 
##      0.2407      0.5793
```

```
## (Intercept)           X 
##     -0.1864      0.5819
```

```
## (Intercept)           X 
##     -0.4064      0.6235
```

```
## (Intercept)           X 
##     0.07607     0.58636
```

```
## (Intercept)           X 
##     -0.2097      0.6135
```

```
## (Intercept)           X 
##     -0.3203      0.5805
```

```
## (Intercept)           X 
##    -0.05406     0.60534
```

```
## (Intercept)           X 
##     -0.3776      0.6521
```

```
## (Intercept)           X 
##     0.01733     0.59456
```

```
## (Intercept)           X 
##     -0.6871      0.6270
```

```
## (Intercept)           X 
##      0.2900      0.5828
```

```
## (Intercept)           X 
##  -0.0009626   0.5637720
```

```
## (Intercept)           X 
##     -0.1512      0.6073
```

```
## (Intercept)           X 
##     -0.2773      0.5969
```

```
## (Intercept)           X 
##     -0.1753      0.6219
```

```
## (Intercept)           X 
##     -0.5467      0.6422
```

```
## (Intercept)           X 
##      0.1392      0.5806
```

```
## (Intercept)           X 
##      0.2077      0.5976
```

```
## (Intercept)           X 
##     0.05735     0.59696
```

```
## (Intercept)           X 
##      0.3511      0.5735
```

```
## (Intercept)           X 
##     0.04875     0.61260
```

```
## (Intercept)           X 
##      0.1965      0.5720
```

```
## (Intercept)           X 
##     -0.2188      0.6194
```

```
## (Intercept)           X 
##      0.1491      0.5831
```

```
## (Intercept)           X 
##      0.2386      0.5625
```

```
## (Intercept)           X 
##     -0.2089      0.6134
```

```
## (Intercept)           X 
##    -0.07473     0.60302
```

```
## (Intercept)           X 
##    -0.07181     0.58970
```

```
## (Intercept)           X 
##     -0.5484      0.6130
```

```
## (Intercept)           X 
##     -0.3048      0.6191
```

```
## (Intercept)           X 
##      0.5884      0.5424
```

```
## (Intercept)           X 
##      0.2884      0.5908
```

```
## (Intercept)           X 
##      0.4020      0.5787
```

```
## (Intercept)           X 
##     -0.3048      0.6219
```

```
## (Intercept)           X 
##     -0.2098      0.6030
```

```
## (Intercept)           X 
##     0.02033     0.58376
```

```
## (Intercept)           X 
##     -0.5942      0.6170
```

```
## (Intercept)           X 
##      0.4483      0.5626
```

```
## (Intercept)           X 
##      0.8251      0.5794
```

```
## (Intercept)           X 
##      0.1691      0.5842
```

```
## (Intercept)           X 
##     -0.2301      0.5952
```

```
## (Intercept)           X 
##     -0.2272      0.6073
```

```
## (Intercept)           X 
##     -0.2396      0.6053
```

```
## (Intercept)           X 
##      0.3685      0.5743
```

```
## (Intercept)           X 
##      0.2838      0.5899
```

```
## (Intercept)           X 
##      0.2711      0.5780
```

```
## (Intercept)           X 
##     -0.5547      0.6131
```

```
## (Intercept)           X 
##     -0.1838      0.6126
```

```
## (Intercept)           X 
##     0.06137     0.57374
```

```
## (Intercept)           X 
##      0.1018      0.5872
```

```
## (Intercept)           X 
##      0.2729      0.5723
```

```
## (Intercept)           X 
##     -0.1877      0.5975
```

```
## (Intercept)           X 
##      0.5231      0.5890
```

```
## (Intercept)           X 
##     -0.1540      0.6089
```

```
## (Intercept)           X 
##     -0.6079      0.6337
```

```
## (Intercept)           X 
##     -0.2542      0.6294
```

```
## (Intercept)           X 
##     0.07627     0.58755
```

```
## (Intercept)           X 
##     0.02673     0.61674
```

```
## (Intercept)           X 
##     -0.3245      0.6373
```

```
## (Intercept)           X 
##     -0.3368      0.6412
```

```
## (Intercept)           X 
##     -0.6555      0.6404
```

```
## (Intercept)           X 
##      0.8898      0.5436
```

```
## (Intercept)           X 
##      0.3547      0.5842
```

```
## (Intercept)           X 
##      0.4101      0.5744
```

```
## (Intercept)           X 
##     -0.6489      0.6482
```

```
## (Intercept)           X 
##     -0.4577      0.6288
```

```
## (Intercept)           X 
##     -0.2342      0.5995
```

```
## (Intercept)           X 
##     0.09331     0.60551
```

```
## (Intercept)           X 
##      0.7633      0.5659
```

```
## (Intercept)           X 
##     -0.3277      0.6182
```

```
## (Intercept)           X 
##    -0.08118     0.59203
```

```
## (Intercept)           X 
##     -0.5914      0.6123
```

```
## (Intercept)           X 
##     -0.4617      0.6460
```

```
## (Intercept)           X 
##     -0.1965      0.6072
```

```
## (Intercept)           X 
##      0.7434      0.5579
```

```
## (Intercept)           X 
##     -0.4633      0.6275
```

```
## (Intercept)           X 
##     -0.5075      0.6531
```

```
## (Intercept)           X 
##      0.2929      0.6112
```

```
## (Intercept)           X 
##      0.2321      0.5984
```

```
## (Intercept)           X 
##     -0.4307      0.6059
```

```
## (Intercept)           X 
##     -0.1504      0.6113
```

```
## (Intercept)           X 
##     -0.3686      0.6147
```

```
## (Intercept)           X 
##     -0.5519      0.6397
```

```
## (Intercept)           X 
##      0.1647      0.6078
```

```
## (Intercept)           X 
##    -0.01174     0.61653
```

```r

## Add relative efficiency
colnames(x1) <- c("Coverage", "VarBeta1")
relEff <- c(x1[1:3, 2]/x1[2, 2], x1[4:6, 2]/x1[5, 2], x1[7:9, 2]/x1[8, 2], x1[10:12, 
    2]/x1[11, 2])
alpha <- rep(rep(c(0.5, 0.9), each = 3), 2)
wcorr <- rep(c("independence", "exchangeable", "AR-1"), 4)
tcorr <- rep(c("exchangeable", "AR-1"), each = 6)
result <- data.frame(cbind(x1, relEff, alpha, wcorr, tcorr))
```


Print the summary of the results

```r
result
```

```
##    Coverage             VarBeta1            relEff alpha        wcorr
## 1       0.9  0.00123177943952216                 1   0.5 independence
## 2       0.9  0.00123177943952216                 1   0.5 exchangeable
## 3       0.9  0.00129109603150457  1.04815520545254   0.5         AR-1
## 4       0.9 0.000246355887904429 0.999999999999984   0.9 independence
## 5       0.9 0.000246355887904433                 1   0.9 exchangeable
## 6      0.91 0.000274444697765231  1.11401720535169   0.9         AR-1
## 7       0.9  0.00199214527431571 0.999999999999997   0.5 independence
## 8       0.9  0.00199214527431571                 1   0.5 exchangeable
## 9       0.9  0.00195354903118664 0.980625788878608   0.5         AR-1
## 10     0.89 0.000482334320703937 0.999999999999986   0.9 independence
## 11     0.89 0.000482334320703944                 1   0.9 exchangeable
## 12      0.9 0.000474371484883907 0.983491044534389   0.9         AR-1
##           tcorr
## 1  exchangeable
## 2  exchangeable
## 3  exchangeable
## 4  exchangeable
## 5  exchangeable
## 6  exchangeable
## 7          AR-1
## 8          AR-1
## 9          AR-1
## 10         AR-1
## 11         AR-1
## 12         AR-1
```





Session info

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
## [1] gee_4.13-18      mvtnorm_0.9-9994 knitr_1.1       
## 
## loaded via a namespace (and not attached):
## [1] digest_0.6.3   evaluate_0.4.3 formatR_0.7    stringr_0.6.2 
## [5] tools_2.15.3
```



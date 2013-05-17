Example about how to infer the interest rate
============================================

Written by [L. Collado-Torres](http://www.biostat.jhsph.edu/~lcollado/#.UZZK3ivF2L0).

# Get predictions

Below I get predictions for the 


```r
## Setup
library(lcollado754)

## Define some data to predict with
pre <- preprocess()
```

```
## Joining by: Amount.Requested, Amount.Funded.By.Investors, Interest.Rate,
## Loan.Length, Loan.Purpose, Debt.To.Income.Ratio, State, Home.Ownership,
## Monthly.Income, FICO.Range, Open.CREDIT.Lines, Revolving.CREDIT.Balance,
## Employment.Length
```

```r
data <- postprocess(data)

topred <- head(data)

## Using the model built-in the package or pass down from
## reproduceAnalysis() using the fresh built model
if (!"model" %in% ls()) model <- NULL
preds <- inferInterestRate(topred = topred, model = model)

## Results
preds
```

```
##    Estimate    SE
## 2    12.149 2.193
## 3    21.571 1.640
## 8    17.831 1.871
## 9    14.353 1.095
## 10    7.788 1.513
## 11   16.760 2.647
```


# Reproducibility

```r
sessionInfo()
```

```
## R version 3.0.0 (2013-04-03)
## Platform: x86_64-apple-darwin10.8.0 (64-bit)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] doSNOW_1.0.6       snow_0.3-12        iterators_1.0.6   
##  [4] foreach_1.4.0      randomForest_4.6-7 cvTools_0.3.2     
##  [7] robustbase_0.9-7   lattice_0.20-15    plyr_1.8          
## [10] car_2.0-17         nnet_7.3-6         MASS_7.3-26       
## [13] markdown_0.5.4     knitr_1.2          lcollado754_0.1   
## 
## loaded via a namespace (and not attached):
## [1] codetools_0.2-8 compiler_3.0.0  digest_0.6.3    evaluate_0.4.3 
## [5] formatR_0.7     grid_3.0.0      stringr_0.6.2   tools_3.0.0
```

```r
print(proc.time())
```

```
##    user  system elapsed 
##  33.307   1.334  44.207
```


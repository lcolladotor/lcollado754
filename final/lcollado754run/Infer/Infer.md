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
topred <- head(postprocess(pre))
```

```
## Removing the earliest credit line information for 4 observations.
```

```r
topred
```

```
##   Amount.Requested Amount.Funded.By.Investors Interest.Rate Loan.Length
## 1            20000                      20000          8.90          36
## 2            19200                      19200         12.12          36
## 3            35000                      35000         21.98          60
## 4            10000                       9975          9.99          36
## 5            12000                      12000         11.71          36
## 6             6000                       6000         15.31          36
##   Debt.To.Income.Ratio Open.CREDIT.Lines Inquiries.in.the.Last.6.Months
## 1                14.90                14                              2
## 2                28.36                12                              1
## 3                23.81                14                              1
## 4                14.30                10                              0
## 5                18.78                11                              0
## 6                20.05                17                              2
##   Issued.Date Earliest.CREDIT.Line Total.CREDIT.Lines State.Region
## 1  2012-11-02           1995-02-25                 43           04
## 2  2013-01-17           2005-07-24                 20           06
## 3  2012-10-23           1998-12-31                 33           09
## 4  2010-11-03           1999-10-29                 16           07
## 5  2011-09-30           1992-04-07                 14           02
## 6  2012-07-25           2000-04-15                 34           01
##   FICO.num Monthly.Income.Log   Loan.Purpose.Mod Home.Ownership.Mod
## 1      737              8.786 debt_consolidation           MORTGAGE
## 2      717              8.430 debt_consolidation           MORTGAGE
## 3      692              9.350 debt_consolidation           MORTGAGE
## 4      697              8.251 debt_consolidation           MORTGAGE
## 5      697              8.069        credit_card               RENT
## 6      672              8.495              other                OWN
##   Employment.Length.Mod Revolving.CREDIT.Balance.log
## 1              < 1 year                        9.566
## 2               2 years                        9.318
## 3               2 years                        9.998
## 4               5 years                        9.143
## 5               9 years                        9.580
## 6               3 years                        9.249
```

```r

## Using the model built-in the package or pass down from
## reproduceAnalysis() using the fresh built model
if (!"model" %in% ls()) model <- NULL
preds <- inferInterestRate(topred = topred, model = model)

## Results
preds
```

```
##   Estimate    SE
## 1    9.691 2.977
## 2   11.939 2.276
## 3   21.511 1.391
## 4   12.121 1.823
## 5   12.479 1.340
## 6   15.336 1.697
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
##  [7] robustbase_0.9-7   lattice_0.20-15    car_2.0-17        
## [10] nnet_7.3-6         MASS_7.3-26        markdown_0.5.4    
## [13] knitr_1.2          plyr_1.8           lcollado754_0.1   
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
##  35.866   1.873 243.404
```


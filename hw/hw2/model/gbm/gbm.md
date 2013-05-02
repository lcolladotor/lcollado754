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

test <- TRUE
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

```r

## Fit gbm
fit.gbm.pois <- gbm(votes.useful ~ ., data = trainC, distribution = "poisson", 
    cv.folds = 10)
```

```
## CV: 1 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.7699          1.5830     0.0010    0.0003
##      2        1.7696          1.5828     0.0010    0.0000
##      3        1.7688          1.5822     0.0010    0.0003
##      4        1.7682          1.5821     0.0010    0.0002
##      5        1.7676          1.5816     0.0010    0.0002
##      6        1.7673          1.5812     0.0010    0.0001
##      7        1.7665          1.5806     0.0010    0.0004
##      8        1.7657          1.5800     0.0010    0.0004
##      9        1.7651          1.5800     0.0010    0.0003
##     10        1.7646          1.5795     0.0010    0.0002
##     20        1.7580          1.5750     0.0010    0.0003
##     40        1.7451          1.5656     0.0010    0.0002
##     60        1.7315          1.5586     0.0010    0.0003
##     80        1.7175          1.5456     0.0010    0.0002
##    100        1.7055          1.5365     0.0010    0.0001
## 
## CV: 2 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.7200          2.0407     0.0010    0.0002
##      2        1.7192          2.0404     0.0010    0.0004
##      3        1.7184          2.0401     0.0010    0.0003
##      4        1.7177          2.0395     0.0010    0.0003
##      5        1.7170          2.0389     0.0010    0.0004
##      6        1.7161          2.0382     0.0010    0.0003
##      7        1.7151          2.0379     0.0010    0.0004
##      8        1.7143          2.0372     0.0010    0.0004
##      9        1.7133          2.0369     0.0010    0.0005
##     10        1.7126          2.0367     0.0010    0.0002
##     20        1.7046          2.0331     0.0010    0.0005
##     40        1.6890          2.0243     0.0010    0.0004
##     60        1.6727          2.0165     0.0010    0.0003
##     80        1.6583          2.0082     0.0010    0.0003
##    100        1.6444          2.0022     0.0010    0.0004
## 
## CV: 3 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.7251          1.9926     0.0010    0.0001
##      2        1.7243          1.9921     0.0010    0.0004
##      3        1.7234          1.9919     0.0010    0.0004
##      4        1.7226          1.9916     0.0010    0.0004
##      5        1.7217          1.9912     0.0010    0.0004
##      6        1.7208          1.9901     0.0010    0.0005
##      7        1.7199          1.9895     0.0010    0.0004
##      8        1.7192          1.9890     0.0010    0.0004
##      9        1.7183          1.9887     0.0010    0.0004
##     10        1.7180          1.9886     0.0010    0.0000
##     20        1.7101          1.9842     0.0010    0.0002
##     40        1.6948          1.9745     0.0010    0.0002
##     60        1.6793          1.9633     0.0010    0.0003
##     80        1.6650          1.9548     0.0010    0.0003
##    100        1.6502          1.9459     0.0010    0.0004
## 
## CV: 4 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.7425          1.8275     0.0010    0.0004
##      2        1.7418          1.8265     0.0010    0.0003
##      3        1.7411          1.8256     0.0010    0.0003
##      4        1.7401          1.8243     0.0010    0.0005
##      5        1.7392          1.8229     0.0010    0.0003
##      6        1.7382          1.8217     0.0010    0.0005
##      7        1.7373          1.8205     0.0010    0.0002
##      8        1.7365          1.8194     0.0010    0.0003
##      9        1.7358          1.8183     0.0010    0.0003
##     10        1.7350          1.8173     0.0010    0.0003
##     20        1.7279          1.8056     0.0010    0.0004
##     40        1.7132          1.7849     0.0010    0.0003
##     60        1.6989          1.7655     0.0010    0.0003
##     80        1.6856          1.7469     0.0010    0.0003
##    100        1.6701          1.7254     0.0010    0.0004
## 
## CV: 5 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.7738          1.5521     0.0010    0.0004
##      2        1.7729          1.5514     0.0010    0.0003
##      3        1.7720          1.5508     0.0010    0.0003
##      4        1.7710          1.5501     0.0010    0.0004
##      5        1.7703          1.5494     0.0010    0.0003
##      6        1.7694          1.5488     0.0010    0.0004
##      7        1.7686          1.5482     0.0010    0.0004
##      8        1.7678          1.5472     0.0010    0.0004
##      9        1.7669          1.5463     0.0010    0.0005
##     10        1.7664          1.5465     0.0010    0.0001
##     20        1.7593          1.5431     0.0010    0.0001
##     40        1.7471          1.5392     0.0010    0.0002
##     60        1.7325          1.5306     0.0010    0.0004
##     80        1.7168          1.5204     0.0010    0.0003
##    100        1.7033          1.5134     0.0010    0.0004
## 
## CV: 6 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.7484          1.7752     0.0010    0.0003
##      2        1.7476          1.7743     0.0010    0.0004
##      3        1.7466          1.7734     0.0010    0.0003
##      4        1.7461          1.7732     0.0010    0.0002
##      5        1.7457          1.7725     0.0010    0.0000
##      6        1.7447          1.7716     0.0010    0.0003
##      7        1.7440          1.7707     0.0010    0.0003
##      8        1.7434          1.7705     0.0010    0.0001
##      9        1.7425          1.7697     0.0010    0.0004
##     10        1.7419          1.7688     0.0010    0.0002
##     20        1.7345          1.7603     0.0010    0.0003
##     40        1.7205          1.7446     0.0010    0.0000
##     60        1.7083          1.7326     0.0010    0.0001
##     80        1.6958          1.7183     0.0010    0.0002
##    100        1.6837          1.7037     0.0010    0.0003
## 
## CV: 7 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.7587          1.6816     0.0010    0.0004
##      2        1.7579          1.6809     0.0010    0.0003
##      3        1.7575          1.6801     0.0010    0.0000
##      4        1.7569          1.6799     0.0010    0.0002
##      5        1.7565          1.6797     0.0010    0.0001
##      6        1.7558          1.6790     0.0010    0.0004
##      7        1.7551          1.6787     0.0010    0.0003
##      8        1.7545          1.6788     0.0010    0.0002
##      9        1.7536          1.6783     0.0010    0.0004
##     10        1.7526          1.6776     0.0010    0.0004
##     20        1.7442          1.6719     0.0010    0.0004
##     40        1.7275          1.6619     0.0010    0.0003
##     60        1.7136          1.6521     0.0010    0.0002
##     80        1.6976          1.6418     0.0010    0.0002
##    100        1.6834          1.6332     0.0010    0.0003
## 
## CV: 8 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.7654          1.6236     0.0010    0.0003
##      2        1.7644          1.6227     0.0010    0.0004
##      3        1.7636          1.6220     0.0010    0.0004
##      4        1.7628          1.6215     0.0010    0.0004
##      5        1.7618          1.6208     0.0010    0.0005
##      6        1.7611          1.6202     0.0010    0.0003
##      7        1.7602          1.6195     0.0010    0.0004
##      8        1.7594          1.6186     0.0010    0.0004
##      9        1.7589          1.6180     0.0010    0.0001
##     10        1.7582          1.6174     0.0010    0.0002
##     20        1.7511          1.6121     0.0010    0.0003
##     40        1.7358          1.6004     0.0010    0.0005
##     60        1.7198          1.5870     0.0010    0.0003
##     80        1.7053          1.5762     0.0010    0.0005
##    100        1.6924          1.5658     0.0010    0.0002
## 
## CV: 9 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.7416          1.8374     0.0010    0.0003
##      2        1.7406          1.8367     0.0010    0.0005
##      3        1.7398          1.8359     0.0010    0.0004
##      4        1.7394          1.8359     0.0010    0.0001
##      5        1.7389          1.8356     0.0010    0.0002
##      6        1.7381          1.8354     0.0010    0.0003
##      7        1.7375          1.8351     0.0010    0.0002
##      8        1.7369          1.8348     0.0010    0.0002
##      9        1.7356          1.8339     0.0010    0.0005
##     10        1.7348          1.8330     0.0010    0.0004
##     20        1.7260          1.8261     0.0010    0.0005
##     40        1.7115          1.8154     0.0010    0.0000
##     60        1.6977          1.8065     0.0010    0.0004
##     80        1.6835          1.7972     0.0010    0.0004
##    100        1.6692          1.7879     0.0010    0.0002
## 
## CV: 10 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.7631          1.6491     0.0010    0.0000
##      2        1.7623          1.6482     0.0010    0.0004
##      3        1.7620          1.6480     0.0010    0.0000
##      4        1.7613          1.6470     0.0010    0.0003
##      5        1.7607          1.6463     0.0010    0.0002
##      6        1.7603          1.6462     0.0010    0.0001
##      7        1.7597          1.6453     0.0010    0.0003
##      8        1.7587          1.6443     0.0010    0.0005
##      9        1.7581          1.6437     0.0010    0.0002
##     10        1.7579          1.6432     0.0010    0.0000
##     20        1.7515          1.6360     0.0010    0.0004
##     40        1.7371          1.6202     0.0010    0.0001
##     60        1.7242          1.6071     0.0010    0.0002
##     80        1.7099          1.5907     0.0010    0.0005
##    100        1.6982          1.5771     0.0010    0.0002
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.7510             nan     0.0010    0.0004
##      2        1.7502             nan     0.0010    0.0004
##      3        1.7498             nan     0.0010    0.0001
##      4        1.7495             nan     0.0010    0.0000
##      5        1.7484             nan     0.0010    0.0005
##      6        1.7476             nan     0.0010    0.0003
##      7        1.7466             nan     0.0010    0.0005
##      8        1.7460             nan     0.0010    0.0002
##      9        1.7451             nan     0.0010    0.0003
##     10        1.7442             nan     0.0010    0.0004
##     20        1.7375             nan     0.0010    0.0004
##     40        1.7232             nan     0.0010    0.0004
##     60        1.7085             nan     0.0010    0.0003
##     80        1.6955             nan     0.0010    0.0000
##    100        1.6817             nan     0.0010    0.0003
```

```r
# summary(fit.gbm.pois)
pretty.gbm.tree(fit.gbm.pois)
```

```
##   SplitVar SplitCodePred LeftNode RightNode MissingNode ErrorReduction
## 0       52     1.875e+02        1         2           3          482.4
## 1       -1    -3.413e-04       -1        -1          -1            0.0
## 2       -1     8.057e-04       -1        -1          -1            0.0
## 3       -1    -8.157e-05       -1        -1          -1            0.0
##   Weight Prediction
## 0    499 -8.157e-05
## 1    386 -3.413e-04
## 2    113  8.057e-04
## 3    499 -8.157e-05
```

```r
show(fit.gbm.pois)
```

```
## gbm(formula = votes.useful ~ ., distribution = "poisson", data = trainC, 
##     cv.folds = 10)
## A gradient boosted model with poisson loss function.
## 100 iterations were performed.
## The best cross-validation iteration was 100.
## There were 65 predictors of which 8 had non-zero influence.
```

```r

## Evaluate with validation data set
pred.pois <- predict(fit.gbm.pois, validateC, type = "response")
```

```
## Using 100 trees...
```

```r
e.gbm.pois <- rmlspe(validateC$votes.useful, pred.pois, includeSE = TRUE)
e.gbm.pois
```

```
## $rmspe
## [1] 0.7009
## 
## $se
## [1] 0.001498
```

```r
summary(pred.pois)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.49    1.49    1.50    1.52    1.53    1.69
```

```r

save(fit.gbm.pois, e.gbm.pois, file = "gbm.Rdata")
```


Reproducibility

```r
print(proc.time())
```

```
##    user  system elapsed 
##   9.364  10.355  19.807
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
## [1] methods   splines   stats     graphics  grDevices utils     datasets 
## [8] base     
## 
## other attached packages:
## [1] cvTools_0.3.2    robustbase_0.9-7 gbm_2.0-8        lattice_0.20-15 
## [5] survival_2.37-4  knitr_1.2       
## 
## loaded via a namespace (and not attached):
## [1] digest_0.6.3   evaluate_0.4.3 formatR_0.7    grid_3.0.0    
## [5] stringr_0.6.2  tools_3.0.0
```


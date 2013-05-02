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

test <- FALSE
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
##      1        1.8677          1.8889     0.0010    0.0003
##      2        1.8672          1.8884     0.0010    0.0003
##      3        1.8667          1.8878     0.0010    0.0003
##      4        1.8661          1.8873     0.0010    0.0003
##      5        1.8656          1.8868     0.0010    0.0003
##      6        1.8651          1.8863     0.0010    0.0003
##      7        1.8646          1.8858     0.0010    0.0003
##      8        1.8640          1.8852     0.0010    0.0003
##      9        1.8635          1.8847     0.0010    0.0003
##     10        1.8629          1.8841     0.0010    0.0003
##     20        1.8576          1.8789     0.0010    0.0003
##     40        1.8474          1.8688     0.0010    0.0003
##     60        1.8374          1.8589     0.0010    0.0002
##     80        1.8277          1.8492     0.0010    0.0002
##    100        1.8184          1.8400     0.0010    0.0002
## 
## CV: 2 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.8708          1.8609     0.0010    0.0003
##      2        1.8703          1.8604     0.0010    0.0003
##      3        1.8697          1.8599     0.0010    0.0003
##      4        1.8692          1.8594     0.0010    0.0003
##      5        1.8686          1.8589     0.0010    0.0003
##      6        1.8681          1.8584     0.0010    0.0003
##      7        1.8675          1.8578     0.0010    0.0003
##      8        1.8670          1.8573     0.0010    0.0003
##      9        1.8664          1.8567     0.0010    0.0003
##     10        1.8659          1.8562     0.0010    0.0003
##     20        1.8605          1.8511     0.0010    0.0003
##     40        1.8502          1.8412     0.0010    0.0002
##     60        1.8402          1.8316     0.0010    0.0003
##     80        1.8304          1.8222     0.0010    0.0002
##    100        1.8211          1.8132     0.0010    0.0002
## 
## CV: 3 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.8707          1.8623     0.0010    0.0003
##      2        1.8701          1.8617     0.0010    0.0003
##      3        1.8696          1.8611     0.0010    0.0003
##      4        1.8690          1.8606     0.0010    0.0003
##      5        1.8685          1.8600     0.0010    0.0003
##      6        1.8679          1.8594     0.0010    0.0003
##      7        1.8674          1.8588     0.0010    0.0003
##      8        1.8668          1.8582     0.0010    0.0003
##      9        1.8663          1.8576     0.0010    0.0003
##     10        1.8658          1.8570     0.0010    0.0003
##     20        1.8604          1.8512     0.0010    0.0003
##     40        1.8501          1.8402     0.0010    0.0003
##     60        1.8403          1.8296     0.0010    0.0002
##     80        1.8308          1.8193     0.0010    0.0002
##    100        1.8216          1.8093     0.0010    0.0002
## 
## CV: 4 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.8684          1.8821     0.0010    0.0003
##      2        1.8679          1.8816     0.0010    0.0003
##      3        1.8674          1.8810     0.0010    0.0003
##      4        1.8668          1.8805     0.0010    0.0003
##      5        1.8663          1.8800     0.0010    0.0003
##      6        1.8657          1.8794     0.0010    0.0003
##      7        1.8652          1.8789     0.0010    0.0003
##      8        1.8646          1.8783     0.0010    0.0003
##      9        1.8641          1.8778     0.0010    0.0003
##     10        1.8635          1.8772     0.0010    0.0003
##     20        1.8581          1.8718     0.0010    0.0003
##     40        1.8476          1.8614     0.0010    0.0003
##     60        1.8375          1.8513     0.0010    0.0002
##     80        1.8279          1.8417     0.0010    0.0002
##    100        1.8186          1.8324     0.0010    0.0002
## 
## CV: 5 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.8690          1.8769     0.0010    0.0003
##      2        1.8685          1.8763     0.0010    0.0003
##      3        1.8679          1.8758     0.0010    0.0003
##      4        1.8674          1.8752     0.0010    0.0003
##      5        1.8668          1.8747     0.0010    0.0003
##      6        1.8663          1.8741     0.0010    0.0003
##      7        1.8658          1.8736     0.0010    0.0003
##      8        1.8652          1.8731     0.0010    0.0003
##      9        1.8647          1.8725     0.0010    0.0003
##     10        1.8642          1.8720     0.0010    0.0003
##     20        1.8588          1.8667     0.0010    0.0003
##     40        1.8485          1.8563     0.0010    0.0002
##     60        1.8384          1.8462     0.0010    0.0003
##     80        1.8288          1.8365     0.0010    0.0002
##    100        1.8195          1.8272     0.0010    0.0002
## 
## CV: 6 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.8701          1.8673     0.0010    0.0003
##      2        1.8695          1.8667     0.0010    0.0003
##      3        1.8690          1.8662     0.0010    0.0003
##      4        1.8685          1.8656     0.0010    0.0003
##      5        1.8679          1.8651     0.0010    0.0003
##      6        1.8674          1.8646     0.0010    0.0003
##      7        1.8668          1.8640     0.0010    0.0003
##      8        1.8663          1.8635     0.0010    0.0003
##      9        1.8657          1.8629     0.0010    0.0003
##     10        1.8652          1.8624     0.0010    0.0003
##     20        1.8598          1.8571     0.0010    0.0003
##     40        1.8494          1.8467     0.0010    0.0003
##     60        1.8393          1.8367     0.0010    0.0003
##     80        1.8295          1.8270     0.0010    0.0002
##    100        1.8202          1.8177     0.0010    0.0002
## 
## CV: 7 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.8682          1.8846     0.0010    0.0003
##      2        1.8677          1.8841     0.0010    0.0003
##      3        1.8671          1.8836     0.0010    0.0003
##      4        1.8666          1.8830     0.0010    0.0003
##      5        1.8660          1.8825     0.0010    0.0003
##      6        1.8655          1.8820     0.0010    0.0003
##      7        1.8649          1.8815     0.0010    0.0003
##      8        1.8644          1.8810     0.0010    0.0003
##      9        1.8639          1.8805     0.0010    0.0003
##     10        1.8633          1.8800     0.0010    0.0003
##     20        1.8581          1.8750     0.0010    0.0003
##     40        1.8477          1.8652     0.0010    0.0003
##     60        1.8376          1.8556     0.0010    0.0002
##     80        1.8278          1.8463     0.0010    0.0002
##    100        1.8184          1.8374     0.0010    0.0002
## 
## CV: 8 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.8723          1.8478     0.0010    0.0003
##      2        1.8718          1.8472     0.0010    0.0003
##      3        1.8712          1.8467     0.0010    0.0003
##      4        1.8706          1.8461     0.0010    0.0003
##      5        1.8701          1.8455     0.0010    0.0003
##      6        1.8695          1.8450     0.0010    0.0003
##      7        1.8690          1.8444     0.0010    0.0003
##      8        1.8684          1.8439     0.0010    0.0003
##      9        1.8679          1.8433     0.0010    0.0003
##     10        1.8673          1.8427     0.0010    0.0003
##     20        1.8619          1.8372     0.0010    0.0003
##     40        1.8516          1.8266     0.0010    0.0002
##     60        1.8415          1.8163     0.0010    0.0002
##     80        1.8320          1.8065     0.0010    0.0002
##    100        1.8226          1.7970     0.0010    0.0002
## 
## CV: 9 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.8707          1.8626     0.0010    0.0003
##      2        1.8701          1.8620     0.0010    0.0003
##      3        1.8696          1.8615     0.0010    0.0003
##      4        1.8691          1.8610     0.0010    0.0003
##      5        1.8685          1.8605     0.0010    0.0003
##      6        1.8680          1.8600     0.0010    0.0003
##      7        1.8674          1.8594     0.0010    0.0003
##      8        1.8669          1.8589     0.0010    0.0003
##      9        1.8664          1.8584     0.0010    0.0003
##     10        1.8658          1.8579     0.0010    0.0003
##     20        1.8603          1.8526     0.0010    0.0003
##     40        1.8499          1.8425     0.0010    0.0003
##     60        1.8398          1.8328     0.0010    0.0002
##     80        1.8299          1.8233     0.0010    0.0002
##    100        1.8205          1.8142     0.0010    0.0002
## 
## CV: 10 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.8703          1.8656     0.0010    0.0003
##      2        1.8697          1.8651     0.0010    0.0003
##      3        1.8692          1.8645     0.0010    0.0003
##      4        1.8686          1.8639     0.0010    0.0003
##      5        1.8680          1.8633     0.0010    0.0003
##      6        1.8675          1.8628     0.0010    0.0003
##      7        1.8669          1.8622     0.0010    0.0003
##      8        1.8664          1.8617     0.0010    0.0003
##      9        1.8658          1.8611     0.0010    0.0003
##     10        1.8653          1.8606     0.0010    0.0003
##     20        1.8598          1.8551     0.0010    0.0003
##     40        1.8494          1.8445     0.0010    0.0002
##     60        1.8392          1.8342     0.0010    0.0003
##     80        1.8295          1.8243     0.0010    0.0002
##    100        1.8202          1.8148     0.0010    0.0002
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.8698             nan     0.0010    0.0003
##      2        1.8693             nan     0.0010    0.0003
##      3        1.8687             nan     0.0010    0.0003
##      4        1.8682             nan     0.0010    0.0003
##      5        1.8676             nan     0.0010    0.0003
##      6        1.8671             nan     0.0010    0.0003
##      7        1.8665             nan     0.0010    0.0003
##      8        1.8660             nan     0.0010    0.0003
##      9        1.8655             nan     0.0010    0.0003
##     10        1.8650             nan     0.0010    0.0003
##     20        1.8596             nan     0.0010    0.0003
##     40        1.8494             nan     0.0010    0.0002
##     60        1.8393             nan     0.0010    0.0002
##     80        1.8296             nan     0.0010    0.0002
##    100        1.8202             nan     0.0010    0.0002
```

```r
# summary(fit.gbm.pois)
pretty.gbm.tree(fit.gbm.pois)
```

```
##   SplitVar SplitCodePred LeftNode RightNode MissingNode ErrorReduction
## 0       52     1.315e+02        1         2           3          30875
## 1       -1    -3.521e-04       -1        -1          -1              0
## 2       -1     5.729e-04       -1        -1          -1              0
## 3       -1    -9.413e-05       -1        -1          -1              0
##   Weight Prediction
## 0  70272 -9.413e-05
## 1  50675 -3.521e-04
## 2  19597  5.729e-04
## 3  70272 -9.413e-05
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
## There were 65 predictors of which 1 had non-zero influence.
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
## [1] 0.6777
## 
## $se
## [1] 0.001544
```

```r
summary(pred.pois)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.34    1.34    1.34    1.37    1.40    1.46
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
## 575.186   4.444 580.262
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


Process the text from reviews
=============================



```r
## Load libraries
library(tau)

## Load data
load("../readData/testData.Rdata")
load("../readData/trainingData.Rdata")
```


Process text for the training data to find the top 200 words

```r
tokenNLP <- function(input, top.words = NULL, n = 200) {
    ## Tokenize
    text <- as.character(input)
    words <- lapply(text, tokenize)
    
    ## Find the top n words
    if (is.null(top.words)) {
        all <- unlist(words)
        top <- sort(table(all), decreasing = TRUE)[1:n]
        top.words <- names(top)
    }
    
    ## Count how many were found per review
    words.found <- lapply(words, function(x) {
        sapply(top.words, function(y) {
            sum(x %in% y)
        })
        
    })
    
    ## Re-arrange the results in a data.frame
    words.mat <- do.call(rbind, words.found)
    result <- data.frame(words.mat)
    colnames(result) <- colnames(words.mat)
    
    ## Done
    return(result)
}

## Process the text for the training data
trainingText <- tokenNLP(trainingData$review$text)

## Process the text for the test Data
testText <- tokenNLP(testData$review$text, top.words = colnames(trainingText))
```


Tidy the data

```r
## Keep only the categories shared in the test and training data (Room for
## improving to doing this from the beginning or using a different
## %cutoff)
common <- table(c(colnames(testData$business), colnames(trainingData$business)))
keep <- names(common[common > 1])

## Training data
trainingProcessed <- trainingData
trainingProcessed$business <- trainingProcessed$business[, keep]
trainingProcessed$review <- cbind(trainingProcessed$review[c("votes.funny", 
    "votes.useful", "votes.cool", "votes.total", "user_id", "review_id", "business_id", 
    "stars", "date", "type")], trainingText)
trainingProcessed$review$date <- as.Date(trainingProcessed$review$date)

## Test data (no votes)
testProcessed <- testData
testProcessed$business <- testProcessed$business[, keep]
testProcessed$review <- cbind(testProcessed$review[c("user_id", "review_id", 
    "business_id", "stars", "date", "type")], testText)
```


Save the results

```r
save(testProcessed, file = "testProcessed.Rdata")
save(trainingProcessed, file = "trainingProcessed.Rdata")

print(proc.time())
```

```
##    user  system elapsed 
## 2248.37   17.22 2267.66
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
## [1] stats     graphics  grDevices utils     datasets  base     
## 
## other attached packages:
## [1] tau_0.0-15 knitr_1.2 
## 
## loaded via a namespace (and not attached):
## [1] digest_0.6.3   evaluate_0.4.3 formatR_0.7    stringr_0.6.2 
## [5] tools_3.0.0
```



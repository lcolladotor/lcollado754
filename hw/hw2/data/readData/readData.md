Process data into R
===================


```r
## Load required libraries
library(plyr)
library(RJSONIO)
library(reshape2)
library(doMC)
```

```
## Loading required package: foreach
```

```
## Loading required package: iterators
```

```
## Loading required package: parallel
```

```r

## Specify the number of cores
registerDoMC(4)

## Check how many cores we are using
getDoParWorkers()
```

```
## [1] 4
```

```r

## Specify the main paths
testPath <- "/Users/lcollado/Google Drive/Yelp-Recruit/yelp_test_set"
trainingPath <- "/Users/lcollado/Google Drive/Yelp-Recruit/yelp_training_set"

## This function processes the columns of the initial data.frame read in
## from the JSON format (they are lists).  It does more processing in the
## case of multi-level lists such as the categories in business data file.
fExtract <- function(lraw, checkin, review) {
    ## Replace NULL's by NA's
    l <- lapply(lraw, function(x) {
        res <- x
        if (length(x) == 0) {
            res <- NA
        }
        return(res)
    })
    ul <- unlist(l)
    if (length(ul) > length(l)) {
        if (checkin) {
            l <- lapply(l, function(y) {
                rep(names(y), y)
            })
        }
        if (review) {
            l.cast <- do.call(rbind, l)
        } else {
            l.melt <- melt(l, as.matrix)
            l.cast <- dcast(l.melt, L1 ~ value)
            ## Remove L1 and NA columns
            l.cast <- l.cast[, -c(1, ncol(l.cast))]
        }
        
        if (checkin | review) {
            l.top <- colnames(l.cast)
            l.total <- rowSums(l.cast)
            use <- l.cast
        } else {
            ## Convert to logic matrix
            l.logic <- apply(l.cast, 1:2, function(x) {
                !is.na(x)
            })
            ## Keep only the columns present more than 5% (or all for checkins),
            ## record the total number of columns
            l.na <- colMeans(l.logic)
            l.top <- names(sort(l.na[l.na >= 0.05], decreasing = TRUE))
            l.total <- rowSums(l.logic)
            use <- l.logic
        }
        ## Choose the names of the new columns: remove spaces and other special
        ## symbols
        names(l.top) <- gsub("[ |&|(|)]", "", l.top)
        new <- c(lapply(l.top, function(x) {
            use[, x]
        }), list(total = l.total))
    } else {
        new <- ul
    }
    return(new)
}

## Function that reads in the json files
readJSON <- function(dirPath, fileName, parallel = FALSE, checkin = FALSE, review = FALSE) {
    ## Complete the full file path
    url <- paste(dirPath, fileName, sep = "/")
    
    ## Read by lines
    con <- file(url, "r")
    input <- readLines(con, -1L)
    close(con)
    
    ## Transform from JSON format into lists
    data.list <- lapply(input, function(x) {
        fromJSON(x, nullValue = NA)
    })
    
    ## Reshape as a data.frame
    data.mat <- do.call(rbind, data.list)
    data.df <- data.frame(data.mat, stringsAsFactors = FALSE)
    
    ## Fix the data.frame so all columns are character vectors and not lists
    data.processed <- llply(data.df, fExtract, checkin = checkin, review = review, 
        .parallel = parallel)
    result <- as.data.frame(data.processed)
    
    ## Done
    return(result)
}

## Read in the test files
testFiles <- list.files(testPath)
names(testFiles) <- gsub("(^.*set_)|(.json)", "", testFiles)
testData <- lapply(testFiles, function(x) {
    checkin <- grepl("checkin", x)
    print(x)
    res <- readJSON(testPath, x, parallel = TRUE, checkin = checkin)
})
```

```
## [1] "yelp_test_set_business.json"
## [1] "yelp_test_set_checkin.json"
## [1] "yelp_test_set_review.json"
## [1] "yelp_test_set_user.json"
```

```r
save(testData, file = "testData.Rdata")

## Read the training files
trainingFiles <- list.files(trainingPath)
names(trainingFiles) <- gsub("(^.*set_)|(.json)", "", trainingFiles)
trainingData <- lapply(trainingFiles, function(x) {
    checkin <- grepl("checkin", x)
    review <- grepl("review|user", x)  # due to the votes variable
    print(x)
    readJSON(trainingPath, x, parallel = TRUE, checkin = checkin, review = review)
})
```

```
## [1] "yelp_training_set_business.json"
## [1] "yelp_training_set_checkin.json"
## [1] "yelp_training_set_review.json"
## [1] "yelp_training_set_user.json"
```

```r
save(trainingData, file = "trainingData.Rdata")


print(proc.time())
```

```
##    user  system elapsed 
##  314.52   26.41  324.48
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
## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
## [1] doMC_1.3.0      iterators_1.0.6 foreach_1.4.0   reshape2_1.2.2 
## [5] RJSONIO_1.0-3   plyr_1.8        knitr_1.1      
## 
## loaded via a namespace (and not attached):
## [1] codetools_0.2-8 compiler_2.15.3 digest_0.6.3    evaluate_0.4.3 
## [5] formatR_0.7     stringr_0.6.2   tools_2.15.3
```




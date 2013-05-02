Add the variable to predict and separate the training data into validation and train subsets
===========================


```r
## Load data
load("../compressReview-small-quick/trainingReview.Rdata")
review <- trainingReview

## Testing
test <- FALSE
if (test) {
    review <- review[1:1000, ]
}

## Remove votes columns that won't be used
review <- review[, !colnames(review) %in% c("votes.funny", "votes.cool", "votes.total")]

## Sample with prob 0.3 for validation, 0.7 for testing
set.seed(20130501)
idx <- sample(1:2, size = nrow(review), replace = TRUE, prob = c(0.3, 0.7))
table(idx)
```

```
## idx
##      1      2 
##  68753 161154
```

```r

validate <- review[idx == 1, ]
train <- review[idx == 2, ]

print(object.size(validate), units = "Mb")
```

```
## 48 Mb
```

```r
print(object.size(train), units = "Mb")
```

```
## 82.5 Mb
```

```r
save(validate, train, idx, file = "train.Rdata")
```


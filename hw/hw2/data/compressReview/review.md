Process the data by review to then use for model selection and prediction
========================

Load and explore the data

```r
## Libraries
library(plyr)
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

## Set up for using plyr in parallel
registerDoMC(4)
getDoParWorkers()
```

```
## [1] 4
```

```r

## Read data
load("../processReview/testProcessed.Rdata")
load("../processReview/trainingProcessed.Rdata")

## Explore it lapply(testProcessed, head)
lapply(testProcessed, summary)
```

```
## $business
##                  business_id   categories.BeautySpas categories.Food
##  _0DaDbjr4iKxqWoN7EvNzA:   1   Mode :logical         Mode :logical  
##  _0F9ZpDmHbH45kM7ujD9Fg:   1   FALSE:1095            FALSE:1073     
##  _7OvxfAje3XDZuSzguF6iA:   1   TRUE :110             TRUE :132      
##  _CblT1wpw8bbZISdOy9lkQ:   1   NA's :0               NA's :0        
##  _CVcmMqTUequ2u6fbJ48vQ:   1                                        
##  _fmMzXSxtD996PgEUsJ3Sw:   1                                        
##  (Other)               :1199                                        
##  categories.Restaurants categories.Shopping categories.total
##  Mode :logical          Mode :logical       Min.   :0.00    
##  FALSE:880              FALSE:1026          1st Qu.:2.00    
##  TRUE :325              TRUE :179           Median :2.00    
##  NA's :0                NA's :0             Mean   :2.66    
##                                             3rd Qu.:3.00    
##                                             Max.   :9.00    
##                                                             
##          city                                     full_address 
##  Phoenix   :447   Phoenix, AZ                           :   9  
##  Scottsdale:189   21001 N Tatum Blvd\nPhoenix, AZ 85050 :   4  
##  Mesa      :111   Phoenix, AZ 85003                     :   3  
##  Tempe     :106   12027 N 28th Dr\nPhoenix, AZ 85029    :   2  
##  Chandler  : 71   2502 E Camelback Rd\nPhoenix, AZ 85016:   2  
##  Glendale  : 53   2922 N Hayden Rd\nScottsdale, AZ 85251:   2  
##  (Other)   :228   (Other)                               :1183  
##     latitude      longitude                  name      neighborhoods 
##  Min.   :32.9   Min.   :-113   Walgreens       :   6   Mode:logical  
##  1st Qu.:33.4   1st Qu.:-112   Jimmy John's    :   5   NA's:1205     
##  Median :33.5   Median :-112   Starbucks       :   5                 
##  Mean   :33.5   Mean   :-112   Starbucks Coffee:   5                 
##  3rd Qu.:33.6   3rd Qu.:-112   Subway          :   5                 
##  Max.   :34.0   Max.   :-111   Walmart         :   5                 
##                                (Other)         :1174                 
##     open          review_count        stars      state           type     
##  Mode :logical   Min.   :  3.00   Min.   :1.00   AZ:1205   business:1205  
##  FALSE:25        1st Qu.:  3.00   1st Qu.:3.00                            
##  TRUE :1180      Median :  3.00   Median :4.00                            
##  NA's :0         Mean   :  9.19   Mean   :3.76                            
##                  3rd Qu.:  7.00   3rd Qu.:4.50                            
##                  Max.   :155.00   Max.   :5.00                            
##                                                                           
## 
## $checkin
##  checkin_info.0.4 checkin_info.1.4 checkin_info.1.6 checkin_info.13.5
##  Min.   :0.000    Min.   :0.000    Min.   :0.000    Min.   : 0.000   
##  1st Qu.:0.000    1st Qu.:0.000    1st Qu.:0.000    1st Qu.: 0.000   
##  Median :0.000    Median :0.000    Median :0.000    Median : 0.000   
##  Mean   :0.029    Mean   :0.016    Mean   :0.038    Mean   : 0.649   
##  3rd Qu.:0.000    3rd Qu.:0.000    3rd Qu.:0.000    3rd Qu.: 1.000   
##  Max.   :4.000    Max.   :5.000    Max.   :5.000    Max.   :15.000   
##                                                                      
##  checkin_info.15.3 checkin_info.18.6 checkin_info.19.5 checkin_info.2.3
##  Min.   :0.000     Min.   : 0.000    Min.   : 0.00     Min.   :0.0000  
##  1st Qu.:0.000     1st Qu.: 0.000    1st Qu.: 0.00     1st Qu.:0.0000  
##  Median :0.000     Median : 0.000    Median : 0.00     Median :0.0000  
##  Mean   :0.301     Mean   : 0.504    Mean   : 0.61     Mean   :0.0123  
##  3rd Qu.:0.000     3rd Qu.: 0.000    3rd Qu.: 0.00     3rd Qu.:0.0000  
##  Max.   :9.000     Max.   :12.000    Max.   :13.00     Max.   :2.0000  
##                                                                        
##  checkin_info.20.5 checkin_info.21.3 checkin_info.0.1 checkin_info.0.2
##  Min.   : 0.000    Min.   :0.000     Min.   :0.0000   Min.   :0.000   
##  1st Qu.: 0.000    1st Qu.:0.000     1st Qu.:0.0000   1st Qu.:0.000   
##  Median : 0.000    Median :0.000     Median :0.0000   Median :0.000   
##  Mean   : 0.428    Mean   :0.192     Mean   :0.0313   Mean   :0.034   
##  3rd Qu.: 0.000    3rd Qu.:0.000     3rd Qu.:0.0000   3rd Qu.:0.000   
##  Max.   :15.000    Max.   :7.000     Max.   :3.0000   Max.   :5.000   
##                                                                       
##  checkin_info.0.3 checkin_info.0.6 checkin_info.1.3 checkin_info.1.5
##  Min.   :0.0000   Min.   :0.000    Min.   :0.0000   Min.   :0.000   
##  1st Qu.:0.0000   1st Qu.:0.000    1st Qu.:0.0000   1st Qu.:0.000   
##  Median :0.0000   Median :0.000    Median :0.0000   Median :0.000   
##  Mean   :0.0218   Mean   :0.076    Mean   :0.0191   Mean   :0.046   
##  3rd Qu.:0.0000   3rd Qu.:0.000    3rd Qu.:0.0000   3rd Qu.:0.000   
##  Max.   :2.0000   Max.   :5.000    Max.   :2.0000   Max.   :6.000   
##                                                                     
##  checkin_info.10.0 checkin_info.11.5 checkin_info.12.1 checkin_info.12.5
##  Min.   :0.000     Min.   : 0.000    Min.   : 0.00     Min.   : 0.00    
##  1st Qu.:0.000     1st Qu.: 0.000    1st Qu.: 0.00     1st Qu.: 0.00    
##  Median :0.000     Median : 0.000    Median : 0.00     Median : 0.00    
##  Mean   :0.309     Mean   : 0.676    Mean   : 0.51     Mean   : 0.73    
##  3rd Qu.:0.000     3rd Qu.: 1.000    3rd Qu.: 1.00     3rd Qu.: 1.00    
##  Max.   :7.000     Max.   :25.000    Max.   :46.00     Max.   :17.00    
##                                                                         
##  checkin_info.12.6 checkin_info.13.1 checkin_info.13.2 checkin_info.13.6
##  Min.   : 0.00     Min.   : 0.000    Min.   :0.000     Min.   : 0.000   
##  1st Qu.: 0.00     1st Qu.: 0.000    1st Qu.:0.000     1st Qu.: 0.000   
##  Median : 0.00     Median : 0.000    Median :0.000     Median : 0.000   
##  Mean   : 0.64     Mean   : 0.338    Mean   :0.331     Mean   : 0.553   
##  3rd Qu.: 1.00     3rd Qu.: 0.000    3rd Qu.:0.000     3rd Qu.: 1.000   
##  Max.   :25.00     Max.   :16.000    Max.   :9.000     Max.   :16.000   
##                                                                         
##  checkin_info.14.0 checkin_info.14.1 checkin_info.15.2 checkin_info.15.5
##  Min.   :0.000     Min.   :0.000     Min.   :0.000     Min.   : 0.000   
##  1st Qu.:0.000     1st Qu.:0.000     1st Qu.:0.000     1st Qu.: 0.000   
##  Median :0.000     Median :0.000     Median :0.000     Median : 0.000   
##  Mean   :0.267     Mean   :0.247     Mean   :0.287     Mean   : 0.518   
##  3rd Qu.:0.000     3rd Qu.:0.000     3rd Qu.:0.000     3rd Qu.: 1.000   
##  Max.   :6.000     Max.   :6.000     Max.   :7.000     Max.   :14.000   
##                                                                         
##  checkin_info.16.4 checkin_info.16.6 checkin_info.17.0 checkin_info.17.1
##  Min.   : 0.000    Min.   : 0.000    Min.   :0.000     Min.   : 0.000   
##  1st Qu.: 0.000    1st Qu.: 0.000    1st Qu.:0.000     1st Qu.: 0.000   
##  Median : 0.000    Median : 0.000    Median :0.000     Median : 0.000   
##  Mean   : 0.488    Mean   : 0.399    Mean   :0.401     Mean   : 0.516   
##  3rd Qu.: 1.000    3rd Qu.: 0.000    3rd Qu.:0.000     3rd Qu.: 1.000   
##  Max.   :10.000    Max.   :12.000    Max.   :7.000     Max.   :11.000   
##                                                                         
##  checkin_info.17.3 checkin_info.19.1 checkin_info.19.2 checkin_info.19.3
##  Min.   : 0.000    Min.   : 0.000    Min.   : 0.000    Min.   : 0.000   
##  1st Qu.: 0.000    1st Qu.: 0.000    1st Qu.: 0.000    1st Qu.: 0.000   
##  Median : 0.000    Median : 0.000    Median : 0.000    Median : 0.000   
##  Mean   : 0.621    Mean   : 0.406    Mean   : 0.481    Mean   : 0.484   
##  3rd Qu.: 1.000    3rd Qu.: 0.000    3rd Qu.: 0.000    3rd Qu.: 0.000   
##  Max.   :18.000    Max.   :13.000    Max.   :13.000    Max.   :13.000   
##                                                                         
##  checkin_info.19.6 checkin_info.2.2 checkin_info.2.5 checkin_info.20.2
##  Min.   : 0.000    Min.   :0.0000   Min.   :0.0000   Min.   :0.000    
##  1st Qu.: 0.000    1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.000    
##  Median : 0.000    Median :0.0000   Median :0.0000   Median :0.000    
##  Mean   : 0.311    Mean   :0.0123   Mean   :0.0232   Mean   :0.292    
##  3rd Qu.: 0.000    3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.000    
##  Max.   :11.000    Max.   :2.0000   Max.   :2.0000   Max.   :9.000    
##                                                                       
##  checkin_info.20.3 checkin_info.21.0 checkin_info.21.6 checkin_info.22.1
##  Min.   : 0.000    Min.   : 0.000    Min.   :0.000     Min.   :0.000    
##  1st Qu.: 0.000    1st Qu.: 0.000    1st Qu.:0.000     1st Qu.:0.000    
##  Median : 0.000    Median : 0.000    Median :0.000     Median :0.000    
##  Mean   : 0.292    Mean   : 0.112    Mean   :0.091     Mean   :0.075    
##  3rd Qu.: 0.000    3rd Qu.: 0.000    3rd Qu.:0.000     3rd Qu.:0.000    
##  Max.   :10.000    Max.   :11.000    Max.   :7.000     Max.   :6.000    
##                                                                         
##  checkin_info.22.3 checkin_info.22.4 checkin_info.23.2 checkin_info.23.6
##  Min.   :0.000     Min.   : 0.000    Min.   :0.0000    Min.   :0.00     
##  1st Qu.:0.000     1st Qu.: 0.000    1st Qu.:0.0000    1st Qu.:0.00     
##  Median :0.000     Median : 0.000    Median :0.0000    Median :0.00     
##  Mean   :0.094     Mean   : 0.198    Mean   :0.0368    Mean   :0.03     
##  3rd Qu.:0.000     3rd Qu.: 0.000    3rd Qu.:0.0000    3rd Qu.:0.00     
##  Max.   :4.000     Max.   :11.000    Max.   :3.0000    Max.   :2.00     
##                                                                         
##  checkin_info.7.0 checkin_info.0.5 checkin_info.10.4 checkin_info.15.1
##  Min.   :0.000    Min.   :0.000    Min.   : 0.000    Min.   :0.000    
##  1st Qu.:0.000    1st Qu.:0.000    1st Qu.: 0.000    1st Qu.:0.000    
##  Median :0.000    Median :0.000    Median : 0.000    Median :0.000    
##  Mean   :0.143    Mean   :0.065    Mean   : 0.373    Mean   :0.292    
##  3rd Qu.:0.000    3rd Qu.:0.000    3rd Qu.: 0.000    3rd Qu.:0.000    
##  Max.   :9.000    Max.   :6.000    Max.   :14.000    Max.   :5.000    
##                                                                       
##  checkin_info.16.3 checkin_info.20.4 checkin_info.21.4 checkin_info.22.2
##  Min.   : 0.00     Min.   : 0.000    Min.   : 0.000    Min.   :0.00     
##  1st Qu.: 0.00     1st Qu.: 0.000    1st Qu.: 0.000    1st Qu.:0.00     
##  Median : 0.00     Median : 0.000    Median : 0.000    Median :0.00     
##  Mean   : 0.42     Mean   : 0.475    Mean   : 0.305    Mean   :0.03     
##  3rd Qu.: 0.00     3rd Qu.: 0.000    3rd Qu.: 0.000    3rd Qu.:0.00     
##  Max.   :13.00     Max.   :12.000    Max.   :12.000    Max.   :3.00     
##                                                                         
##  checkin_info.22.5 checkin_info.23.4 checkin_info.23.5 checkin_info.8.5
##  Min.   : 0.000    Min.   :0.000     Min.   :0.000     Min.   : 0.000  
##  1st Qu.: 0.000    1st Qu.:0.000     1st Qu.:0.000     1st Qu.: 0.000  
##  Median : 0.000    Median :0.000     Median :0.000     Median : 0.000  
##  Mean   : 0.151    Mean   :0.101     Mean   :0.109     Mean   : 0.343  
##  3rd Qu.: 0.000    3rd Qu.:0.000     3rd Qu.:0.000     3rd Qu.: 0.000  
##  Max.   :10.000    Max.   :7.000     Max.   :9.000     Max.   :27.000  
##                                                                        
##  checkin_info.0.0 checkin_info.1.0 checkin_info.11.1 checkin_info.11.4
##  Min.   :0.0000   Min.   :0.0000   Min.   : 0.00     Min.   : 0.00    
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.: 0.00     1st Qu.: 0.00    
##  Median :0.0000   Median :0.0000   Median : 0.00     Median : 0.00    
##  Mean   :0.0218   Mean   :0.0136   Mean   : 0.55     Mean   : 0.64    
##  3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.: 0.00     3rd Qu.: 1.00    
##  Max.   :2.0000   Max.   :2.0000   Max.   :48.00     Max.   :36.00    
##                                                                       
##  checkin_info.11.6 checkin_info.12.2 checkin_info.12.4 checkin_info.16.5
##  Min.   : 0.000    Min.   : 0.000    Min.   : 0.000    Min.   : 0.000   
##  1st Qu.: 0.000    1st Qu.: 0.000    1st Qu.: 0.000    1st Qu.: 0.000   
##  Median : 0.000    Median : 0.000    Median : 0.000    Median : 0.000   
##  Mean   : 0.519    Mean   : 0.465    Mean   : 0.624    Mean   : 0.493   
##  3rd Qu.: 0.000    3rd Qu.: 0.000    3rd Qu.: 1.000    3rd Qu.: 0.000   
##  Max.   :12.000    Max.   :19.000    Max.   :23.000    Max.   :11.000   
##                                                                         
##  checkin_info.17.6 checkin_info.2.6 checkin_info.2.0 checkin_info.22.6
##  Min.   : 0.000    Min.   :0.000    Min.   :0.0000   Min.   :0.000    
##  1st Qu.: 0.000    1st Qu.:0.000    1st Qu.:0.0000   1st Qu.:0.000    
##  Median : 0.000    Median :0.000    Median :0.0000   Median :0.000    
##  Mean   : 0.546    Mean   :0.015    Mean   :0.0041   Mean   :0.068    
##  3rd Qu.: 0.000    3rd Qu.:0.000    3rd Qu.:0.0000   3rd Qu.:0.000    
##  Max.   :18.000    Max.   :2.000    Max.   :1.0000   Max.   :5.000    
##                                                                       
##  checkin_info.10.5 checkin_info.20.6 checkin_info.7.1 checkin_info.11.0
##  Min.   : 0.000    Min.   :0.000     Min.   :0.000    Min.   : 0.000   
##  1st Qu.: 0.000    1st Qu.:0.000     1st Qu.:0.000    1st Qu.: 0.000   
##  Median : 0.000    Median :0.000     Median :0.000    Median : 0.000   
##  Mean   : 0.599    Mean   :0.213     Mean   :0.144    Mean   : 0.437   
##  3rd Qu.: 1.000    3rd Qu.:0.000     3rd Qu.:0.000    3rd Qu.: 0.000   
##  Max.   :15.000    Max.   :9.000     Max.   :5.000    Max.   :18.000   
##                                                                        
##  checkin_info.16.0 checkin_info.19.4 checkin_info.8.2 checkin_info.9.5
##  Min.   :0.000     Min.   : 0.000    Min.   :0.000    Min.   : 0.000  
##  1st Qu.:0.000     1st Qu.: 0.000    1st Qu.:0.000    1st Qu.: 0.000  
##  Median :0.000     Median : 0.000    Median :0.000    Median : 0.000  
##  Mean   :0.332     Mean   : 0.688    Mean   :0.181    Mean   : 0.422  
##  3rd Qu.:0.000     3rd Qu.: 1.000    3rd Qu.:0.000    3rd Qu.: 0.000  
##  Max.   :6.000     Max.   :14.000    Max.   :8.000    Max.   :19.000  
##                                                                       
##  checkin_info.11.3 checkin_info.23.0 checkin_info.1.1 checkin_info.10.3
##  Min.   : 0.00     Min.   :0.00      Min.   :0.0000   Min.   : 0.000   
##  1st Qu.: 0.00     1st Qu.:0.00      1st Qu.:0.0000   1st Qu.: 0.000   
##  Median : 0.00     Median :0.00      Median :0.0000   Median : 0.000   
##  Mean   : 0.53     Mean   :0.03      Mean   :0.0136   Mean   : 0.281   
##  3rd Qu.: 0.00     3rd Qu.:0.00      3rd Qu.:0.0000   3rd Qu.: 0.000   
##  Max.   :33.00     Max.   :2.00      Max.   :2.0000   Max.   :14.000   
##                                                                        
##  checkin_info.12.0 checkin_info.13.4 checkin_info.14.4 checkin_info.18.4
##  Min.   : 0.000    Min.   :0.000     Min.   : 0.000    Min.   : 0.00    
##  1st Qu.: 0.000    1st Qu.:0.000     1st Qu.: 0.000    1st Qu.: 0.00    
##  Median : 0.000    Median :0.000     Median : 0.000    Median : 0.00    
##  Mean   : 0.462    Mean   :0.386     Mean   : 0.342    Mean   : 0.87    
##  3rd Qu.: 1.000    3rd Qu.:0.000     3rd Qu.: 0.000    3rd Qu.: 1.00    
##  Max.   :18.000    Max.   :8.000     Max.   :10.000    Max.   :35.00    
##                                                                         
##  checkin_info.18.5 checkin_info.20.0 checkin_info.21.5 checkin_info.15.6
##  Min.   : 0.000    Min.   :0.000     Min.   : 0.000    Min.   : 0.000   
##  1st Qu.: 0.000    1st Qu.:0.000     1st Qu.: 0.000    1st Qu.: 0.000   
##  Median : 0.000    Median :0.000     Median : 0.000    Median : 0.000   
##  Mean   : 0.725    Mean   :0.228     Mean   : 0.272    Mean   : 0.394   
##  3rd Qu.: 1.000    3rd Qu.:0.000     3rd Qu.: 0.000    3rd Qu.: 0.000   
##  Max.   :15.000    Max.   :7.000     Max.   :13.000    Max.   :13.000   
##                                                                         
##  checkin_info.8.4 checkin_info.13.0 checkin_info.17.2 checkin_info.17.4
##  Min.   : 0.000   Min.   :0.000     Min.   : 0.000    Min.   : 0.000   
##  1st Qu.: 0.000   1st Qu.:0.000     1st Qu.: 0.000    1st Qu.: 0.000   
##  Median : 0.000   Median :0.000     Median : 0.000    Median : 0.000   
##  Mean   : 0.229   Mean   :0.322     Mean   : 0.567    Mean   : 0.676   
##  3rd Qu.: 0.000   3rd Qu.:0.000     3rd Qu.: 1.000    3rd Qu.: 1.000   
##  Max.   :12.000   Max.   :7.000     Max.   :16.000    Max.   :16.000   
##                                                                        
##  checkin_info.18.2 checkin_info.7.6 checkin_info.10.6 checkin_info.11.2
##  Min.   : 0.000    Min.   : 0.000   Min.   : 0.000    Min.   : 0.00    
##  1st Qu.: 0.000    1st Qu.: 0.000   1st Qu.: 0.000    1st Qu.: 0.00    
##  Median : 0.000    Median : 0.000   Median : 0.000    Median : 0.00    
##  Mean   : 0.553    Mean   : 0.158   Mean   : 0.474    Mean   : 0.52    
##  3rd Qu.: 1.000    3rd Qu.: 0.000   3rd Qu.: 0.000    3rd Qu.: 0.00    
##  Max.   :18.000    Max.   :18.000   Max.   :20.000    Max.   :49.00    
##                                                                        
##  checkin_info.12.3 checkin_info.14.2 checkin_info.14.3 checkin_info.14.5
##  Min.   : 0.000    Min.   :0.000     Min.   :0.000     Min.   : 0.000   
##  1st Qu.: 0.000    1st Qu.:0.000     1st Qu.:0.000     1st Qu.: 0.000   
##  Median : 0.000    Median :0.000     Median :0.000     Median : 0.000   
##  Mean   : 0.436    Mean   :0.238     Mean   :0.247     Mean   : 0.557   
##  3rd Qu.: 0.000    3rd Qu.:0.000     3rd Qu.:0.000     3rd Qu.: 1.000   
##  Max.   :10.000    Max.   :5.000     Max.   :6.000     Max.   :12.000   
##                                                                         
##  checkin_info.14.6 checkin_info.16.1 checkin_info.17.5 checkin_info.18.0
##  Min.   : 0.000    Min.   : 0.000    Min.   : 0.000    Min.   : 0.000   
##  1st Qu.: 0.000    1st Qu.: 0.000    1st Qu.: 0.000    1st Qu.: 0.000   
##  Median : 0.000    Median : 0.000    Median : 0.000    Median : 0.000   
##  Mean   : 0.482    Mean   : 0.424    Mean   : 0.662    Mean   : 0.471   
##  3rd Qu.: 1.000    3rd Qu.: 0.000    3rd Qu.: 1.000    3rd Qu.: 0.000   
##  Max.   :11.000    Max.   :15.000    Max.   :17.000    Max.   :14.000   
##                                                                         
##  checkin_info.18.1 checkin_info.18.3 checkin_info.19.0 checkin_info.21.1
##  Min.   : 0.000    Min.   : 0.000    Min.   :0.000     Min.   : 0.000   
##  1st Qu.: 0.000    1st Qu.: 0.000    1st Qu.:0.000     1st Qu.: 0.000   
##  Median : 0.000    Median : 0.000    Median :0.000     Median : 0.000   
##  Mean   : 0.542    Mean   : 0.644    Mean   :0.341     Mean   : 0.139   
##  3rd Qu.: 1.000    3rd Qu.: 1.000    3rd Qu.:0.000     3rd Qu.: 0.000   
##  Max.   :13.000    Max.   :22.000    Max.   :7.000     Max.   :12.000   
##                                                                         
##  checkin_info.10.1 checkin_info.10.2 checkin_info.13.3 checkin_info.15.0
##  Min.   : 0.000    Min.   :0.000     Min.   : 0.000    Min.   :0.000    
##  1st Qu.: 0.000    1st Qu.:0.000     1st Qu.: 0.000    1st Qu.:0.000    
##  Median : 0.000    Median :0.000     Median : 0.000    Median :0.000    
##  Mean   : 0.313    Mean   :0.286     Mean   : 0.332    Mean   :0.255    
##  3rd Qu.: 0.000    3rd Qu.:0.000     3rd Qu.: 0.000    3rd Qu.:0.000    
##  Max.   :16.000    Max.   :8.000     Max.   :13.000    Max.   :6.000    
##                                                                         
##  checkin_info.15.4 checkin_info.16.2 checkin_info.20.1 checkin_info.21.2
##  Min.   : 0.000    Min.   : 0.000    Min.   : 0.000    Min.   : 0.000   
##  1st Qu.: 0.000    1st Qu.: 0.000    1st Qu.: 0.000    1st Qu.: 0.000   
##  Median : 0.000    Median : 0.000    Median : 0.000    Median : 0.000   
##  Mean   : 0.387    Mean   : 0.451    Mean   : 0.287    Mean   : 0.159   
##  3rd Qu.: 0.000    3rd Qu.: 0.750    3rd Qu.: 0.000    3rd Qu.: 0.000   
##  Max.   :11.000    Max.   :27.000    Max.   :11.000    Max.   :11.000   
##                                                                         
##  checkin_info.22.0 checkin_info.9.2 checkin_info.8.0 checkin_info.9.0
##  Min.   :0.0000    Min.   :0.000    Min.   : 0.000   Min.   :0.000   
##  1st Qu.:0.0000    1st Qu.:0.000    1st Qu.: 0.000   1st Qu.:0.000   
##  Median :0.0000    Median :0.000    Median : 0.000   Median :0.000   
##  Mean   :0.0572    Mean   :0.195    Mean   : 0.155   Mean   :0.199   
##  3rd Qu.:0.0000    3rd Qu.:0.000    3rd Qu.: 0.000   3rd Qu.:0.000   
##  Max.   :3.0000    Max.   :7.000    Max.   :10.000   Max.   :7.000   
##                                                                      
##  checkin_info.9.3 checkin_info.9.4 checkin_info.23.1 checkin_info.5.5
##  Min.   :0.000    Min.   :0.000    Min.   :0.0000    Min.   :0.00    
##  1st Qu.:0.000    1st Qu.:0.000    1st Qu.:0.0000    1st Qu.:0.00    
##  Median :0.000    Median :0.000    Median :0.0000    Median :0.00    
##  Mean   :0.154    Mean   :0.222    Mean   :0.0354    Mean   :0.03    
##  3rd Qu.:0.000    3rd Qu.:0.000    3rd Qu.:0.0000    3rd Qu.:0.00    
##  Max.   :5.000    Max.   :8.000    Max.   :3.0000    Max.   :2.00    
##                                                                      
##  checkin_info.6.5 checkin_info.7.3 checkin_info.7.5 checkin_info.8.3
##  Min.   : 0.000   Min.   :0.000    Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 0.000   1st Qu.:0.000    1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.000   Median :0.000    Median : 0.000   Median : 0.000  
##  Mean   : 0.087   Mean   :0.161    Mean   : 0.214   Mean   : 0.169  
##  3rd Qu.: 0.000   3rd Qu.:0.000    3rd Qu.: 0.000   3rd Qu.: 0.000  
##  Max.   :11.000   Max.   :9.000    Max.   :24.000   Max.   :12.000  
##                                                                     
##  checkin_info.9.1 checkin_info.3.0 checkin_info.4.1 checkin_info.4.2
##  Min.   :0.000    Min.   :0.0000   Min.   : 0.000   Min.   :0.000   
##  1st Qu.:0.000    1st Qu.:0.0000   1st Qu.: 0.000   1st Qu.:0.000   
##  Median :0.000    Median :0.0000   Median : 0.000   Median :0.000   
##  Mean   :0.138    Mean   :0.0014   Mean   : 0.049   Mean   :0.023   
##  3rd Qu.:0.000    3rd Qu.:0.0000   3rd Qu.: 0.000   3rd Qu.:0.000   
##  Max.   :5.000    Max.   :1.0000   Max.   :17.000   Max.   :6.000   
##                                                                     
##  checkin_info.5.0 checkin_info.5.1 checkin_info.5.2 checkin_info.5.3
##  Min.   :0.0000   Min.   :0.00     Min.   :0.000    Min.   :0.000   
##  1st Qu.:0.0000   1st Qu.:0.00     1st Qu.:0.000    1st Qu.:0.000   
##  Median :0.0000   Median :0.00     Median :0.000    Median :0.000   
##  Mean   :0.0381   Mean   :0.06     Mean   :0.057    Mean   :0.072   
##  3rd Qu.:0.0000   3rd Qu.:0.00     3rd Qu.:0.000    3rd Qu.:0.000   
##  Max.   :3.0000   Max.   :6.00     Max.   :7.000    Max.   :8.000   
##                                                                     
##  checkin_info.5.4 checkin_info.6.0 checkin_info.6.1 checkin_info.6.2
##  Min.   : 0.000   Min.   :0.000    Min.   :0.000    Min.   : 0.000  
##  1st Qu.: 0.000   1st Qu.:0.000    1st Qu.:0.000    1st Qu.: 0.000  
##  Median : 0.000   Median :0.000    Median :0.000    Median : 0.000  
##  Mean   : 0.074   Mean   :0.072    Mean   :0.097    Mean   : 0.106  
##  3rd Qu.: 0.000   3rd Qu.:0.000    3rd Qu.:0.000    3rd Qu.: 0.000  
##  Max.   :16.000   Max.   :6.000    Max.   :6.000    Max.   :10.000  
##                                                                     
##  checkin_info.6.3 checkin_info.6.4 checkin_info.7.2 checkin_info.7.4
##  Min.   : 0.000   Min.   : 0.000   Min.   :0.000    Min.   : 0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.000    1st Qu.: 0.000  
##  Median : 0.000   Median : 0.000   Median :0.000    Median : 0.000  
##  Mean   : 0.109   Mean   : 0.161   Mean   :0.176    Mean   : 0.214  
##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:0.000    3rd Qu.: 0.000  
##  Max.   :13.000   Max.   :24.000   Max.   :9.000    Max.   :16.000  
##                                                                     
##  checkin_info.8.1 checkin_info.8.6 checkin_info.9.6 checkin_info.6.6
##  Min.   :0.00     Min.   : 0.00    Min.   : 0.000   Min.   :0.0000  
##  1st Qu.:0.00     1st Qu.: 0.00    1st Qu.: 0.000   1st Qu.:0.0000  
##  Median :0.00     Median : 0.00    Median : 0.000   Median :0.0000  
##  Mean   :0.18     Mean   : 0.26    Mean   : 0.313   Mean   :0.0436  
##  3rd Qu.:0.00     3rd Qu.: 0.00    3rd Qu.: 0.000   3rd Qu.:0.0000  
##  Max.   :7.00     Max.   :38.00    Max.   :20.000   Max.   :3.0000  
##                                                                     
##  checkin_info.3.2 checkin_info.23.3 checkin_info.3.4 checkin_info.4.3
##  Min.   :0.0000   Min.   :0.000     Min.   :0.0000   Min.   : 0.000  
##  1st Qu.:0.0000   1st Qu.:0.000     1st Qu.:0.0000   1st Qu.: 0.000  
##  Median :0.0000   Median :0.000     Median :0.0000   Median : 0.000  
##  Mean   :0.0095   Mean   :0.053     Mean   :0.0068   Mean   : 0.035  
##  3rd Qu.:0.0000   3rd Qu.:0.000     3rd Qu.:0.0000   3rd Qu.: 0.000  
##  Max.   :2.0000   Max.   :8.000     Max.   :1.0000   Max.   :12.000  
##                                                                      
##  checkin_info.5.6 checkin_info.4.0 checkin_info.1.2 checkin_info.2.4
##  Min.   :0.0000   Min.   : 0.000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.: 0.000   1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :0.0000   Median : 0.000   Median :0.0000   Median :0.0000  
##  Mean   :0.0191   Mean   : 0.029   Mean   :0.0095   Mean   :0.0136  
##  3rd Qu.:0.0000   3rd Qu.: 0.000   3rd Qu.:0.0000   3rd Qu.:0.0000  
##  Max.   :3.0000   Max.   :14.000   Max.   :1.0000   Max.   :3.0000  
##                                                                     
##  checkin_info.3.6 checkin_info.3.3 checkin_info.4.5 checkin_info.4.4
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.000   
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.000   
##  Median :0.0000   Median :0.0000   Median :0.0000   Median :0.000   
##  Mean   :0.0041   Mean   :0.0041   Mean   :0.0082   Mean   :0.022   
##  3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.000   
##  Max.   :1.0000   Max.   :1.0000   Max.   :2.0000   Max.   :7.000   
##                                                                     
##  checkin_info.3.5 checkin_info.2.1 checkin_info.3.1 checkin_info.total
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :  3.0     
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:  8.0     
##  Median :0.0000   Median :0.0000   Median :0.0000   Median : 18.0     
##  Mean   :0.0027   Mean   :0.0027   Mean   :0.0027   Mean   : 43.4     
##  3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.: 44.0     
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :684.0     
##                                                                       
##       type                     business_id 
##  checkin:734   _7OvxfAje3XDZuSzguF6iA:  1  
##                _CblT1wpw8bbZISdOy9lkQ:  1  
##                _CVcmMqTUequ2u6fbJ48vQ:  1  
##                _HAYDjkPJVabBXyvx-u9YQ:  1  
##                _jgTXxLb-EdoDGx7NIt-LA:  1  
##                _K5cfCM7-zQZ1hXdbsc2fQ:  1  
##                (Other)               :728  
## 
## $review
##                    user_id                       review_id    
##  wqQ-50Sjf9ol3SYaPbA1YQ:   93   __pahYxcWwu05dPXxC_m5w:    1  
##  ky75pkIL378HJbe-x0GwXA:   81   __Ph4Kj6a13aFaxq7zPzug:    1  
##  kGgAARL2UmvCcTRfiscjug:   72   __uFLnzYUfE-mu82UU7evA:    1  
##  1BW2HC851fJKPfJeQxjkTA:   58   _-c6weN5djKAnA_Hug7FPQ:    1  
##  uZbTb-u-GVjTa2gtQfry5g:   57   _-K5y40uUfgutUziZq0SJg:    1  
##  ikm0UCahtK34LbLCEw4YTw:   53   _0-4_tUb2cr6W17HJ2f_ew:    1  
##  (Other)               :22542   (Other)               :22950  
##                  business_id        stars              date      
##  fWnGkLnZzv2yl3fB7xF40g:  152   Min.   :1.00   2013-02-20:  354  
##  aPdDcFXPsWVxBhaXuZcDpw:  145   1st Qu.:3.00   2013-02-26:  332  
##  AC1xogsITgaae5ic8B9Ryw:  141   Median :4.00   2013-02-18:  325  
##  jBb5KHHAj4s8hcXVS6QXDg:  138   Mean   :3.77   2013-02-24:  310  
##  MPyxaNVuWlAQqJ0iKV5rQw:  122   3rd Qu.:5.00   2013-02-25:  302  
##  lHr-aMBHh29zLpuB2ShAMQ:  115   Max.   :5.00   2013-01-27:  281  
##  (Other)               :22143                  (Other)   :21052  
##      type                            .              the       
##  review:22956   Min.   :   0   Min.   :  0.0   Min.   : 0.00  
##                 1st Qu.:  44   1st Qu.:  3.0   1st Qu.: 1.00  
##                 Median :  87   Median :  6.0   Median : 3.00  
##                 Mean   : 120   Mean   :  8.5   Mean   : 4.91  
##                 3rd Qu.: 157   3rd Qu.: 11.0   3rd Qu.: 7.00  
##                 Max.   :1058   Max.   :329.0   Max.   :59.00  
##                                                               
##        ,              and           I               a        
##  Min.   : 0.00   Min.   : 0   Min.   : 0.00   Min.   : 0.00  
##  1st Qu.: 1.00   1st Qu.: 1   1st Qu.: 0.00   1st Qu.: 1.00  
##  Median : 3.00   Median : 3   Median : 2.00   Median : 2.00  
##  Mean   : 4.53   Mean   : 4   Mean   : 3.26   Mean   : 3.06  
##  3rd Qu.: 6.00   3rd Qu.: 6   3rd Qu.: 4.00   3rd Qu.: 4.00  
##  Max.   :72.00   Max.   :50   Max.   :66.00   Max.   :33.00  
##                                                              
##        \n              to              of             was       
##  Min.   : 0.00   Min.   : 0.00   Min.   : 0.00   Min.   : 0.00  
##  1st Qu.: 0.00   1st Qu.: 0.00   1st Qu.: 0.00   1st Qu.: 0.00  
##  Median : 0.00   Median : 2.00   Median : 1.00   Median : 1.00  
##  Mean   : 2.63   Mean   : 2.86   Mean   : 1.77   Mean   : 1.91  
##  3rd Qu.: 4.00   3rd Qu.: 4.00   3rd Qu.: 2.00   3rd Qu.: 3.00  
##  Max.   :62.00   Max.   :39.00   Max.   :28.00   Max.   :38.00  
##                                                                 
##        is             for              it              in       
##  Min.   : 0.00   Min.   : 0.00   Min.   : 0.00   Min.   : 0.00  
##  1st Qu.: 0.00   1st Qu.: 0.00   1st Qu.: 0.00   1st Qu.: 0.00  
##  Median : 1.00   Median : 1.00   Median : 1.00   Median : 1.00  
##  Mean   : 1.51   Mean   : 1.38   Mean   : 1.28   Mean   : 1.29  
##  3rd Qu.: 2.00   3rd Qu.: 2.00   3rd Qu.: 2.00   3rd Qu.: 2.00  
##  Max.   :18.00   Max.   :18.00   Max.   :32.00   Max.   :22.00  
##                                                                 
##        !               The             that            with       
##  Min.   :  0.00   Min.   : 0.00   Min.   : 0.00   Min.   : 0.000  
##  1st Qu.:  0.00   1st Qu.: 0.00   1st Qu.: 0.00   1st Qu.: 0.000  
##  Median :  0.00   Median : 1.00   Median : 0.00   Median : 0.000  
##  Mean   :  1.37   Mean   : 1.03   Mean   : 1.06   Mean   : 0.948  
##  3rd Qu.:  2.00   3rd Qu.: 2.00   3rd Qu.: 1.00   3rd Qu.: 1.000  
##  Max.   :111.00   Max.   :17.00   Max.   :27.00   Max.   :21.000  
##                                                                   
##       but             you               my               on        
##  Min.   : 0.00   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 0.00   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.00   Median : 0.000   Median : 0.000   Median : 0.000  
##  Mean   : 0.82   Mean   : 0.819   Mean   : 0.904   Mean   : 0.808  
##  3rd Qu.: 1.00   3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.: 1.000  
##  Max.   :14.00   Max.   :23.000   Max.   :26.000   Max.   :14.000  
##                                                                    
##       have             this             had              are       
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   : 0.00  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.00  
##  Median : 0.000   Median : 0.000   Median : 0.000   Median : 0.00  
##  Mean   : 0.821   Mean   : 0.741   Mean   : 0.668   Mean   : 0.63  
##  3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.: 1.00  
##  Max.   :13.000   Max.   :13.000   Max.   :24.000   Max.   :14.00  
##                                                                    
##       they             not               )              place      
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median : 0.000   Median : 0.000   Median : 0.000   Median :0.000  
##  Mean   : 0.695   Mean   : 0.612   Mean   : 0.545   Mean   :0.557  
##  3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.:1.000  
##  Max.   :21.000   Max.   :14.000   Max.   :19.000   Max.   :9.000  
##                                                                    
##        at              good            were              (         
##  Min.   : 0.000   Min.   :0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 0.000   1st Qu.:0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.000   Median :0.000   Median : 0.000   Median : 0.000  
##  Mean   : 0.569   Mean   :0.525   Mean   : 0.559   Mean   : 0.486  
##  3rd Qu.: 1.000   3rd Qu.:1.000   3rd Qu.: 1.000   3rd Qu.: 1.000  
##  Max.   :13.000   Max.   :8.000   Max.   :18.000   Max.   :17.000  
##                                                                    
##       food              "               we               so        
##  Min.   : 0.000   Min.   : 0.00   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 0.000   1st Qu.: 0.00   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.000   Median : 0.00   Median : 0.000   Median : 0.000  
##  Mean   : 0.537   Mean   : 0.46   Mean   : 0.551   Mean   : 0.512  
##  3rd Qu.: 1.000   3rd Qu.: 0.00   3rd Qu.: 0.000   3rd Qu.: 1.000  
##  Max.   :10.000   Max.   :38.00   Max.   :21.000   Max.   :11.000  
##                                                                    
##        -               be               as              like       
##  Min.   : 0.00   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 0.00   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.00   Median : 0.000   Median : 0.000   Median : 0.000  
##  Mean   : 0.45   Mean   : 0.513   Mean   : 0.472   Mean   : 0.401  
##  3rd Qu.: 0.00   3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.: 1.000  
##  Max.   :70.00   Max.   :10.000   Max.   :11.000   Max.   :11.000  
##                                                                    
##        me              out             there             here      
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median : 0.000   Median : 0.000   Median : 0.000   Median :0.000  
##  Mean   : 0.465   Mean   : 0.423   Mean   : 0.389   Mean   :0.386  
##  3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.:1.000  
##  Max.   :22.000   Max.   :14.000   Max.   :10.000   Max.   :9.000  
##                                                                    
##       just           great            all             very      
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.363   Mean   :0.398   Mean   :0.368   Mean   :0.403  
##  3rd Qu.:1.000   3rd Qu.:1.000   3rd Qu.:1.000   3rd Qu.:1.000  
##  Max.   :9.000   Max.   :9.000   Max.   :9.000   Max.   :9.000  
##                                                                 
##        or             get             one              from      
##  Min.   :0.000   Min.   :0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median : 0.000   Median :0.000  
##  Mean   :0.333   Mean   :0.355   Mean   : 0.313   Mean   :0.323  
##  3rd Qu.:0.000   3rd Qu.:1.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :9.000   Max.   :9.000   Max.   :10.000   Max.   :9.000  
##                                                                  
##       time            their              up             about      
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median : 0.000   Median : 0.000   Median : 0.000   Median :0.000  
##  Mean   : 0.339   Mean   : 0.318   Mean   : 0.313   Mean   :0.295  
##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :13.000   Max.   :15.000   Max.   :11.000   Max.   :9.000  
##                                                                    
##        if              go              We             really     
##  Min.   :0.000   Min.   :0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median : 0.000   Median :0.000  
##  Mean   :0.286   Mean   :0.292   Mean   : 0.324   Mean   :0.279  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :8.000   Max.   :7.000   Max.   :10.000   Max.   :9.000  
##                                                                  
##        ?               some            would             back      
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median : 0.000   Median : 0.000   Median : 0.000   Median :0.000  
##  Mean   : 0.244   Mean   : 0.264   Mean   : 0.297   Mean   :0.297  
##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :22.000   Max.   :10.000   Max.   :10.000   Max.   :9.000  
##                                                                    
##        an             been           which            when      
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.281   Mean   :0.273   Mean   :0.245   Mean   :0.258  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :7.000   Max.   :8.000   Max.   :9.000   Max.   :7.000  
##                                                                 
##       our            service             It              can       
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median : 0.000   Median : 0.000   Median : 0.000   Median :0.000  
##  Mean   : 0.293   Mean   : 0.291   Mean   : 0.244   Mean   :0.238  
##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :17.000   Max.   :11.000   Max.   :10.000   Max.   :8.000  
##                                                                    
##       your             more             $               They       
##  Min.   : 0.000   Min.   :0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 0.000   1st Qu.:0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.000   Median :0.000   Median : 0.000   Median : 0.000  
##  Mean   : 0.235   Mean   :0.233   Mean   : 0.246   Mean   : 0.262  
##  3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.: 0.000  
##  Max.   :10.000   Max.   :7.000   Max.   :15.000   Max.   :12.000  
##                                                                    
##       will            what            This            only      
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.259   Mean   :0.229   Mean   :0.223   Mean   :0.216  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :8.000   Max.   :9.000   Max.   :6.000   Max.   :7.000  
##                                                                 
##       it's             by              :               I'm       
##  Min.   :0.000   Min.   :0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median : 0.000   Median :0.000  
##  Mean   :0.179   Mean   :0.211   Mean   : 0.187   Mean   :0.179  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :7.000   Max.   :8.000   Max.   :14.000   Max.   :8.000  
##                                                                  
##       too            little           has            don't      
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.191   Mean   :0.174   Mean   :0.191   Mean   :0.178  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :6.000   Max.   :7.000   Max.   :6.000   Max.   :9.000  
##                                                                 
##        My            other             them             also     
##  Min.   :0.000   Min.   : 0.000   Min.   : 0.000   Min.   :0.00  
##  1st Qu.:0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.00  
##  Median :0.000   Median : 0.000   Median : 0.000   Median :0.00  
##  Mean   :0.211   Mean   : 0.197   Mean   : 0.223   Mean   :0.19  
##  3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:0.00  
##  Max.   :7.000   Max.   :11.000   Max.   :13.000   Max.   :6.00  
##                                                                  
##     because           nice            I've            than      
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.191   Mean   :0.194   Mean   :0.169   Mean   :0.166  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :7.000   Max.   :6.000   Max.   :8.000   Max.   :6.000  
##                                                                 
##        no            always           even             do        
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   : 0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median : 0.000  
##  Mean   :0.181   Mean   :0.185   Mean   :0.175   Mean   : 0.187  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.: 0.000  
##  Max.   :9.000   Max.   :9.000   Max.   :8.000   Max.   :15.000  
##                                                                  
##       got             love              us              well      
##  Min.   :0.000   Min.   : 0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median :0.000   Median : 0.000   Median : 0.000   Median :0.000  
##  Mean   :0.172   Mean   : 0.157   Mean   : 0.189   Mean   :0.163  
##  3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :8.000   Max.   :13.000   Max.   :19.000   Max.   :6.000  
##                                                                   
##       much            menu             best           people     
##  Min.   :0.000   Min.   : 0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median : 0.000   Median :0.000   Median :0.000  
##  Mean   :0.148   Mean   : 0.138   Mean   :0.153   Mean   :0.142  
##  3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :6.000   Max.   :10.000   Max.   :7.000   Max.   :8.000  
##                                                                  
##      pretty        ordered           try          restaurant   
##  Min.   :0.00   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.00   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.00   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.13   Mean   :0.148   Mean   :0.149   Mean   :0.151  
##  3rd Qu.:0.00   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :6.00   Max.   :7.000   Max.   :6.000   Max.   :8.000  
##                                                                
##      order             know           could           didn't    
##  Min.   : 0.000   Min.   :0.000   Min.   :0.000   Min.   :0.00  
##  1st Qu.: 0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.00  
##  Median : 0.000   Median :0.000   Median :0.000   Median :0.00  
##  Mean   : 0.159   Mean   :0.141   Mean   :0.146   Mean   :0.14  
##  3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.00  
##  Max.   :25.000   Max.   :8.000   Max.   :8.000   Max.   :8.00  
##                                                                 
##       over            think            bar              went      
##  Min.   : 0.000   Min.   :0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.: 0.000   1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median : 0.000   Median :0.000   Median : 0.000   Median :0.000  
##  Mean   : 0.142   Mean   :0.118   Mean   : 0.114   Mean   :0.136  
##  3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :10.000   Max.   :4.000   Max.   :12.000   Max.   :7.000  
##                                                                   
##      going            make          friendly        chicken     
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.139   Mean   :0.133   Mean   :0.147   Mean   :0.126  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :9.000   Max.   :5.000   Max.   :5.000   Max.   :9.000  
##                                                                 
##      better            he              did              If       
##  Min.   :0.000   Min.   : 0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median : 0.000   Median :0.000   Median :0.000  
##  Mean   :0.121   Mean   : 0.177   Mean   :0.146   Mean   :0.123  
##  3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :5.000   Max.   :22.000   Max.   :8.000   Max.   :5.000  
##                                                                  
##      never             am             way             It's      
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.133   Mean   :0.128   Mean   :0.119   Mean   :0.105  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :5.000   Max.   :6.000   Max.   :5.000   Max.   :6.000  
##                                                                 
##      after           first           staff            off       
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.132   Mean   :0.135   Mean   :0.148   Mean   :0.124  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :9.000   Max.   :7.000   Max.   :8.000   Max.   :5.000  
##                                                                 
##      lunch           night             i               she        
##  Min.   :0.000   Min.   :0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median :0.000   Median :0.000   Median : 0.000   Median : 0.000  
##  Mean   :0.107   Mean   :0.105   Mean   : 0.088   Mean   : 0.149  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.: 0.000  
##  Max.   :9.000   Max.   :6.000   Max.   :14.000   Max.   :15.000  
##                                                                   
##       few            right            who             say      
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.00  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.00  
##  Median :0.000   Median :0.000   Median :0.000   Median :0.00  
##  Mean   :0.115   Mean   :0.118   Mean   :0.121   Mean   :0.11  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.00  
##  Max.   :5.000   Max.   :4.000   Max.   :7.000   Max.   :5.00  
##                                                                
##       want            came            two             made      
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.112   Mean   :0.128   Mean   :0.111   Mean   :0.117  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :7.000   Max.   :8.000   Max.   :6.000   Max.   :6.000  
##                                                                 
##       how          delicious         pizza            again      
##  Min.   :0.000   Min.   :0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median : 0.000   Median :0.000  
##  Mean   :0.114   Mean   :0.108   Mean   : 0.105   Mean   :0.128  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :7.000   Max.   :5.000   Max.   :12.000   Max.   :6.000  
##                                                                  
##       any             come           around          cheese     
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   : 0.00  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 0.00  
##  Median :0.000   Median :0.000   Median :0.000   Median : 0.00  
##  Mean   :0.112   Mean   :0.119   Mean   :0.107   Mean   : 0.09  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.: 0.00  
##  Max.   :5.000   Max.   :5.000   Max.   :7.000   Max.   :10.00  
##                                                                 
##       then            down            see            fresh      
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.107   Mean   :0.101   Mean   :0.103   Mean   :0.099  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :6.000   Max.   :5.000   Max.   :5.000   Max.   :6.000  
##                                                                 
##       eat              &             before           find      
##  Min.   :0.000   Min.   : 0.00   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.: 0.00   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median : 0.00   Median :0.000   Median :0.000  
##  Mean   :0.096   Mean   : 0.11   Mean   :0.105   Mean   :0.106  
##  3rd Qu.:0.000   3rd Qu.: 0.00   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :7.000   Max.   :21.00   Max.   :5.000   Max.   :7.000  
##                                                                 
##      sauce             day               So             And       
##  Min.   : 0.000   Min.   : 0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median : 0.000   Median : 0.000   Median :0.000   Median :0.000  
##  Mean   : 0.087   Mean   : 0.108   Mean   :0.092   Mean   :0.084  
##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :10.000   Max.   :12.000   Max.   :8.000   Max.   :7.000  
##                                                                   
##       wait            But             sure           salad       
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   : 0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median : 0.000  
##  Mean   :0.104   Mean   :0.087   Mean   :0.103   Mean   : 0.088  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.: 0.000  
##  Max.   :6.000   Max.   :7.000   Max.   :5.000   Max.   :14.000  
##                                                                  
##    experience       still         something          bit       
##  Min.   :0.00   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.00   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.00   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.11   Mean   :0.098   Mean   :0.093   Mean   :0.086  
##  3rd Qu.:0.00   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :5.00   Max.   :6.000   Max.   :4.000   Max.   :5.000  
##                                                                
##       area            said             take           times      
##  Min.   :0.000   Min.   : 0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median : 0.000   Median :0.000   Median :0.000  
##  Mean   :0.101   Mean   : 0.111   Mean   :0.104   Mean   :0.101  
##  3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :7.000   Max.   :12.000   Max.   :5.000   Max.   :4.000  
##                                                                  
##       her               2               '               ever      
##  Min.   : 0.000   Min.   :0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.: 0.000   1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median : 0.000   Median :0.000   Median : 0.000   Median :0.000  
##  Mean   : 0.124   Mean   :0.096   Mean   : 0.084   Mean   :0.098  
##  3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :15.000   Max.   :7.000   Max.   :20.000   Max.   :7.000  
##                                                                   
##    definitely        though           into            new       
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.094   Mean   :0.083   Mean   :0.083   Mean   :0.111  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :8.000   Max.   :4.000   Max.   :4.000   Max.   :7.000  
##                                                                 
##      thing           give            bad             meal      
##  Min.   :0.00   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.00   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.00   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.08   Mean   :0.092   Mean   :0.086   Mean   :0.081  
##  3rd Qu.:0.00   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :5.00   Max.   :4.000   Max.   :5.000   Max.   :9.000  
##                                                                
##       most          location         happy            since      
##  Min.   :0.000   Min.   :0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median : 0.000   Median :0.000  
##  Mean   :0.085   Mean   :0.096   Mean   : 0.087   Mean   :0.094  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :6.000   Max.   :9.000   Max.   :10.000   Max.   :6.000  
##                                                                  
##       many      
##  Min.   :0.000  
##  1st Qu.:0.000  
##  Median :0.000  
##  Mean   :0.088  
##  3rd Qu.:0.000  
##  Max.   :6.000  
##                 
## 
## $user
##   review_count          name      average_stars 
##  Min.   :   1.0   Chris   :  56   Min.   :1.00  
##  1st Qu.:   1.0   John    :  53   1st Qu.:3.00  
##  Median :   2.0   David   :  47   Median :4.00  
##  Mean   :  12.9   Michael :  47   Mean   :3.71  
##  3rd Qu.:   6.0   Jennifer:  44   3rd Qu.:5.00  
##  Max.   :1369.0   Mike    :  42   Max.   :5.00  
##                   (Other) :4816                 
##                    user_id       type     
##  __K0F5CMao-1u3y3DoE7wA:   1   user:5105  
##  __UPhB37Awu7Ue-saxK7Gg:   1              
##  __xuNw6-rbLvAbFrS_y-xg:   1              
##  __YRhEhah0mc0LExN6_ILA:   1              
##  _-lBbF_oVwiiDhL6L8QyRg:   1              
##  _0MVJpRw4vBDBtUWOITd_g:   1              
##  (Other)               :5099
```

```r

# lapply(trainingProcessed, head)
lapply(trainingProcessed, summary)
```

```
## $business
##                  business_id    categories.BeautySpas categories.Food
##  __apI0spr0I996M9DRpUwg:    1   Mode :logical         Mode :logical  
##  __szJVgpRqcISuzdLfUfEw:    1   FALSE:10773           FALSE:9921     
##  _-9pMxBWtG_x8l4rHWBasg:    1   TRUE :764             TRUE :1616     
##  _-TRcD1fi3Eq6U0tEb6Qnw:    1   NA's :0               NA's :0        
##  _0DvMQAs9KvA58Pf3Z4ltg:    1                                        
##  _0Eql3UB3K5-6q4IHNBbWA:    1                                        
##  (Other)               :11531                                        
##  categories.Restaurants categories.Shopping categories.total
##  Mode :logical          Mode :logical       Min.   : 0.00   
##  FALSE:7034             FALSE:9856          1st Qu.: 2.00   
##  TRUE :4503             TRUE :1681          Median : 2.00   
##  NA's :0                NA's :0             Mean   : 2.68   
##                                             3rd Qu.: 3.00   
##                                             Max.   :10.00   
##                                                             
##          city                                       full_address    
##  Phoenix   :4154   7014 E Camelback Rd\nScottsdale, AZ 85251:   38  
##  Scottsdale:2024   Phoenix, AZ                              :   24  
##  Tempe     :1153   2000 E Rio Salado Pkwy\nTempe, AZ 85281  :   19  
##  Mesa      : 898   21001 N Tatum Blvd\nPhoenix, AZ 85050    :   18  
##  Chandler  : 865   5000 S Arizona Mills Cir\nTempe, AZ 85282:   17  
##  Glendale  : 610   7000 E Mayo Blvd\nPhoenix, AZ 85054      :   14  
##  (Other)   :1833   (Other)                                  :11407  
##     latitude      longitude                  name       neighborhoods 
##  Min.   :32.9   Min.   :-113   Subway          :   68   Mode:logical  
##  1st Qu.:33.4   1st Qu.:-112   McDonald's      :   53   NA's:11537    
##  Median :33.5   Median :-112   Starbucks       :   46                 
##  Mean   :33.5   Mean   :-112   Taco Bell       :   39                 
##  3rd Qu.:33.6   3rd Qu.:-112   Discount Tire   :   33                 
##  Max.   :34.0   Max.   :-111   Starbucks Coffee:   33                 
##                                (Other)         :11265                 
##     open          review_count       stars      state     
##  Mode :logical   Min.   :  3.0   Min.   :1.00   AZ:11534  
##  FALSE:1224      1st Qu.:  4.0   1st Qu.:3.00   CA:    1  
##  TRUE :10313     Median :  6.0   Median :3.50   CO:    1  
##  NA's :0         Mean   : 20.2   Mean   :3.67   SC:    1  
##                  3rd Qu.: 16.0   3rd Qu.:4.50             
##                  Max.   :862.0   Max.   :5.00             
##                                                           
##        type      
##  business:11537  
##                  
##                  
##                  
##                  
##                  
##                  
## 
## $checkin
##  checkin_info.0.5 checkin_info.1.6 checkin_info.10.0 checkin_info.10.1
##  Min.   : 0.00    Min.   : 0.00    Min.   :  0.00    Min.   :  0.00   
##  1st Qu.: 0.00    1st Qu.: 0.00    1st Qu.:  0.00    1st Qu.:  0.00   
##  Median : 0.00    Median : 0.00    Median :  0.00    Median :  0.00   
##  Mean   : 0.22    Mean   : 0.12    Mean   :  0.52    Mean   :  0.51   
##  3rd Qu.: 0.00    3rd Qu.: 0.00    3rd Qu.:  0.00    3rd Qu.:  0.00   
##  Max.   :90.00    Max.   :44.00    Max.   :159.00    Max.   :113.00   
##                                                                       
##  checkin_info.10.2 checkin_info.10.3 checkin_info.10.4 checkin_info.10.5
##  Min.   :  0.00    Min.   :  0.00    Min.   :  0.00    Min.   :  0.00   
##  1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00   
##  Median :  0.00    Median :  0.00    Median :  0.00    Median :  0.00   
##  Mean   :  0.54    Mean   :  0.54    Mean   :  0.64    Mean   :  1.12   
##  3rd Qu.:  0.00    3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.00   
##  Max.   :146.00    Max.   :143.00    Max.   :159.00    Max.   :151.00   
##                                                                         
##  checkin_info.10.6 checkin_info.11.0 checkin_info.11.1 checkin_info.11.2
##  Min.   :  0.00    Min.   :  0.00    Min.   :  0.00    Min.   :  0.00   
##  1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00   
##  Median :  0.00    Median :  0.00    Median :  0.00    Median :  0.00   
##  Mean   :  1.05    Mean   :  0.98    Mean   :  1.03    Mean   :  1.04   
##  3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.00   
##  Max.   :151.00    Max.   :195.00    Max.   :167.00    Max.   :172.00   
##                                                                         
##  checkin_info.11.3 checkin_info.11.4 checkin_info.11.5 checkin_info.11.6
##  Min.   :  0.0     Min.   :  0.00    Min.   :  0.00    Min.   :  0.00   
##  1st Qu.:  0.0     1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00   
##  Median :  0.0     Median :  0.00    Median :  0.00    Median :  0.00   
##  Mean   :  1.1     Mean   :  1.25    Mean   :  1.43    Mean   :  1.27   
##  3rd Qu.:  1.0     3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.00   
##  Max.   :185.0     Max.   :199.00    Max.   :166.00    Max.   :218.00   
##                                                                         
##  checkin_info.12.0 checkin_info.12.1 checkin_info.12.2 checkin_info.12.3
##  Min.   :  0.0     Min.   :  0.00    Min.   :  0.0     Min.   :  0.00   
##  1st Qu.:  0.0     1st Qu.:  0.00    1st Qu.:  0.0     1st Qu.:  0.00   
##  Median :  0.0     Median :  0.00    Median :  0.0     Median :  0.00   
##  Mean   :  1.1     Mean   :  1.09    Mean   :  1.1     Mean   :  1.18   
##  3rd Qu.:  1.0     3rd Qu.:  1.00    3rd Qu.:  1.0     3rd Qu.:  1.00   
##  Max.   :147.0     Max.   :137.00    Max.   :139.0     Max.   :188.00   
##                                                                         
##  checkin_info.12.4 checkin_info.12.5 checkin_info.12.6 checkin_info.13.1
##  Min.   :  0.00    Min.   :  0.00    Min.   :  0.00    Min.   :  0.00   
##  1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00   
##  Median :  0.00    Median :  0.00    Median :  0.00    Median :  0.00   
##  Mean   :  1.42    Mean   :  1.54    Mean   :  1.42    Mean   :  0.63   
##  3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.00   
##  Max.   :254.00    Max.   :133.00    Max.   :235.00    Max.   :141.00   
##                                                                         
##  checkin_info.13.2 checkin_info.13.3 checkin_info.13.5 checkin_info.13.6
##  Min.   :  0.00    Min.   :  0.00    Min.   :  0.00    Min.   :  0.0    
##  1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.0    
##  Median :  0.00    Median :  0.00    Median :  0.00    Median :  0.0    
##  Mean   :  0.67    Mean   :  0.69    Mean   :  1.42    Mean   :  1.3    
##  3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.0    
##  Max.   :139.00    Max.   :193.00    Max.   :132.00    Max.   :231.0    
##                                                                         
##  checkin_info.14.0 checkin_info.14.1 checkin_info.14.3 checkin_info.14.4
##  Min.   :  0.00    Min.   :  0.00    Min.   :  0.00    Min.   :  0.00   
##  1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00   
##  Median :  0.00    Median :  0.00    Median :  0.00    Median :  0.00   
##  Mean   :  0.53    Mean   :  0.47    Mean   :  0.51    Mean   :  0.69   
##  3rd Qu.:  1.00    3rd Qu.:  0.00    3rd Qu.:  1.00    3rd Qu.:  1.00   
##  Max.   :184.00    Max.   :156.00    Max.   :202.00    Max.   :283.00   
##                                                                         
##  checkin_info.14.5 checkin_info.14.6 checkin_info.15.0 checkin_info.15.3
##  Min.   :  0.00    Min.   :  0.00    Min.   :  0.00    Min.   :  0.00   
##  1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00   
##  Median :  0.00    Median :  0.00    Median :  0.00    Median :  0.00   
##  Mean   :  1.19    Mean   :  1.06    Mean   :  0.52    Mean   :  0.55   
##  3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.00   
##  Max.   :115.00    Max.   :198.00    Max.   :187.00    Max.   :217.00   
##                                                                         
##  checkin_info.15.5 checkin_info.16.1 checkin_info.16.3 checkin_info.16.5
##  Min.   :  0.00    Min.   :  0.00    Min.   :  0.00    Min.   :  0.00   
##  1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00   
##  Median :  0.00    Median :  0.00    Median :  0.00    Median :  0.00   
##  Mean   :  1.09    Mean   :  0.77    Mean   :  0.82    Mean   :  1.12   
##  3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.00   
##  Max.   :126.00    Max.   :138.00    Max.   :212.00    Max.   :137.00   
##                                                                         
##  checkin_info.20.1 checkin_info.8.5 checkin_info.9.4 checkin_info.9.5
##  Min.   :  0.00    Min.   :  0.00   Min.   :  0.00   Min.   :  0.00  
##  1st Qu.:  0.00    1st Qu.:  0.00   1st Qu.:  0.00   1st Qu.:  0.00  
##  Median :  0.00    Median :  0.00   Median :  0.00   Median :  0.00  
##  Mean   :  0.61    Mean   :  0.58   Mean   :  0.43   Mean   :  0.88  
##  3rd Qu.:  0.00    3rd Qu.:  0.00   3rd Qu.:  0.00   3rd Qu.:  1.00  
##  Max.   :100.00    Max.   :224.00   Max.   :220.00   Max.   :167.00  
##                                                                      
##  checkin_info.9.6 checkin_info.0.0 checkin_info.17.1 checkin_info.17.4
##  Min.   :  0.00   Min.   : 0.000   Min.   :  0.00    Min.   :  0.0    
##  1st Qu.:  0.00   1st Qu.: 0.000   1st Qu.:  0.00    1st Qu.:  0.0    
##  Median :  0.00   Median : 0.000   Median :  0.00    Median :  0.0    
##  Mean   :  0.73   Mean   : 0.052   Mean   :  1.15    Mean   :  1.5    
##  3rd Qu.:  0.00   3rd Qu.: 0.000   3rd Qu.:  1.00    3rd Qu.:  1.0    
##  Max.   :176.00   Max.   :17.000   Max.   :187.00    Max.   :470.0    
##                                                                       
##  checkin_info.18.0 checkin_info.18.5 checkin_info.19.2 checkin_info.19.4
##  Min.   :  0.00    Min.   :  0.00    Min.   :  0.00    Min.   :  0.00   
##  1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00   
##  Median :  0.00    Median :  0.00    Median :  0.00    Median :  0.00   
##  Mean   :  1.12    Mean   :  1.44    Mean   :  1.08    Mean   :  1.64   
##  3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.00   
##  Max.   :217.00    Max.   :119.00    Max.   :182.00    Max.   :234.00   
##                                                                         
##  checkin_info.2.5 checkin_info.2.6 checkin_info.20.3 checkin_info.23.0
##  Min.   : 0.000   Min.   : 0.000   Min.   :  0.00    Min.   : 0.00    
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:  0.00    1st Qu.: 0.00    
##  Median : 0.000   Median : 0.000   Median :  0.00    Median : 0.00    
##  Mean   : 0.035   Mean   : 0.038   Mean   :  0.77    Mean   : 0.07    
##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:  1.00    3rd Qu.: 0.00    
##  Max.   :19.000   Max.   :14.000   Max.   :149.00    Max.   :37.00    
##                                                                       
##  checkin_info.3.5 checkin_info.3.6 checkin_info.0.2 checkin_info.13.4
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   :  0.00   
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:  0.00   
##  Median : 0.000   Median : 0.000   Median : 0.000   Median :  0.00   
##  Mean   : 0.015   Mean   : 0.015   Mean   : 0.058   Mean   :  0.85   
##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:  1.00   
##  Max.   :14.000   Max.   :11.000   Max.   :17.000   Max.   :233.00   
##                                                                      
##  checkin_info.14.2 checkin_info.19.0 checkin_info.19.1 checkin_info.19.3
##  Min.   :  0.0     Min.   :  0.00    Min.   :  0       Min.   :  0.00   
##  1st Qu.:  0.0     1st Qu.:  0.00    1st Qu.:  0       1st Qu.:  0.00   
##  Median :  0.0     Median :  0.00    Median :  0       Median :  0.00   
##  Mean   :  0.5     Mean   :  0.86    Mean   :  1       Mean   :  1.16   
##  3rd Qu.:  1.0     3rd Qu.:  1.00    3rd Qu.:  1       3rd Qu.:  1.00   
##  Max.   :183.0     Max.   :213.00    Max.   :128       Max.   :190.00   
##                                                                         
##  checkin_info.20.5 checkin_info.20.6 checkin_info.21.0 checkin_info.21.6
##  Min.   : 0.0      Min.   :  0.00    Min.   :  0.00    Min.   :  0.0    
##  1st Qu.: 0.0      1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.0    
##  Median : 0.0      Median :  0.00    Median :  0.00    Median :  0.0    
##  Mean   : 1.1      Mean   :  0.49    Mean   :  0.29    Mean   :  0.3    
##  3rd Qu.: 1.0      3rd Qu.:  0.00    3rd Qu.:  0.00    3rd Qu.:  0.0    
##  Max.   :58.0      Max.   :242.00    Max.   :115.00    Max.   :284.0    
##                                                                         
##  checkin_info.23.4 checkin_info.6.2 checkin_info.6.3 checkin_info.6.4
##  Min.   :  0.00    Min.   :  0.00   Min.   :  0.00   Min.   :  0.00  
##  1st Qu.:  0.00    1st Qu.:  0.00   1st Qu.:  0.00   1st Qu.:  0.00  
##  Median :  0.00    Median :  0.00   Median :  0.00   Median :  0.00  
##  Mean   :  0.37    Mean   :  0.21   Mean   :  0.21   Mean   :  0.24  
##  3rd Qu.:  0.00    3rd Qu.:  0.00   3rd Qu.:  0.00   3rd Qu.:  0.00  
##  Max.   :131.00    Max.   :135.00   Max.   :157.00   Max.   :178.00  
##                                                                      
##  checkin_info.7.4 checkin_info.7.5 checkin_info.8.2 checkin_info.9.2
##  Min.   :  0.0    Min.   :  0.00   Min.   :  0.00   Min.   :  0.00  
##  1st Qu.:  0.0    1st Qu.:  0.00   1st Qu.:  0.00   1st Qu.:  0.00  
##  Median :  0.0    Median :  0.00   Median :  0.00   Median :  0.00  
##  Mean   :  0.4    Mean   :  0.32   Mean   :  0.38   Mean   :  0.34  
##  3rd Qu.:  0.0    3rd Qu.:  0.00   3rd Qu.:  0.00   3rd Qu.:  0.00  
##  Max.   :235.0    Max.   :160.00   Max.   :247.00   Max.   :179.00  
##                                                                     
##  checkin_info.15.1 checkin_info.17.6 checkin_info.20.0 checkin_info.15.4
##  Min.   :  0.00    Min.   :  0.00    Min.   :  0.00    Min.   :  0.00   
##  1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00   
##  Median :  0.00    Median :  0.00    Median :  0.00    Median :  0.00   
##  Mean   :  0.51    Mean   :  1.04    Mean   :  0.54    Mean   :  0.73   
##  3rd Qu.:  0.00    3rd Qu.:  1.00    3rd Qu.:  0.00    3rd Qu.:  1.00   
##  Max.   :172.00    Max.   :303.00    Max.   :143.00    Max.   :269.00   
##                                                                         
##  checkin_info.16.2 checkin_info.18.4 checkin_info.17.5 checkin_info.18.1
##  Min.   :  0.00    Min.   :  0.00    Min.   :  0.00    Min.   :  0.00   
##  1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00   
##  Median :  0.00    Median :  0.00    Median :  0.00    Median :  0.00   
##  Mean   :  0.81    Mean   :  1.78    Mean   :  1.32    Mean   :  1.26   
##  3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.00   
##  Max.   :175.00    Max.   :305.00    Max.   :152.00    Max.   :189.00   
##                                                                         
##  checkin_info.5.4 checkin_info.6.6 checkin_info.7.6 checkin_info.8.4
##  Min.   :  0.00   Min.   : 0.00    Min.   :  0.0    Min.   :  0.00  
##  1st Qu.:  0.00   1st Qu.: 0.00    1st Qu.:  0.0    1st Qu.:  0.00  
##  Median :  0.00   Median : 0.00    Median :  0.0    Median :  0.00  
##  Mean   :  0.12   Mean   : 0.08    Mean   :  0.2    Mean   :  0.44  
##  3rd Qu.:  0.00   3rd Qu.: 0.00    3rd Qu.:  0.0    3rd Qu.:  0.00  
##  Max.   :138.00   Max.   :88.00    Max.   :124.0    Max.   :285.00  
##                                                                     
##  checkin_info.8.6 checkin_info.1.1 checkin_info.15.2 checkin_info.15.6
##  Min.   :  0.00   Min.   :0.000    Min.   :  0.00    Min.   :  0.00   
##  1st Qu.:  0.00   1st Qu.:0.000    1st Qu.:  0.00    1st Qu.:  0.00   
##  Median :  0.00   Median :0.000    Median :  0.00    Median :  0.00   
##  Mean   :  0.46   Mean   :0.024    Mean   :  0.54    Mean   :  0.94   
##  3rd Qu.:  0.00   3rd Qu.:0.000    3rd Qu.:  1.00    3rd Qu.:  1.00   
##  Max.   :244.00   Max.   :7.000    Max.   :191.00    Max.   :220.00   
##                                                                       
##  checkin_info.16.0 checkin_info.16.6 checkin_info.17.0 checkin_info.17.2
##  Min.   :  0.00    Min.   :  0.00    Min.   :  0.00    Min.   :  0.00   
##  1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.:  0.00   
##  Median :  0.00    Median :  0.00    Median :  0.00    Median :  0.00   
##  Mean   :  0.72    Mean   :  0.92    Mean   :  1.04    Mean   :  1.25   
##  3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.:  1.00   
##  Max.   :150.00    Max.   :181.00    Max.   :202.00    Max.   :201.00   
##                                                                         
##  checkin_info.17.3 checkin_info.18.2 checkin_info.18.3 checkin_info.18.6
##  Min.   :  0.0     Min.   :  0.00    Min.   :  0.0     Min.   :  0      
##  1st Qu.:  0.0     1st Qu.:  0.00    1st Qu.:  0.0     1st Qu.:  0      
##  Median :  0.0     Median :  0.00    Median :  0.0     Median :  0      
##  Mean   :  1.3     Mean   :  1.36    Mean   :  1.4     Mean   :  1      
##  3rd Qu.:  1.0     3rd Qu.:  1.00    3rd Qu.:  1.0     3rd Qu.:  1      
##  Max.   :261.0     Max.   :199.00    Max.   :236.0     Max.   :320      
##                                                                         
##  checkin_info.19.5 checkin_info.19.6 checkin_info.21.1 checkin_info.21.2
##  Min.   :  0.00    Min.   :  0.00    Min.   : 0.00     Min.   :  0.00   
##  1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.: 0.00     1st Qu.:  0.00   
##  Median :  0.00    Median :  0.00    Median : 0.00     Median :  0.00   
##  Mean   :  1.39    Mean   :  0.76    Mean   : 0.33     Mean   :  0.39   
##  3rd Qu.:  1.00    3rd Qu.:  1.00    3rd Qu.: 0.00     3rd Qu.:  0.00   
##  Max.   :106.00    Max.   :315.00    Max.   :78.00     Max.   :115.00   
##                                                                         
##  checkin_info.7.2 checkin_info.8.0 checkin_info.9.1 checkin_info.13.0
##  Min.   :  0.00   Min.   :  0.0    Min.   :  0.00   Min.   :  0.0    
##  1st Qu.:  0.00   1st Qu.:  0.0    1st Qu.:  0.00   1st Qu.:  0.0    
##  Median :  0.00   Median :  0.0    Median :  0.00   Median :  0.0    
##  Mean   :  0.34   Mean   :  0.4    Mean   :  0.33   Mean   :  0.7    
##  3rd Qu.:  0.00   3rd Qu.:  0.0    3rd Qu.:  0.00   3rd Qu.:  1.0    
##  Max.   :205.00   Max.   :341.0    Max.   :190.00   Max.   :164.0    
##                                                                      
##  checkin_info.22.6 checkin_info.23.2 checkin_info.8.3 checkin_info.16.4
##  Min.   :  0.00    Min.   : 0.0      Min.   :  0.00   Min.   :  0.00   
##  1st Qu.:  0.00    1st Qu.: 0.0      1st Qu.:  0.00   1st Qu.:  0.00   
##  Median :  0.00    Median : 0.0      Median :  0.00   Median :  0.00   
##  Mean   :  0.17    Mean   : 0.1      Mean   :  0.39   Mean   :  1.04   
##  3rd Qu.:  0.00    3rd Qu.: 0.0      3rd Qu.:  0.00   3rd Qu.:  1.00   
##  Max.   :192.00    Max.   :35.0      Max.   :240.00   Max.   :279.00   
##                                                                        
##  checkin_info.20.4 checkin_info.21.4 checkin_info.21.5 checkin_info.23.3
##  Min.   :  0.00    Min.   :  0.00    Min.   : 0.00     Min.   : 0.00    
##  1st Qu.:  0.00    1st Qu.:  0.00    1st Qu.: 0.00     1st Qu.: 0.00    
##  Median :  0.00    Median :  0.00    Median : 0.00     Median : 0.00    
##  Mean   :  1.21    Mean   :  0.86    Mean   : 0.83     Mean   : 0.14    
##  3rd Qu.:  1.00    3rd Qu.:  0.00    3rd Qu.: 0.00     3rd Qu.: 0.00    
##  Max.   :144.00    Max.   :157.00    Max.   :59.00     Max.   :51.00    
##                                                                         
##  checkin_info.2.3 checkin_info.2.4 checkin_info.20.2 checkin_info.21.3
##  Min.   :0.000    Min.   :0.000    Min.   :  0.00    Min.   :  0.00   
##  1st Qu.:0.000    1st Qu.:0.000    1st Qu.:  0.00    1st Qu.:  0.00   
##  Median :0.000    Median :0.000    Median :  0.00    Median :  0.00   
##  Mean   :0.014    Mean   :0.015    Mean   :  0.69    Mean   :  0.45   
##  3rd Qu.:0.000    3rd Qu.:0.000    3rd Qu.:  1.00    3rd Qu.:  0.00   
##  Max.   :6.000    Max.   :4.000    Max.   :110.00    Max.   :173.00   
##                                                                       
##  checkin_info.22.4 checkin_info.3.2 checkin_info.3.4 checkin_info.4.2
##  Min.   :  0.0     Min.   :0.00     Min.   : 0.000   Min.   : 0.00   
##  1st Qu.:  0.0     1st Qu.:0.00     1st Qu.: 0.000   1st Qu.: 0.00   
##  Median :  0.0     Median :0.00     Median : 0.000   Median : 0.00   
##  Mean   :  0.6     Mean   :0.01     Mean   : 0.014   Mean   : 0.04   
##  3rd Qu.:  0.0     3rd Qu.:0.00     3rd Qu.: 0.000   3rd Qu.: 0.00   
##  Max.   :133.0     Max.   :7.00     Max.   :15.000   Max.   :57.00   
##                                                                      
##  checkin_info.4.4 checkin_info.5.1 checkin_info.5.2 checkin_info.5.3
##  Min.   : 0.00    Min.   :  0.00   Min.   : 0.00    Min.   :  0.00  
##  1st Qu.: 0.00    1st Qu.:  0.00   1st Qu.: 0.00    1st Qu.:  0.00  
##  Median : 0.00    Median :  0.00   Median : 0.00    Median :  0.00  
##  Mean   : 0.04    Mean   :  0.11   Mean   : 0.11    Mean   :  0.11  
##  3rd Qu.: 0.00    3rd Qu.:  0.00   3rd Qu.: 0.00    3rd Qu.:  0.00  
##  Max.   :43.00    Max.   :113.00   Max.   :99.00    Max.   :130.00  
##                                                                     
##  checkin_info.5.6 checkin_info.6.0 checkin_info.6.1 checkin_info.6.5
##  Min.   : 0.00    Min.   :  0.00   Min.   :  0.00   Min.   :  0.00  
##  1st Qu.: 0.00    1st Qu.:  0.00   1st Qu.:  0.00   1st Qu.:  0.00  
##  Median : 0.00    Median :  0.00   Median :  0.00   Median :  0.00  
##  Mean   : 0.03    Mean   :  0.19   Mean   :  0.21   Mean   :  0.14  
##  3rd Qu.: 0.00    3rd Qu.:  0.00   3rd Qu.:  0.00   3rd Qu.:  0.00  
##  Max.   :62.00    Max.   :187.00   Max.   :142.00   Max.   :119.00  
##                                                                     
##  checkin_info.7.0 checkin_info.7.1 checkin_info.7.3 checkin_info.8.1
##  Min.   :  0.00   Min.   :  0.00   Min.   :  0.00   Min.   :  0.00  
##  1st Qu.:  0.00   1st Qu.:  0.00   1st Qu.:  0.00   1st Qu.:  0.00  
##  Median :  0.00   Median :  0.00   Median :  0.00   Median :  0.00  
##  Mean   :  0.31   Mean   :  0.34   Mean   :  0.37   Mean   :  0.36  
##  3rd Qu.:  0.00   3rd Qu.:  0.00   3rd Qu.:  0.00   3rd Qu.:  0.00  
##  Max.   :283.00   Max.   :178.00   Max.   :258.00   Max.   :228.00  
##                                                                     
##  checkin_info.9.0 checkin_info.9.3 checkin_info.0.3 checkin_info.22.5
##  Min.   :  0.00   Min.   :  0.00   Min.   : 0.000   Min.   :  0.00   
##  1st Qu.:  0.00   1st Qu.:  0.00   1st Qu.: 0.000   1st Qu.:  0.00   
##  Median :  0.00   Median :  0.00   Median : 0.000   Median :  0.00   
##  Mean   :  0.36   Mean   :  0.34   Mean   : 0.064   Mean   :  0.57   
##  3rd Qu.:  0.00   3rd Qu.:  0.00   3rd Qu.: 0.000   3rd Qu.:  0.00   
##  Max.   :207.00   Max.   :191.00   Max.   :13.000   Max.   :106.00   
##                                                                      
##  checkin_info.22.0 checkin_info.22.3 checkin_info.23.1 checkin_info.0.1
##  Min.   : 0.00     Min.   :  0.00    Min.   : 0.00     Min.   : 0.000  
##  1st Qu.: 0.00     1st Qu.:  0.00    1st Qu.: 0.00     1st Qu.: 0.000  
##  Median : 0.00     Median :  0.00    Median : 0.00     Median : 0.000  
##  Mean   : 0.15     Mean   :  0.26    Mean   : 0.09     Mean   : 0.044  
##  3rd Qu.: 0.00     3rd Qu.:  0.00    3rd Qu.: 0.00     3rd Qu.: 0.000  
##  Max.   :96.00     Max.   :129.00    Max.   :34.00     Max.   :13.000  
##                                                                        
##  checkin_info.22.2 checkin_info.23.6 checkin_info.0.6 checkin_info.1.0
##  Min.   : 0.00     Min.   : 0.00     Min.   : 0.00    Min.   :0.000   
##  1st Qu.: 0.00     1st Qu.: 0.00     1st Qu.: 0.00    1st Qu.:0.000   
##  Median : 0.00     Median : 0.00     Median : 0.00    Median :0.000   
##  Mean   : 0.12     Mean   : 0.09     Mean   : 0.26    Mean   :0.021   
##  3rd Qu.: 0.00     3rd Qu.: 0.00     3rd Qu.: 0.00    3rd Qu.:0.000   
##  Max.   :53.00     Max.   :89.00     Max.   :86.00    Max.   :9.000   
##                                                                       
##  checkin_info.22.1 checkin_info.23.5 checkin_info.1.5 checkin_info.5.0
##  Min.   : 0.00     Min.   :  0.00    Min.   : 0.000   Min.   :  0.0   
##  1st Qu.: 0.00     1st Qu.:  0.00    1st Qu.: 0.000   1st Qu.:  0.0   
##  Median : 0.00     Median :  0.00    Median : 0.000   Median :  0.0   
##  Mean   : 0.17     Mean   :  0.41    Mean   : 0.103   Mean   :  0.1   
##  3rd Qu.: 0.00     3rd Qu.:  0.00    3rd Qu.: 0.000   3rd Qu.:  0.0   
##  Max.   :73.00     Max.   :115.00    Max.   :31.000   Max.   :148.0   
##                                                                       
##  checkin_info.0.4 checkin_info.1.4 checkin_info.3.1 checkin_info.1.2
##  Min.   : 0.00    Min.   :0.000    Min.   :0.000    Min.   :0.000   
##  1st Qu.: 0.00    1st Qu.:0.000    1st Qu.:0.000    1st Qu.:0.000   
##  Median : 0.00    Median :0.000    Median :0.000    Median :0.000   
##  Mean   : 0.07    Mean   :0.034    Mean   :0.006    Mean   :0.025   
##  3rd Qu.: 0.00    3rd Qu.:0.000    3rd Qu.:0.000    3rd Qu.:0.000   
##  Max.   :32.00    Max.   :9.000    Max.   :4.000    Max.   :8.000   
##                                                                     
##  checkin_info.4.3 checkin_info.4.6 checkin_info.5.5 checkin_info.4.5
##  Min.   : 0.00    Min.   : 0.000   Min.   : 0.00    Min.   : 0.00   
##  1st Qu.: 0.00    1st Qu.: 0.000   1st Qu.: 0.00    1st Qu.: 0.00   
##  Median : 0.00    Median : 0.000   Median : 0.00    Median : 0.00   
##  Mean   : 0.04    Mean   : 0.016   Mean   : 0.05    Mean   : 0.02   
##  3rd Qu.: 0.00    3rd Qu.: 0.000   3rd Qu.: 0.00    3rd Qu.: 0.00   
##  Max.   :80.00    Max.   :25.000   Max.   :66.00    Max.   :36.00   
##                                                                     
##  checkin_info.4.0 checkin_info.4.1 checkin_info.1.3 checkin_info.3.3
##  Min.   : 0.00    Min.   : 0.00    Min.   :0.000    Min.   : 0.000  
##  1st Qu.: 0.00    1st Qu.: 0.00    1st Qu.:0.000    1st Qu.: 0.000  
##  Median : 0.00    Median : 0.00    Median :0.000    Median : 0.000  
##  Mean   : 0.03    Mean   : 0.04    Mean   :0.029    Mean   : 0.012  
##  3rd Qu.: 0.00    3rd Qu.: 0.00    3rd Qu.:0.000    3rd Qu.: 0.000  
##  Max.   :55.00    Max.   :44.00    Max.   :7.000    Max.   :13.000  
##                                                                     
##  checkin_info.2.0 checkin_info.2.1 checkin_info.2.2 checkin_info.total
##  Min.   :0.000    Min.   :0.000    Min.   :0.000    Min.   :    3     
##  1st Qu.:0.000    1st Qu.:0.000    1st Qu.:0.000    1st Qu.:   11     
##  Median :0.000    Median :0.000    Median :0.000    Median :   30     
##  Mean   :0.011    Mean   :0.009    Mean   :0.011    Mean   :   94     
##  3rd Qu.:0.000    3rd Qu.:0.000    3rd Qu.:0.000    3rd Qu.:   91     
##  Max.   :4.000    Max.   :4.000    Max.   :4.000    Max.   :22972     
##                                                                       
##       type                      business_id  
##  checkin:8282   _-9pMxBWtG_x8l4rHWBasg:   1  
##                 _0DvMQAs9KvA58Pf3Z4ltg:   1  
##                 _1QQZuf4zZOyFCvXc0o6Vg:   1  
##                 _2sa9ikO_Xm5xe2lqHQZUg:   1  
##                 _4_84uCAFDI2VHHEQz5f2g:   1  
##                 _4kkqB-ZqnnD7u_Xjnmtgg:   1  
##                 (Other)               :8276  
## 
## $review
##   votes.funny    votes.useful      votes.cool      votes.total    
##  Min.   : 0.0   Min.   :  0.00   Min.   :  0.00   Min.   :  0.00  
##  1st Qu.: 0.0   1st Qu.:  0.00   1st Qu.:  0.00   1st Qu.:  0.00  
##  Median : 0.0   Median :  1.00   Median :  0.00   Median :  1.00  
##  Mean   : 0.7   Mean   :  1.39   Mean   :  0.87   Mean   :  2.95  
##  3rd Qu.: 1.0   3rd Qu.:  2.00   3rd Qu.:  1.00   3rd Qu.:  3.00  
##  Max.   :70.0   Max.   :120.00   Max.   :117.00   Max.   :237.00  
##                                                                   
##                    user_id                        review_id     
##  fczQCSmaWF78toLEmb0Zsw:   588   ___XYEos-RIkPsQwplRYyw:     1  
##  90a6z--_CUrl84aCzZyPsg:   506   __-32b1OWffXPHRrRgWJFQ:     1  
##  0CMz8YaO3f8xu4KqQgKb9Q:   473   __0G7C28bnbwxdtanv-1Bg:     1  
##  4ozupHULqGyO42s3zNUzOQ:   442   __0iPx4ocdLZk58kLpeakQ:     1  
##  joIzw_aUiNvBTuGoytrH7g:   392   __0tg60rGHLDR-MqztVUyA:     1  
##  0bNXP9quoJEgyVZu9ipGgQ:   376   __0z2Ar1Vs96LII1Gkw2qg:     1  
##  (Other)               :227130   (Other)               :229901  
##                  business_id         stars           date           
##  hW0Ne_HTHEAgGF1rAdmR-g:   844   Min.   :1.00   Min.   :2005-03-07  
##  VVeogjZya58oiTxK7qUjAQ:   794   1st Qu.:3.00   1st Qu.:2010-02-22  
##  JokKtdXU7zXHcr20Lrk29A:   731   Median :4.00   Median :2011-04-25  
##  ntN85eu27C04nwyPa8IHtw:   679   Mean   :3.77   Mean   :2011-01-12  
##  EWMwV5V9BxNs_U6nNVMeqw:   645   3rd Qu.:5.00   3rd Qu.:2012-03-15  
##  V1nEpIRmEa1768oj_tuxeQ:   588   Max.   :5.00   Max.   :2013-01-05  
##  (Other)               :225626                                      
##      type                             .              the       
##  review:229907   Min.   :   0   Min.   :  0.0   Min.   : 0.00  
##                  1st Qu.:  53   1st Qu.:  4.0   1st Qu.: 2.00  
##                  Median : 100   Median :  7.0   Median : 4.00  
##                  Mean   : 131   Mean   :  9.7   Mean   : 5.52  
##                  3rd Qu.: 173   3rd Qu.: 13.0   3rd Qu.: 8.00  
##                  Max.   :1027   Max.   :508.0   Max.   :75.00  
##                                                                
##        ,             and              I               a        
##  Min.   : 0.0   Min.   : 0.00   Min.   : 0.00   Min.   : 0.00  
##  1st Qu.: 1.0   1st Qu.: 1.00   1st Qu.: 1.00   1st Qu.: 1.00  
##  Median : 3.0   Median : 3.00   Median : 2.00   Median : 2.00  
##  Mean   : 5.3   Mean   : 4.25   Mean   : 3.51   Mean   : 3.47  
##  3rd Qu.: 7.0   3rd Qu.: 6.00   3rd Qu.: 5.00   3rd Qu.: 5.00  
##  Max.   :91.0   Max.   :54.00   Max.   :77.00   Max.   :42.00  
##                                                                
##        \n               to           of             was       
##  Min.   :  0.00   Min.   : 0   Min.   : 0.00   Min.   : 0.00  
##  1st Qu.:  0.00   1st Qu.: 1   1st Qu.: 0.00   1st Qu.: 0.00  
##  Median :  2.00   Median : 2   Median : 1.00   Median : 1.00  
##  Mean   :  3.38   Mean   : 3   Mean   : 2.05   Mean   : 2.01  
##  3rd Qu.:  6.00   3rd Qu.: 4   3rd Qu.: 3.00   3rd Qu.: 3.00  
##  Max.   :131.00   Max.   :51   Max.   :38.00   Max.   :36.00  
##                                                               
##        is             for              it              in       
##  Min.   : 0.00   Min.   : 0.00   Min.   : 0.00   Min.   : 0.00  
##  1st Qu.: 0.00   1st Qu.: 0.00   1st Qu.: 0.00   1st Qu.: 0.00  
##  Median : 1.00   Median : 1.00   Median : 1.00   Median : 1.00  
##  Mean   : 1.71   Mean   : 1.47   Mean   : 1.45   Mean   : 1.43  
##  3rd Qu.: 3.00   3rd Qu.: 2.00   3rd Qu.: 2.00   3rd Qu.: 2.00  
##  Max.   :31.00   Max.   :20.00   Max.   :26.00   Max.   :28.00  
##                                                                 
##        !              The             that            with      
##  Min.   : 0.00   Min.   : 0.00   Min.   : 0.00   Min.   : 0.00  
##  1st Qu.: 0.00   1st Qu.: 0.00   1st Qu.: 0.00   1st Qu.: 0.00  
##  Median : 0.00   Median : 1.00   Median : 1.00   Median : 1.00  
##  Mean   : 1.36   Mean   : 1.16   Mean   : 1.16   Mean   : 1.07  
##  3rd Qu.: 2.00   3rd Qu.: 2.00   3rd Qu.: 2.00   3rd Qu.: 2.00  
##  Max.   :93.00   Max.   :26.00   Max.   :39.00   Max.   :25.00  
##                                                                 
##       but              you              my              on        
##  Min.   : 0.000   Min.   : 0.00   Min.   : 0.00   Min.   : 0.000  
##  1st Qu.: 0.000   1st Qu.: 0.00   1st Qu.: 0.00   1st Qu.: 0.000  
##  Median : 1.000   Median : 0.00   Median : 0.00   Median : 0.000  
##  Mean   : 0.966   Mean   : 0.93   Mean   : 0.91   Mean   : 0.895  
##  3rd Qu.: 1.000   3rd Qu.: 1.00   3rd Qu.: 1.00   3rd Qu.: 1.000  
##  Max.   :16.000   Max.   :36.00   Max.   :40.00   Max.   :16.000  
##                                                                   
##       have             this             had             are        
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.00   Min.   : 0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.00   1st Qu.: 0.000  
##  Median : 0.000   Median : 0.000   Median : 0.00   Median : 0.000  
##  Mean   : 0.874   Mean   : 0.798   Mean   : 0.71   Mean   : 0.688  
##  3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.: 1.00   3rd Qu.: 1.000  
##  Max.   :24.000   Max.   :20.000   Max.   :17.00   Max.   :23.000  
##                                                                    
##       they             not               )             place      
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.00   Min.   : 0.00  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.00   1st Qu.: 0.00  
##  Median : 0.000   Median : 0.000   Median : 0.00   Median : 0.00  
##  Mean   : 0.685   Mean   : 0.676   Mean   : 0.65   Mean   : 0.65  
##  3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.: 1.00   3rd Qu.: 1.00  
##  Max.   :19.000   Max.   :22.000   Max.   :34.00   Max.   :12.00  
##                                                                   
##        at              good             were              (        
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   : 0.00  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.00  
##  Median : 0.000   Median : 0.000   Median : 0.000   Median : 0.00  
##  Mean   : 0.637   Mean   : 0.623   Mean   : 0.599   Mean   : 0.58  
##  3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.: 1.00  
##  Max.   :16.000   Max.   :15.000   Max.   :21.000   Max.   :34.00  
##                                                                    
##       food              "               we               so        
##  Min.   : 0.000   Min.   : 0.00   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 0.000   1st Qu.: 0.00   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.000   Median : 0.00   Median : 0.000   Median : 0.000  
##  Mean   : 0.572   Mean   : 0.57   Mean   : 0.557   Mean   : 0.554  
##  3rd Qu.: 1.000   3rd Qu.: 0.00   3rd Qu.: 0.000   3rd Qu.: 1.000  
##  Max.   :14.000   Max.   :80.00   Max.   :31.000   Max.   :14.000  
##                                                                    
##        -                be               as              like       
##  Min.   :  0.00   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.:  0.00   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median :  0.00   Median : 0.000   Median : 0.000   Median : 0.000  
##  Mean   :  0.55   Mean   : 0.544   Mean   : 0.513   Mean   : 0.487  
##  3rd Qu.:  0.00   3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.: 1.000  
##  Max.   :283.00   Max.   :13.000   Max.   :14.000   Max.   :27.000  
##                                                                     
##        me             out             there             here      
##  Min.   : 0.00   Min.   : 0.000   Min.   : 0.000   Min.   : 0.00  
##  1st Qu.: 0.00   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.00  
##  Median : 0.00   Median : 0.000   Median : 0.000   Median : 0.00  
##  Mean   : 0.46   Mean   : 0.444   Mean   : 0.436   Mean   : 0.42  
##  3rd Qu.: 1.00   3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.: 1.00  
##  Max.   :26.00   Max.   :17.000   Max.   :17.000   Max.   :55.00  
##                                                                   
##       just           great             all              very       
##  Min.   : 0.00   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 0.00   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.00   Median : 0.000   Median : 0.000   Median : 0.000  
##  Mean   : 0.42   Mean   : 0.418   Mean   : 0.409   Mean   : 0.391  
##  3rd Qu.: 1.00   3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.: 1.000  
##  Max.   :13.00   Max.   :10.000   Max.   :10.000   Max.   :13.000  
##                                                                    
##        or             get              one              from       
##  Min.   : 0.00   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 0.00   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.00   Median : 0.000   Median : 0.000   Median : 0.000  
##  Mean   : 0.38   Mean   : 0.374   Mean   : 0.364   Mean   : 0.348  
##  3rd Qu.: 1.00   3rd Qu.: 1.000   3rd Qu.: 1.000   3rd Qu.: 1.000  
##  Max.   :12.00   Max.   :11.000   Max.   :11.000   Max.   :15.000  
##                                                                    
##       time            their              up             about       
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.000   Median : 0.000   Median : 0.000   Median : 0.000  
##  Mean   : 0.346   Mean   : 0.339   Mean   : 0.336   Mean   : 0.328  
##  3rd Qu.: 1.000   3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.: 0.000  
##  Max.   :11.000   Max.   :23.000   Max.   :12.000   Max.   :11.000  
##                                                                     
##        if               go               We            really      
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.00   Min.   : 0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.00   1st Qu.: 0.000  
##  Median : 0.000   Median : 0.000   Median : 0.00   Median : 0.000  
##  Mean   : 0.327   Mean   : 0.326   Mean   : 0.32   Mean   : 0.318  
##  3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.: 0.00   3rd Qu.: 0.000  
##  Max.   :11.000   Max.   :12.000   Max.   :15.00   Max.   :13.000  
##                                                                    
##        ?              some            would           back       
##  Min.   : 0.00   Min.   : 0.000   Min.   : 0.0   Min.   : 0.000  
##  1st Qu.: 0.00   1st Qu.: 0.000   1st Qu.: 0.0   1st Qu.: 0.000  
##  Median : 0.00   Median : 0.000   Median : 0.0   Median : 0.000  
##  Mean   : 0.32   Mean   : 0.313   Mean   : 0.3   Mean   : 0.298  
##  3rd Qu.: 0.00   3rd Qu.: 0.000   3rd Qu.: 0.0   3rd Qu.: 0.000  
##  Max.   :36.00   Max.   :16.000   Max.   :11.0   Max.   :11.000  
##                                                                  
##        an             been            which             when       
##  Min.   :0.000   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.:0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median :0.000   Median : 0.000   Median : 0.000   Median : 0.000  
##  Mean   :0.296   Mean   : 0.285   Mean   : 0.285   Mean   : 0.281  
##  3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.: 0.000  
##  Max.   :9.000   Max.   :11.000   Max.   :15.000   Max.   :10.000  
##                                                                    
##       our            service             It              can        
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.000   Median : 0.000   Median : 0.000   Median : 0.000  
##  Mean   : 0.281   Mean   : 0.277   Mean   : 0.276   Mean   : 0.275  
##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.: 0.000  
##  Max.   :22.000   Max.   :11.000   Max.   :11.000   Max.   :11.000  
##                                                                     
##       your            more              $              They       
##  Min.   : 0.00   Min.   : 0.000   Min.   : 0.00   Min.   : 0.000  
##  1st Qu.: 0.00   1st Qu.: 0.000   1st Qu.: 0.00   1st Qu.: 0.000  
##  Median : 0.00   Median : 0.000   Median : 0.00   Median : 0.000  
##  Mean   : 0.27   Mean   : 0.259   Mean   : 0.26   Mean   : 0.256  
##  3rd Qu.: 0.00   3rd Qu.: 0.000   3rd Qu.: 0.00   3rd Qu.: 0.000  
##  Max.   :22.00   Max.   :27.000   Max.   :36.00   Max.   :12.000  
##                                                                   
##       will             what             This            only      
##  Min.   : 0.000   Min.   : 0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median : 0.000   Median : 0.000   Median :0.000   Median :0.000  
##  Mean   : 0.251   Mean   : 0.245   Mean   :0.238   Mean   :0.235  
##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :16.000   Max.   :11.000   Max.   :8.000   Max.   :8.000  
##                                                                   
##       it's              by               :               I'm        
##  Min.   : 0.000   Min.   : 0.000   Min.   :  0.00   Min.   : 0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:  0.00   1st Qu.: 0.000  
##  Median : 0.000   Median : 0.000   Median :  0.00   Median : 0.000  
##  Mean   : 0.233   Mean   : 0.232   Mean   :  0.23   Mean   : 0.219  
##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:  0.00   3rd Qu.: 0.000  
##  Max.   :11.000   Max.   :11.000   Max.   :127.00   Max.   :11.000  
##                                                                     
##       too            little           has             don't      
##  Min.   :0.000   Min.   :0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median : 0.000   Median :0.000  
##  Mean   :0.219   Mean   :0.214   Mean   : 0.213   Mean   :0.212  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :8.000   Max.   :9.000   Max.   :10.000   Max.   :9.000  
##                                                                  
##        My             other            them             also      
##  Min.   : 0.000   Min.   :0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.: 0.000   1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median : 0.000   Median :0.000   Median : 0.000   Median :0.000  
##  Mean   : 0.211   Mean   :0.209   Mean   : 0.207   Mean   :0.206  
##  3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :12.000   Max.   :8.000   Max.   :14.000   Max.   :8.000  
##                                                                   
##     because            nice            I've           than      
##  Min.   : 0.000   Min.   :0.000   Min.   : 0.0   Min.   :0.000  
##  1st Qu.: 0.000   1st Qu.:0.000   1st Qu.: 0.0   1st Qu.:0.000  
##  Median : 0.000   Median :0.000   Median : 0.0   Median :0.000  
##  Mean   : 0.206   Mean   :0.201   Mean   : 0.2   Mean   :0.192  
##  3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.: 0.0   3rd Qu.:0.000  
##  Max.   :11.000   Max.   :8.000   Max.   :55.0   Max.   :7.000  
##                                                                 
##        no             always           even             do       
##  Min.   : 0.000   Min.   :0.000   Min.   :0.000   Min.   : 0.00  
##  1st Qu.: 0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 0.00  
##  Median : 0.000   Median :0.000   Median :0.000   Median : 0.00  
##  Mean   : 0.188   Mean   :0.187   Mean   :0.187   Mean   : 0.19  
##  3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.: 0.00  
##  Max.   :25.000   Max.   :9.000   Max.   :9.000   Max.   :39.00  
##                                                                  
##       got              love              us              well      
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median : 0.000   Median : 0.000   Median : 0.000   Median :0.000  
##  Mean   : 0.185   Mean   : 0.185   Mean   : 0.181   Mean   :0.176  
##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :11.000   Max.   :12.000   Max.   :15.000   Max.   :8.000  
##                                                                    
##       much            menu             best           people      
##  Min.   :0.000   Min.   : 0.000   Min.   :0.000   Min.   : 0.000  
##  1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:0.000   1st Qu.: 0.000  
##  Median :0.000   Median : 0.000   Median :0.000   Median : 0.000  
##  Mean   :0.174   Mean   : 0.171   Mean   :0.168   Mean   : 0.166  
##  3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.: 0.000  
##  Max.   :9.000   Max.   :12.000   Max.   :9.000   Max.   :26.000  
##                                                                   
##      pretty         ordered            try          restaurant    
##  Min.   :0.000   Min.   : 0.000   Min.   :0.000   Min.   : 0.000  
##  1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:0.000   1st Qu.: 0.000  
##  Median :0.000   Median : 0.000   Median :0.000   Median : 0.000  
##  Mean   :0.166   Mean   : 0.165   Mean   :0.165   Mean   : 0.164  
##  3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.: 0.000  
##  Max.   :9.000   Max.   :11.000   Max.   :8.000   Max.   :10.000  
##                                                                   
##      order             know            could            didn't     
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   : 0.00  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.00  
##  Median : 0.000   Median : 0.000   Median : 0.000   Median : 0.00  
##  Mean   : 0.159   Mean   : 0.158   Mean   : 0.157   Mean   : 0.15  
##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.: 0.00  
##  Max.   :15.000   Max.   :12.000   Max.   :10.000   Max.   :11.00  
##                                                                    
##       over            think            bar              went       
##  Min.   : 0.000   Min.   :0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 0.000   1st Qu.:0.000   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median : 0.000   Median :0.000   Median : 0.000   Median : 0.000  
##  Mean   : 0.149   Mean   :0.148   Mean   : 0.148   Mean   : 0.144  
##  3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.: 0.000  
##  Max.   :12.000   Max.   :8.000   Max.   :16.000   Max.   :11.000  
##                                                                    
##      going            make          friendly        chicken      
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   : 0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median : 0.000  
##  Mean   :0.143   Mean   :0.143   Mean   :0.142   Mean   : 0.142  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.: 0.000  
##  Max.   :7.000   Max.   :9.000   Max.   :6.000   Max.   :12.000  
##                                                                  
##      better            he              did               If       
##  Min.   :0.000   Min.   : 0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median :0.000   Median : 0.000   Median : 0.000   Median :0.000  
##  Mean   :0.142   Mean   : 0.142   Mean   : 0.141   Mean   :0.141  
##  3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :9.000   Max.   :22.000   Max.   :14.000   Max.   :7.000  
##                                                                   
##      never            am              way              It's      
##  Min.   :0.00   Min.   : 0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.:0.00   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median :0.00   Median : 0.000   Median : 0.000   Median :0.000  
##  Mean   :0.14   Mean   : 0.139   Mean   : 0.138   Mean   :0.136  
##  3rd Qu.:0.00   3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :7.00   Max.   :10.000   Max.   :10.000   Max.   :9.000  
##                                                                  
##      after           first           staff             off       
##  Min.   :0.000   Min.   :0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median : 0.000   Median :0.000  
##  Mean   :0.136   Mean   :0.136   Mean   : 0.134   Mean   :0.134  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :7.000   Max.   :7.000   Max.   :11.000   Max.   :7.000  
##                                                                  
##      lunch            night              i              she       
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.00   Min.   : 0.00  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.00   1st Qu.: 0.00  
##  Median : 0.000   Median : 0.000   Median : 0.00   Median : 0.00  
##  Mean   : 0.132   Mean   : 0.131   Mean   : 0.13   Mean   : 0.13  
##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.: 0.00   3rd Qu.: 0.00  
##  Max.   :10.000   Max.   :10.000   Max.   :47.00   Max.   :35.00  
##                                                                   
##       few            right            who              say       
##  Min.   :0.000   Min.   :0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median : 0.000   Median :0.000  
##  Mean   :0.129   Mean   :0.128   Mean   : 0.128   Mean   :0.127  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :7.000   Max.   :8.000   Max.   :11.000   Max.   :7.000  
##                                                                  
##       want             came             two             made      
##  Min.   : 0.000   Min.   : 0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median : 0.000   Median : 0.000   Median :0.000   Median :0.000  
##  Mean   : 0.127   Mean   : 0.127   Mean   :0.124   Mean   :0.123  
##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :14.000   Max.   :10.000   Max.   :8.000   Max.   :9.000  
##                                                                   
##       how          delicious         pizza            again      
##  Min.   :0.000   Min.   :0.000   Min.   : 0.000   Min.   : 0.00  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 0.000   1st Qu.: 0.00  
##  Median :0.000   Median :0.000   Median : 0.000   Median : 0.00  
##  Mean   :0.122   Mean   :0.121   Mean   : 0.121   Mean   : 0.12  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.: 0.00  
##  Max.   :7.000   Max.   :8.000   Max.   :18.000   Max.   :19.00  
##                                                                  
##       any            come          around           cheese      
##  Min.   :0.00   Min.   :0.00   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.:0.00   1st Qu.:0.00   1st Qu.: 0.000   1st Qu.: 0.000  
##  Median :0.00   Median :0.00   Median : 0.000   Median : 0.000  
##  Mean   :0.12   Mean   :0.12   Mean   : 0.119   Mean   : 0.118  
##  3rd Qu.:0.00   3rd Qu.:0.00   3rd Qu.: 0.000   3rd Qu.: 0.000  
##  Max.   :6.00   Max.   :7.00   Max.   :11.000   Max.   :15.000  
##                                                                 
##       then            down            see            fresh      
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.117   Mean   :0.116   Mean   :0.115   Mean   :0.115  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :9.000   Max.   :9.000   Max.   :8.000   Max.   :9.000  
##                                                                 
##       eat              &             before           find      
##  Min.   :0.000   Min.   : 0.00   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.: 0.00   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median : 0.00   Median :0.000   Median :0.000  
##  Mean   :0.115   Mean   : 0.11   Mean   :0.113   Mean   :0.112  
##  3rd Qu.:0.000   3rd Qu.: 0.00   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :7.000   Max.   :40.00   Max.   :8.000   Max.   :8.000  
##                                                                 
##      sauce             day               So            And       
##  Min.   : 0.000   Min.   : 0.000   Min.   :0.00   Min.   : 0.00  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.00   1st Qu.: 0.00  
##  Median : 0.000   Median : 0.000   Median :0.00   Median : 0.00  
##  Mean   : 0.112   Mean   : 0.111   Mean   :0.11   Mean   : 0.11  
##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:0.00   3rd Qu.: 0.00  
##  Max.   :13.000   Max.   :12.000   Max.   :9.00   Max.   :11.00  
##                                                                  
##       wait            But             sure           salad       
##  Min.   : 0.00   Min.   :0.000   Min.   :0.000   Min.   : 0.000  
##  1st Qu.: 0.00   1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 0.000  
##  Median : 0.00   Median :0.000   Median :0.000   Median : 0.000  
##  Mean   : 0.11   Mean   :0.109   Mean   :0.109   Mean   : 0.108  
##  3rd Qu.: 0.00   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.: 0.000  
##  Max.   :10.00   Max.   :8.000   Max.   :6.000   Max.   :14.000  
##                                                                  
##    experience         still         something          bit       
##  Min.   : 0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.: 0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median : 0.000   Median :0.000   Median :0.000   Median :0.000  
##  Mean   : 0.108   Mean   :0.108   Mean   :0.108   Mean   :0.107  
##  3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :11.000   Max.   :8.000   Max.   :7.000   Max.   :7.000  
##                                                                  
##       area             said             take           times      
##  Min.   : 0.000   Min.   : 0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median : 0.000   Median : 0.000   Median :0.000   Median :0.000  
##  Mean   : 0.107   Mean   : 0.107   Mean   :0.106   Mean   :0.105  
##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :12.000   Max.   :14.000   Max.   :7.000   Max.   :8.000  
##                                                                   
##       her               2                '               ever      
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median : 0.000   Median : 0.000   Median : 0.000   Median :0.000  
##  Mean   : 0.105   Mean   : 0.104   Mean   : 0.103   Mean   :0.103  
##  3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :31.000   Max.   :24.000   Max.   :24.000   Max.   :6.000  
##                                                                    
##    definitely        though          into          new       
##  Min.   :0.000   Min.   : 0.0   Min.   :0.0   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.: 0.0   1st Qu.:0.0   1st Qu.:0.000  
##  Median :0.000   Median : 0.0   Median :0.0   Median :0.000  
##  Mean   :0.101   Mean   : 0.1   Mean   :0.1   Mean   :0.099  
##  3rd Qu.:0.000   3rd Qu.: 0.0   3rd Qu.:0.0   3rd Qu.:0.000  
##  Max.   :7.000   Max.   :10.0   Max.   :9.0   Max.   :9.000  
##                                                              
##      thing            give            bad            meal      
##  Min.   :0.000   Min.   :0.000   Min.   : 0.0   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.: 0.0   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median : 0.0   Median :0.000  
##  Mean   :0.099   Mean   :0.098   Mean   : 0.1   Mean   :0.098  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.: 0.0   3rd Qu.:0.000  
##  Max.   :6.000   Max.   :6.000   Max.   :52.0   Max.   :9.000  
##                                                                
##       most          location          happy            since      
##  Min.   :0.000   Min.   : 0.000   Min.   : 0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:0.000  
##  Median :0.000   Median : 0.000   Median : 0.000   Median :0.000  
##  Mean   :0.098   Mean   : 0.098   Mean   : 0.098   Mean   :0.098  
##  3rd Qu.:0.000   3rd Qu.: 0.000   3rd Qu.: 0.000   3rd Qu.:0.000  
##  Max.   :6.000   Max.   :11.000   Max.   :12.000   Max.   :6.000  
##                                                                   
##       many      
##  Min.   :0.000  
##  1st Qu.:0.000  
##  Median :0.000  
##  Mean   :0.097  
##  3rd Qu.:0.000  
##  Max.   :8.000  
##                 
## 
## $user
##   votes.funny     votes.useful     votes.cool     votes.total   
##  Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
##  1st Qu.:    0   1st Qu.:    1   1st Qu.:    0   1st Qu.:    2  
##  Median :    1   Median :    5   Median :    1   Median :    8  
##  Mean   :   44   Mean   :   77   Mean   :   52   Mean   :  174  
##  3rd Qu.:    7   3rd Qu.:   23   3rd Qu.:    7   3rd Qu.:   37  
##  Max.   :24519   Max.   :24293   Max.   :22410   Max.   :70341  
##                                                                 
##                    user_id            name       average_stars 
##  __7iSC6XCyWW4pHd8Cz0SQ:    1   David   :  465   Min.   :0.00  
##  __b7YV2PZCin-nL2KC4RKg:    1   John    :  447   1st Qu.:3.33  
##  __FXEOrWIjXMOElz2pGlBQ:    1   Michael :  418   Median :3.86  
##  __IH6IiLx8-V0KvEcn1u6g:    1   Chris   :  417   Mean   :3.74  
##  __k3AFYK7Yev9OFyPVSAaw:    1   Mike    :  383   3rd Qu.:4.36  
##  __mbhFpCj377OxiJJozXRQ:    1   Jennifer:  365   Max.   :5.00  
##  (Other)               :43867   (Other) :41378                 
##   review_count    type      
##  Min.   :   1   user:43873  
##  1st Qu.:   2               
##  Median :   7               
##  Mean   :  39               
##  3rd Qu.:  23               
##  Max.   :5807               
## 
```


Merge the data just to be able to then process the data properly. I mean, some businesses' might not be defined in the test data and you have to find them in the training data. At least, I think that's what some were discussing in the forums.


```r
types <- names(trainingProcessed)
names(types) <- types
all <- lapply(types, function(x) {
    ## For lazy code
    y <- trainingProcessed[[x]]
    z <- testProcessed[[x]]
    
    ## Find if the columns match
    toadd <- colnames(y)[!colnames(y) %in% colnames(z)]
    new <- z
    
    ## Add empty columns if necessary (aka, for the votes columns not present
    ## in the test data)
    if (length(toadd) > 0) {
        empty <- data.frame(matrix(NA, ncol = length(toadd), nrow = nrow(z)))
        colnames(empty) <- toadd
        new <- cbind(z, empty)
    }
    
    ## Reorder the columns so the rbind makes sense
    idx <- sapply(colnames(y), function(m) {
        which(colnames(new) == m)
    })
    new <- new[, idx]
    
    ## Finish
    rbind(y, new)
})

## Check that it makes sense
identical(unlist(lapply(all, nrow)), unlist(lapply(types, function(x) {
    nrow(testProcessed[[x]]) + nrow(trainingProcessed[[x]])
})))
```

```
## [1] TRUE
```


Define the function that will process the information for the reviews. The goal is to pull the information from the other tables (business, checkin, user) into a big review table. Then this table can be the input for the modelling functions in R.


```r
## Helper functions for ldply
processUser <- function(x, user.a) {
    i <- which(user.a$user_id == x)
    which.cols <- colnames(user.a) %in% c("name", "average_stars", "review_count")
    if (length(i) >= 1) {
        res <- user.a[i, which.cols]
    } else {
        res <- user.a[i, which.cols]
        res[1, ] <- rep(NA, length(which.cols))
    }
    if (length(i) > 1) {
        warning("Unexpected case. Only using the info from the first match.")
        print(user.a[i, which.cols])
    }
    ## Avoid variable names overlap with columns from other tables
    colnames(res) <- paste0("user.", colnames(res))
    return(res)
}

processCheckin <- function(x, checkin.a) {
    i <- which(checkin.a$business_id == x)
    which.cols <- !colnames(checkin.a) %in% c("business_id", "type")
    if (length(i) >= 1) {
        res <- checkin.a[i, which.cols]
    } else {
        res <- checkin.a[i, which.cols]
        res[1, ] <- rep(NA, length(which.cols))
    }
    if (length(i) > 1) {
        warning("Unexpected case. Only using the info from the first match.")
        print(checkin.a[i, which.cols])
    }
    ## Avoid variable names overlap with columns from other tables
    colnames(res) <- gsub("_info", "", colnames(res))
    return(res)
}

processBusiness <- function(x, business.a) {
    i <- which(business.a$business_id == x)
    ## Get the zipcode
    business.a$zipcode <- as.integer(gsub("^.*(AZ|CA|CO|SC)(| )", "", business.a$full_address))
    
    which.cols <- !colnames(business.a) %in% c("business_id", "state", "type", 
        "full_address", "neighborhoods", "name")
    if (length(i) >= 1) {
        res <- business.a[i, which.cols]
    } else {
        res <- business.a[i, which.cols]
        res[1, ] <- rep(NA, length(which.cols))
    }
    if (length(i) > 1) {
        warning("Unexpected case. Only using the info from the first match.")
        print(business.a[i, which.cols])
    }
    ## Avoid variable names overlap with columns from other tables
    colnames(res) <- paste0("business.", colnames(res))
    return(res)
}

processFun <- function(input, all, parallel = TRUE) {
    
    ## Test
    if (TRUE) {
        input <- lapply(testProcessed, head, n = 10)
    }
    
    ## Split into objects to save typing later
    review <- input$review
    business <- input$business
    user <- input$user
    checkin <- input$checkin
    
    review.a <- all$review
    business.a <- all$business
    user.a <- all$user
    checkin.a <- all$checkin
    
    ## Add the information from the user
    add.user <- ldply(as.character(review$user_id), processUser, user.a = user.a, 
        .parallel = parallel)
    
    ## Process the checkin info
    add.checkin <- ldply(as.character(review$business_id), processCheckin, checkin.a = checkin.a, 
        .parallel = parallel)
    
    ## Process the business info
    add.business <- ldply(as.character(review$business_id), processBusiness, 
        business.a = business.a, .parallel = parallel)
    
    ## Remove uninformative columns from the review
    colnames(review)[7:206] <- paste0("word.", colnames(review)[7:206])
    add.review <- review[, !colnames(review) %in% c("type")]
    
    ## Merge the results
    result <- cbind(add.review, add.user, add.business, add.checkin)
    
    ## Done
    return(result)
}
```


Actually process the data and save the results

```r
testReview <- processFun(testProcessed, all = all)
trainingReview <- processFun(trainingProcessed, all = all)

print(object.size(testReview), units = "Mb")
```

```
## 3.7 Mb
```

```r
print(object.size(trainingReview), units = "Mb")
```

```
## 3.7 Mb
```

```r

save(testReview, file = "testReview.Rdata")
save(trainingReview, file = "trainingReview.Rdata")
```



Reproducibility

```r
print(proc.time())
```

```
##    user  system elapsed 
##  17.411   2.539  17.836
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
## [1] doMC_1.3.0      iterators_1.0.6 foreach_1.4.0   plyr_1.8       
## [5] knitr_1.1      
## 
## loaded via a namespace (and not attached):
## [1] codetools_0.2-8 compiler_2.15.3 digest_0.6.3    evaluate_0.4.3 
## [5] formatR_0.7     stringr_0.6.2   tools_2.15.3
```


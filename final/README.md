# Final project for 140.754 by L. Collado-Torres


## Installation instructions

```S
## Only if needed
install.packages("devtools")

library(devtools)
## This is a required dependency. It's  needed to create the cool knitr bootstrap html reports.
install_github(username='rstudio', repo='markdown')

## This is the main package.
install_github("lcollado754", "lcolladotor", subdir="final/lcollado754")
```

## Reproduce the analysis in your working directory

```S
library(lcollado754)
reproduceAnalysis()
```

## Check out the results

They are stored in __/lcollado754run/__

## Example on how to use inferInterestRate()

Look at how to do so [here](http://htmlpreview.github.io/?https://github.com/lcolladotor/lcollado754/blob/master/final/lcollado754run/Infer/Infer.html).

## Read the report by running

```S
library(lcollado754)
browseVignettes("lcollado754")
```


## Enjoy!

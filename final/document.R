library(devtools)

## Create the backbone:
if(FALSE) create("lcollado754")

## Create the document fresh
document("lcollado754", clean=TRUE)

## Check the doc, pkg
check_doc("lcollado754")
check("lcollado754")

## Reproduce the analysis
library(lcollado754)
help(package=lcollado754)

## EDA
reproduceAnalysis("EDA")






dev_mode()
load_all("lcollado754")
dev_mode()

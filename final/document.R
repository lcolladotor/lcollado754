library(devtools)

## Create the backbone:
if(FALSE) create("lcollado754")

## Create the documentation fresh
document("lcollado754", clean=TRUE)

## Build the vignette
build_vignettes("lcollado754")

## Check the doc, pkg
check_doc("lcollado754")
check("lcollado754")

## Reproduce the analysis
library(lcollado754)
help(package=lcollado754)

## All of it
reproduceAnalysis("all")






dev_mode()
load_all("lcollado754")
dev_mode()

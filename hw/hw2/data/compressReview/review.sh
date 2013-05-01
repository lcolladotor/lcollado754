#!/bin/bash	
#$ -cwd 
#$ -l mem_free=50G,h_vmem=80G,h_fsize=20G
#$ -m e 
#$ -N hw2review
#$ -pe local 20

echo "**** Job starts ****"
date

# Run the test
Rscript --min-vsize=3G --min-nsize=10M -e "library(knitr); knit2html('review.Rmd')"

echo "**** Job ends ****"
date

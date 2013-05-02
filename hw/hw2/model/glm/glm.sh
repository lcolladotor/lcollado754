#!/bin/bash	
#$ -cwd 
#$ -l jabba,mem_free=50G,h_vmem=80G,h_fsize=20G
#$ -m e 
#$ -N hw2glm

echo "**** Job starts ****"
date

# Run the test
Rscript --min-vsize=3G --min-nsize=10M -e "library(knitr); knit2html('glm.Rmd')"

echo "**** Job ends ****"
date

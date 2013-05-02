#!/bin/bash	
#$ -cwd 
#$ -l jabba,mem_free=10G,h_vmem=15G,h_fsize=20G
#$ -m e 
#$ -N hw2rf
#$ -pe local 20

echo "**** Job starts ****"
date

# Run the test
Rscript --min-vsize=3G --min-nsize=10M -e "library(knitr); knit2html('rf.Rmd')"

echo "**** Job ends ****"
date

#!/bin/bash	
#$ -cwd 
#$ -l mem_free=10G,h_vmem=20G,h_fsize=10G
#$ -m e 
#$ -N hw2NLP

echo "**** Job starts ****"
date

# Run the test
Rscript --min-vsize=3G --min-nsize=10M -e "library(knitr); knit2html('nlp.Rmd')"

echo "**** Job ends ****"
date

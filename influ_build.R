##An R script for building the influ package
##This file can be run using
##	R --vanilla < influ_build.R

library(roxygen)

#Delete the existing package directory because package.skeleton does not seem to overwrite code files otherwise
system('rm -rf influ')
#Create a package skeleton
package.skeleton('influ',code_files='influ.R',force=TRUE)
#Write DESCRIPTION file. Note roxygen will overwrite some of this
cat("Package: influ
Type: Package
Title: Influence plots and metrics
Version: 0.1
Date: 2011-02-23
Author: Nokome Bentley <nbentley@trophia.com>
Maintainer: Nokome Bentley <nbentley@trophia.com>
Description: A package for generating step plots, influence plots, CDI
    plots, and influence metrics for a GLM model
License: CC BY-NC
LazyLoad: yes
Collate: 'influ.R'",file = 'influ/DESCRIPTION')
#Run roxygen ("R CMD roxygen -d influ" could be used instead)
roxygenize('influ',roxygen.dir='influ',copy.package=FALSE,unlink.target=FALSE)

##Check the package using builtin R check utility
system('R CMD check influ')

##Build package
system('R CMD build --binary influ')

##Create vignette
Sweave('influ_vignette.snw')
system('pdflatex -output-directory=docs influ_vignette.tex')

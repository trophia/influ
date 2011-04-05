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
Version: 0.2
Date: 2011-04-06
Author: Nokome Bentley <nbentley@trophia.com>
Maintainer: Nokome Bentley <nbentley@trophia.com>
Description: A package for generating step plots, influence plots, CDI
    plots, and influence metrics for a GLM model
License: Creative Commons Attribution-ShareAlike (CC BY-SA 3.0 http://creativecommons.org/licenses/by-sa/3.0/). 
	For attribution please cite Bentley, N., Kendrick, T. H., Starr, P. J., & Breen, P. A. (in preparation). Influence plots and metrics: tools for
better understanding fisheries catch per unit effort standardisations.
LazyLoad: yes
Collate: 'influ.R'",file = 'influ/DESCRIPTION')
#Run roxygen ("R CMD roxygen -d influ" could be used instead)
roxygenize('influ',roxygen.dir='influ',copy.package=FALSE,unlink.target=FALSE)

##Check the package using builtin R check utility
system('R CMD check influ')

##Build package
system('R CMD build --binary influ')
##Zip up a windows source package
system('zip -r influ_0.2.zip influ')

##Create vignette
Sweave('influ_vignette.snw')
system('pdflatex -output-directory=docs influ_vignette.tex')

#The files needed for each release are:
#	influ_0.2_R_i486-pc-linux-gnu.tar.gz
#	influ_0.2.tar
#	influ_0.2.zip
#	docs/influ_vignette.pdf
#	influ.Rcheck/influ-manual.pdf


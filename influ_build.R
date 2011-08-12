#' A script for building the "influ" package on Linux
#'
#' R --vanilla < influ_build.R
#' Copyright 2010-2011 Nokome Bentley

library(roxygen)

version = '0.4'
date = '2011-08-12'

#Delete the existing package directory because package.skeleton does not seem to overwrite code files otherwise
system('rm -rf influ')
#Create a package skeleton
package.skeleton('influ',code_files='influ.R',force=TRUE)
#Write DESCRIPTION file. Note roxygen will overwrite some of this
cat("Package: influ
Type: Package
Title: Influence plots and metrics
Version:",version,"
Date:",date,"
Author: Nokome Bentley <nbentley@trophia.com>
Maintainer: Nokome Bentley <nbentley@trophia.com>
URL: http://projects.trophia.com/influ
Description: A package for generating step plots, influence plots, CDI plots, and influence metrics for a GLM model
License: Creative Commons Attribution-ShareAlike (CC BY-SA 3.0 http://creativecommons.org/licenses/by-sa/3.0/). 
	For attribution please cite Bentley, N., Kendrick, T. H., Starr, P. J., & Breen, P. A. (in preparation). Influence plots and metrics: tools for better understanding fisheries catch per unit effort standardisations.
LazyLoad: yes
Collate: 'influ.R'
Suggests: proto
",file = 'influ/DESCRIPTION')
#Run roxygen ("R CMD roxygen -d influ" could be used instead)
roxygenize('influ',roxygen.dir='influ',copy.package=FALSE,unlink.target=FALSE)

##Check the package using builtin R check utility and copy the manual that is produced
system('R CMD check influ')
system('cp influ.Rcheck/influ-manual.pdf influ-manual.pdf')

##Create vignette
Sweave('influ_vignette.snw')
system('pdflatex influ_vignette.tex')

##Build linux source package
system('R CMD build influ')

##Build windows package by uploading to http://win-builder.r-project.org/
##It is of course to cross-build R packages or to build on Windows (http://www.biostat.wisc.edu/~kbroman/Rintro/Rwinpack.html) but both
## are quite involved, and the above is not.

#Check that the package can be installed properly
#	install.packages("influ_0.4.tar.gz",repos=NULL)
#	help(package='influ')

#The files needed for each release are:
#	influ_0.x.tar.gz
#	influ_0.x.zip
#	influ_vignette.pdf
#	influ-manual.pdf


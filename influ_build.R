# Copyright (c) 2010-2014 Nokome Bentley, Trophia Ltd
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
# 
#     * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, 
# BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT 
# SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
# OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#' A script for building the "influ" package on Linux
#'
#' R --interactive --vanilla < influ_build.R
#'

library(roxygen2)

version = '0.8'
date = '2014-04-07'

#Delete the existing package directory because package.skeleton does not seem to overwrite code files otherwise
system('rm -rf influ')

#Create a package skeleton
package.skeleton('influ',code_files='influ.R',force=TRUE)

#Write DESCRIPTION file. Note roxygen will overwrite some of this
cat("Package: influ
Title: Influence plots and metrics
Version:",version,"
Date:",date,"
Author: Nokome Bentley <nbentley@trophia.com>
Maintainer: Nokome Bentley <nbentley@trophia.com>
URL: http://projects.trophia.com/influ
Description: A package for generating step plots, influence plots, coefficient-distribution-influence plots, and influence metrics for a GLM model.
Citation: Bentley, N., Kendrick, T. H., Starr, P. J., & Breen, P. A. (2011). Influence plots and metrics: tools for better understanding fisheries catch per unit effort standardisations. ICES Journal of Marine Science, 69: 84-88.
License: BSD 2-Clause
LazyLoad: yes
Collate: 'influ.R'
Depends: proto
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

##Build windows package by uploading influ_0.x.tar.gz to http://win-builder.r-project.org/
##It is of course possible to cross-build R packages or to build on Windows (http://www.biostat.wisc.edu/~kbroman/Rintro/Rwinpack.html) but both
## are quite involved, and the above is not.

#Check that the package can be installed properly
#	install.packages("influ_0.4.tar.gz",repos=NULL)
#	help(package='influ')

#The files needed for each release are:
#	influ_0.x.tar.gz
#	influ_0.x.zip
#	influ_vignette.pdf
#	influ-manual.pdf


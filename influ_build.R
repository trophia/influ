##An R script for building the influ package
##This file can be run using
##	R --vanilla < influ_build.R

library(roxygen)

#####################################################################

#Delete the existing package directory because package.skeleton does not seem to overwrite code files otherwise
system('rm -rf influ')
#Create a package skeleton
package.skeleton('influ',code_files='influ.R',force=TRUE)
#Run roxygen ("R CMD roxygen -d influ" could be used instead)
roxygenize('influ',roxygen.dir='influ',copy.package=FALSE,unlink.target=FALSE)

#R CMD roxygen -d influ

#####################################################################
##Check the package using builtin R check utility
system('R CMD check influ')

#####################################################################

##Build PDF file ("R CMD Rd2pdf influ" did not work for me but this does)
##On Ubuntu I found it was necessary to "sudo apt-get install texlive-latex-base texlive-fonts-recommended"
system('pdflatex -output-directory=doc influ.Rcheck/influ-manual.tex')

#####################################################################

##Build package
system('R CMD build --binary influ')

##Install and load package as a further check
#install.packages('influ_1.0_R_i486-pc-linux-gnu.tar.gz',repos=NULL)
#detach(package:influ)
#library(influ)

##Bring up some help
#help(package=influ)
#?Influence

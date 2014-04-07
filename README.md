# influ

## What is it?

A package for the R language which generates step plots, influence plots, coefficient-distribution-influence (CDI) plots, and influence metrics for linear models
as described in the paper [Bentley, N., Kendrick, T. H., Starr, P. J., & Breen, P. A. (2011). Influence plots and metrics: tools for better understanding fisheries catch-per-unit-effort standardisations. ICES Journal of Marine Science, 69: 84-88. doi:10.1093/icesjms/fsr174](http://icesjms.oxfordjournals.org/cgi/reprint/fsr174?
ijkey=zZGx3RoK1zkxhaL&keytype=ref) (There is a pre-print, pre-review version [here](https://github.com/downloads/trophia/influ/Bentley%20et%20al%20%28preprint%29%20CPUE%20influence.pdf) but the above link is recommended as it gives full access to the final version)

## How do I get it?

The package is still in development. A beta version is available under [Releases](https://github.com/trophia/influ/releases/). 
Eventually influ will be submitted to the [Comprehensive R Archive Network](http://cran.r-project.org/) for direct installation via the `install.packages` R function.

## How do I use it?

We recommend looking at the `influ` [vignette](https://github.com/trophia/influ/releases/download/0.8/influ_vignette.pdf) first. But if you don't have time for even that, here's an even quicker run down....

```s
#Install the package after downloading it
install.packages("/path/to/influ_0.X.zip",repos=NULL) #Windows
install.packages("/path/to/influ_0.X.tar.gz",repos=NULL) #Linux

#Load the package
library(influ)

#Fit a glm model (the first term, in this case year, should be a factor!)
myModel = glm(log(catch)~year+effort+month+area+depth+method,data=myData)
 
##Create an influence object for that model
myInfl = Influence$new(myModel)
myInfl$calc()

##Look at the summary data.frame and execute the various methods
## to create plots
myInfl$summary
myInfl$stanPlot()
myInfl$stepPlot()
myInfl$influPlot()
myInfl$cdiPlot('month')
myInfl$cdiPlotAll()
```

There is also a [reference manual](https://github.com/downloads/trophia/influ/influ-manual.pdf) and if that fails you could always try the [source code](https://github.com/trophia/influ/blob/master/influ.R)!

## Who has contributed to development?

The initial development of this software was done by Nokome Bentley, [Trophia Ltd](http://www.trophia.com) and funded by the [New Zealand Seafood Industry Council Ltd.](http://www.seafic.co.nz)


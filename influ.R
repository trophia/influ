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

#' A package for generating step plots, influence plots, CDI plots, and influence metrics for a linear model
#'
#' The concept of influence in generalised linear models is described in 
#' Bentley, N., Kendrick, T. H., Starr, P. J., & Breen, P. A. (2011). Influence plots and metrics: tools for better understanding fisheries catch-per-unit-effort standardisations.
#' This package provides an implementation of the plots and metrics described in that paper. ICES Journal of Marine Science, doi:10.1093/icesjms/fsr174.
#'
#' Currently, this package works for \code{glm} models with log transformed dependent variables. These
#' are the type of models commonly used for one part of the delta-lognormal approach to catch-per-unit-effort (CPUE)
#' standardisation. e.g.\code{ model = glm(log(catch)~year+month+vessel+effort)}
#' In time, the package may be generalised to other types of models (e.g. \code{gam}s) and other types of dependent variables.
#'
#' This document provides minimal documentation and the best way to learn how to use this package is through the vignette:
#'  "An example of how to use the 'influ' package"
#'
#' @docType package
#' @name influ-package
#' @aliases influ
#' @title Influence in linear models 
#' @author Nokome Bentley
#' @import proto
#' @export Influence
NULL

library(proto)

#' The influence prototype object.
#'
#' @seealso Influence$new
Influence = proto()

#' Create a new Influence object.
#'
#' A new Influence object needs to be created for each combination of a model and focus term.
#' For example, you might compare the inflence of the same term in two separate models:\code{
#'    influ1 = Influence$new(model1,'year')
#'    influ2 = Influence$new(model2,'year')
#' }
#'
#' @name Influence$new
#' @param model The model for which the influence of terms will be determined
#' @param data The data which was used to fit the model (required for some types of models that do not store the data)
#' @param response The response term in the model
#' @param focus The focus term for which influence is to be calculated.
#' @return A new Influence object
Influence$new <- function(.,model,data=NULL,response=NULL,focus=NULL){
  instance = .$proto(
    model = model,
    data = data,
    response = response,
    focus = focus
  )
  instance$init()
  instance
}

#' Initialise the Influence object
#'
#' This method should not need to be called directly. It is called from the \code{$new} constructor method.
#'
#' @name Influence$init
#' @return None
Influence$init <- function(.){

  # If no data was supplied....
  if(is.null(.$data)){
    # See if the model has a data variable
    .$data = .$model$data
    if(is.null(.$data)){
      # See if it is available as a model attribute
      .$data = attr(.$model,"data")
    }
    if(is.null(.$data)){
      stop("You need to provide the data that was fitted to for this type of model e. Influence$new(model,data,...)")
    }
  }

  .$terms = attr(.$model$terms,"term.labels")
  if(is.null(.$response)) .$response = names(.$model$model)[1]
  if(is.null(.$focus)) .$focus = .$terms[1]
  
  .$labels = list()
  for(term in .$terms) .$labels[term] = term

  .$orders = list()
  for(term in .$terms) .$orders[term] = 'asis'

  #An alternative, but potentially buggy, way of getting response term
  #Obtain the response name. This seems the safest way, albeit long winded.
  #variablesList = attr(model$terms,"variables") #List of variables including the response
  #responseIndex = attr(model$terms,"response") #Index of the response variable in variablesList 
  #responseName = as.character(variablesList)[responseIndex+1]
}

#' Obtain the coefficients associated with a particular term in the model
#'
#' Extracts the coefficients
#'
#' @name coeffs
#' @param model The model to extract coefficients from
#' @param term The term in the model for which coefficients are extracted
#' @return A vector of coefficients                                                                     
coeffs = function(.,model=.$model,term=.$focus){
  coeffs = coefficients(model)
  rows = substr(names(coeffs),1,nchar(term))==term
  c(0,coeffs[rows])
}

#' Obtain standard errors for the coefficients associated with a particular term in the model
#'
#' Extract standard errors using the method of Francis
#'
#' @name ses
#' @param model The model to extract standard errors for a coefficient from
#' @param term The term in the model for which coefficients SEs are extracted
ses = function(.,model=.$model,term=.$focus){
  type = class(model)[1]
  if(type=='glm'|type=='negbin'){
    #The "cov.scaled" member of a glm object is the estimated covariance matrix of 
    #the estimated coefficients scaled by dispersion"    
    V = summary(model)$cov.scaled
  }
  else if(type=='survreg'){
    #The member "var" for a survreg object is the "the variance-covariance matrix for the parameters, 
    #including the log(scale) parameter(s)" 
    V = model$var
  }
  
  rows = substr(row.names(V),1,nchar(term))==term
  V = V[rows,rows]
  n = sum(rows)+1
  Q = matrix(-1/n, nrow=n, ncol=n-1)
  Q[-1,] = Q[-1,] + diag(rep(1,n-1))
  V0 = (Q%*%V) %*% t(Q)
  se = sqrt(diag(V0))
  se
}

#' Obtain the effects associated with a particular term in the model
#'
#' @name Influence$effects
#' @param model The model to extract effects from
#' @param term The term in the model for which effects are extracted
#' @return A numeric vector of effects for the model term
Influence$effects = function(.,model=.$model,term=.$focus){
  coeffs = .$coeffs(model,term)
  exp(coeffs-mean(coeffs))
}

#' Perform necessary calculations
#'
#' This method must be called for the \code{$summary} data frame to be available or before any plots (\code{$stanPlot(),$stepPlot(),$influPlot(),$cdiPlot()}) can be generated from the Influence object
#'
#' @name Influence$calc
#' @return None
Influence$calc <- function(.){
  #Get observed values
  observed = .$model$model[,.$response]
  if(class(observed)[1]=='Surv') observed = as.numeric(observed[,1])
  logged = substr(.$response,1,4)=='log('
  #Create a data frame that is used to store various values for each level of focal term
  .$indices = data.frame(level=levels(.$model$model[,.$focus]))
  #Add an unstandardised index
  if(logged | sum(observed<=0)==0){
    if(logged) log_observed = observed else log_observed = log(observed)
    # Calculate geometic mean
    .$indices = merge(.$indices,aggregate(list(unstan=log_observed),list(level=.$model$model[,.$focus]),mean))
    # Turn into a relative index
    .$indices$unstan = with(.$indices,exp(unstan-mean(unstan)))
  }else {
     # There are zeros (or even negative numbers) in the data so we cant calculate a geometric mean.
     # Use arithmetic mean instead
    .$indices = merge(.$indices,aggregate(list(unstan=observed),list(level=.$model$model[,.$focus]),mean))
     # Turn into a relative index
    .$indices$unstan = with(.$indices,unstan/mean(unstan))
  }
  #Add standardised index.
  coeffs = .$coeffs()
  ses = .$ses()
  base = mean(coeffs)
  .$indices$stan = exp(coeffs-base)
  .$indices$stanLower = exp(coeffs-base-2*ses)
  .$indices$stanUpper = exp(coeffs-base+2*ses)
  
  #Create models with terms succesively added
  .$summary = NULL
  
  #TODO calculate influence statistics in this loop too
  for(termCount in 0:length(.$terms)){
    if(termCount>0){
      term = .$terms[termCount]
      #Update both formula and model
      newFormula = formula(.$model)
      newFormula = update.formula(newFormula,formula(paste("~",paste(paste(.$terms[1:termCount],collapse='+')))))
      model = update(.$model,newFormula)
      #Get index for this model
      index = .$effects(model)
      #Add column to .$indices
      .$indices = cbind(.$indices,index)
      #Give index column the right hand side of formula as name
      names(.$indices)[ncol(.$indices)] = if(termCount==1) term else paste('+',term)
    } else {
      term = 'intercept'
      model = update(.$model,.~1)
    }
    
    type = class(model)[1]
    logLike =  switch(type,
        survreg = model$loglik[2],
        logLik(model)
    )
    fitted = switch(type,
        survreg = predict(model,type='response'),
        fitted(model)
    )
    
    #Sums-of-squared based R2 based on log(observed) and log(fitted)
    if(termCount==0) r2 = 0 else r2 = cor(log(observed),log(fitted))^2
    
    #Deviance pseudo-R2
    r2Dev = (model$null.deviance-model$deviance)/model$null.deviance
    if(length(r2Dev)==0) r2Dev = NA
    
    #Negelkerke pseudo-R2
    if(termCount==0) logLikeInterceptOnly = logLike
    n = length(observed)
    r2Negel = (1-exp((logLikeInterceptOnly-logLike)*(2/n)))/(1-exp(logLikeInterceptOnly*(2/n)))
    
    .$summary = rbind(.$summary,data.frame(
      term = term,
      k = length(coef(model)),
      logLike = logLike,
      aic = extractAIC(model)[2],
      r2 = r2,
      r2Dev = r2Dev,
      r2Negel = r2Negel
    ))
  }
  #R2 values presented as differences
  .$summary = within(.$summary,{
    k = c(1,diff(k))
    r2 = c(NA,diff(r2))
    r2Dev = c(NA,diff(r2Dev))
    r2Negel = c(NA,diff(r2Negel))
  })
  
  #Calculate predicted values
  preds = predict(.$model,type='terms',se.fit=T)
  fit = as.data.frame(preds$fit)
  se.fit = as.data.frame(preds$se.fit)
  preds = cbind(fit,se.fit)
  names(preds) = c(paste('fit',names(fit),sep='.'),paste('se.fit',names(fit),sep='.'))
  .$preds = cbind(.$data,preds)
  #Calculate influences and statisitcs
  .$influences = data.frame(level=levels(.$model$model[,.$focus]))
  overall = c(NA,NA) # NAs for null model and for focus term
  trend = c(NA,NA)
  for(term in .$terms){
      if(term!=.$focus){
        infl = aggregate(
          list(value = .$preds[,paste('fit',term,sep='.')]),
          list(level = .$preds[,.$focus]),
          mean
        )
        overall = c(overall,with(infl,exp(mean(abs(value)))-1))
        trend = c(trend,with(infl,exp(cov(1:length(value),value)/var(1:length(value)))-1))
        names(infl) = c('level',term)
        .$influences = merge(.$influences,infl,all.x=T,by='level')
      }
  }
  #Insert statistics into summary table with NAs for NULL and focus terms
  .$summary$overall = overall
  .$summary$trend = trend
}

#' Standardization plot
#'
#' This plot simply compares the standardised and unstandardised indices from a model so that the overall standardisation effect can be ascertained.
#'
#' @name Influence$stanPlot
#' @return None
Influence$stanPlot <- function(.){
  with(.$indices,{
    plot(NA,ylim=c(0,max(unstan,stanUpper)),xlim=c(1,length(level)),las=1,ylab='Index',xlab=.$labels[[.$focus]],xaxt='n')
    lines(as.integer(level),unstan,type='o',pch=1,cex=1.25)
    lines(as.integer(level),stan,type='o',pch=16,cex=1.25)
    segments(as.integer(level),stanLower,as.integer(level),stanUpper)
    axis(side=1,at=1:length(levels(level)),labels=levels(level))
    legend('top',legend=c('Unstandardised','Standardised'),pch=c(1,16),pt.cex=1.25,bty='n')
  })
}

#' Step plot
#'
#' A plot of the standardised indices as each explanatory variable is added to the model (in the order that they are specified in the model equation)
#'
#' @name Influence$stepPlot
#' @param panels Whether or not to produce a "progressive" step plot (default) in which each step is represented by a separate 
#'  panel with the previous steps shown as a dashed line and other steps shown as dashed grey lines. If FALSE then a single panel
#'  plot is produced with symbols for each step
#' @return None
Influence$stepPlot <- function(.,panels=T,setpar=T){
  startCol = 6
  cols = startCol:ncol(.$indices)
  if(panels){
    if(setpar)par(mfrow=c(length(cols),1),mar=c(0,4,0,0),oma=c(4,1,1,1))
    levels = as.integer(.$indices$level)
    xlim=c(1,length(levels))
    ylim=c(0,max(.$indices[,cols]))
    for(col in cols){
      plot(NA,xaxt='n',las=1,ylab='Index',ylim=ylim,xlim=xlim)
      #abline(h=1,lty=2)
      for(prev in cols[1]:col){
        if(prev<col-1) lines(levels,.$indices[,prev],col='grey',lwd=1.5)
        if(prev==col-1) lines(levels,.$indices[,prev],lty=3,col='black',lwd=1.5)
        if(prev==col) points(levels,.$indices[,prev],pch=16,cex=1.25,col='black',type='o')
      }
      legend('topleft',legend=names(.$indices)[col],bty='n')
    }
    axis(side=1,at=1:length(.$indices$level),labels=.$indices$level)
  }
  else {
    with(.$indices,{
      plot(NA,ylim=c(0,max(.$indices[,cols])),xlim=c(1,nrow(.$indices)),las=1,ylab='Index',xlab=.$labels[[.$focus]],xaxt='n')
      for(col in cols) lines(as.integer(level),.$indices[,col],type='o',pch=col-startCol+1,cex=1.25)
      axis(side=1,at=1:length(level),labels=level)
      legend('top',legend=names(.$indices)[cols],pch=cols-startCol+1,pt.cex=1.25,bty='n')
    })
  }
}

#' Influence plot
#'
#' A plot of the influence of each explanatory variable in the model
#'
#' @name Influence$influPlot
#' @param panels Whether or not to produce a plot with separate panels for each variable
#' @return None
Influence$influPlot <- function(.,panels=T,setpar=T){
  cols = 2:ncol(.$influences)
  ylim=exp(range(.$influences[,cols]))
  xlim=c(1,nrow(.$influences))
  if(panels){
    if(setpar) par(mfrow=c(length(cols),1),mar=c(0,4,0,0),oma=c(4,1,1,1))
    levels = as.integer(.$influences$level)
    for(col in cols){
      plot(levels,exp(.$influences[,col]),pch=16,cex=1.25,col='black',type='o',xaxt='n',las=1,ylab='Influence',ylim=ylim,xlim=xlim)
      abline(h=1,lty=2)
      legend('topleft',legend=names(.$influences)[col],bty='n')
    }
    axis(side=1,at=1:length(.$influences$level),labels=.$influences$level)
  }
  else{
    with(.$influences,{
      plot(NA,ylim=ylim,xlim=xlim,las=1,ylab='Influence',xlab=.$labels[[.$focus]],xaxt='n')
      for(col in cols) lines(as.integer(level),exp(.$influences[,col]),type='o',pch=col,cex=1.25)
      axis(side=1,at=1:length(level),labels=level)
      legend('top',legend=names(.$influences)[cols],pch=cols,pt.cex=1.25,bty='n')
      abline(h=1,lty=2)
    })
  }
}

#' Step and influence plots
#'
#' Side by side, panellised step and influence plots
#'
#' @name Influence$stepAndInfluPlot
#' @return None
Influence$stepAndInfluPlot <- function(.){
  par(mfcol=c(ncol(.$indices)-5,2),mar=c(0,5,0,0),oma=c(4,1,1,1))
  .$stepPlot(setpar=F)
  #Create a blank plot in the top of the influences column for the row for the focus term.
  plot.new()
  .$influPlot(setpar=F)
}

#' A coefficient-distribution-influence (CDI) plot for a model term
#'
#' @name Influence$cdiPlot
#' @param term The model term for which the CDI plot should be generated
#' @param variable Optional. In some cases it may be necessary to specifiy the variable that is related to the term. For example, for the term \code{"log(effort)"} the variable is \code{"effort"}. 
#' In most cases this can be deduced from the term, but for complexes terms this argumey may also need to be supplied (an error message will tell you if it does).
#' @return None
#' @seealso cdiPlotAll
Influence$cdiPlot <- function(.,term,variable=NULL){
  par(oma=c(1,1,1,1),cex.lab=1.25,cex.axis=1.25)
  layout(matrix(c(1,1,0,2,2,3,2,2,3),3,3,byrow=TRUE))
  
  #Define levels of term on which coefficient and distribution plots will be based
  #This is done by search for each column name in the data as a whole word in the
  #each term. This allows for matching of terms like 'poly(log(var),3)' with 'var'
  if(is.null(variable)){
    for(name in names(.$preds)){
      match = grep(paste('([^[:alnum:]_])+',name,'([^[:alnum:]_])+',sep=''),paste('(',term,')')) # ([^\\w])+ Means ignore any 'word' character (alphanumerics plus underscore)
      if(length(match)>0){
        variable = name
        break
      }
    }
  }
  if(is.null(variable)) stop('Unable to find a matching variable for term "',term,'". Please specify in the argument "variable".')
  levels = .$preds[,variable]

  #Numeric terms are cut into factors
  if(is.numeric(levels)){
    breaks = pretty(levels,30)
    step = breaks[2]-breaks[1]
    labels = breaks+step/2
    breaks = c(breaks,breaks[length(breaks)]+step)
    levels = cut(levels,breaks,labels=labels,include.lowest=T)
  }

  #Reorder levels according to coefficients if necessary
  if(.$orders[[term]]=='coef'){
    coeffs = aggregate(.$preds[,paste('fit',term,sep='.')],list(levels),mean)
    names(coeffs) = c('term','coeff')
    coeffs = coeffs[order(coeffs$coeff),]
    levels = factor(levels,levels=coeffs$term,ordered=T)
  }

  #Coefficients
  coeffs = aggregate(.$preds[,paste(c('fit','se.fit'),term,sep='.')],list(levels),mean)
  names(coeffs) = c('term','coeff','se')
  coeffs = within(coeffs,{
    lower = coeff-se
    upper = coeff+se
  })

  par(mar=c(0,5,3,0),las=1)
  with(coeffs,{
    xs = 1:max(as.integer(term))
    ylim = c(min(exp(lower)),max(exp(upper)))
    if(ylim[1]<0.5*min(exp(coeff))) ylim[1] = 0.5*min(exp(coeff))
    if(ylim[2]>2*max(exp(coeff))) ylim[2] = 2*max(exp(coeff))
    plot(as.integer(term),exp(coeff),xlim=range(xs),ylim=ylim,pch=2,cex=1.5,xaxt='n',ylab='',log='y')
    mtext('Coefficient',side=2,line=4,las=0,cex=0.8)
    abline(h=1,lty=2)
    abline(v=xs,lty=1,col='grey')
    segments(as.integer(term),exp(lower),as.integer(term),exp(upper))
    arrows(as.integer(term),exp(lower),as.integer(term),exp(upper),angle=90,code=3,length=0.05)
    axis(3,at=xs,labels=levels(term)[xs])
  })

  #Distribution
  distrs = aggregate(.$preds[,1],list(levels,.$preds[,.$focus]),length)
  names(distrs) = c('term','focus','count')
  distrs = merge(distrs,aggregate(list(total=distrs$count),list(focus=distrs$focus),sum))
  distrs$prop = with(distrs,count/total)
  par(mar=c(5,5,0,0),las=1)
  xlab = .$labels[[variable]]
  if(is.null(xlab)) xlab = variable
  ylab= .$labels[[.$focus]]
  if(is.null(ylab)) ylab = .$focus
  with(distrs,{
    xs = 1:max(as.integer(term))
    ys = 1:max(as.integer(focus))
    plot(NA,xlim=range(xs),ylim=range(ys),xaxt='n',yaxt='n',xlab=xlab,ylab='')
    abline(v=xs,lty=1,col='grey')
    axis(1,at=xs,labels=levels(term)[xs])
    abline(h=ys,lty=1,col='grey')
    axis(2,at=ys,labels=levels(focus)[ys])
    mtext(ylab,side=2,line=4,las=0,cex=0.8)
    points(as.integer(term),as.integer(focus),cex=sqrt(prop)*12)
  })

  #Influence
  par(mar=c(5,0,0,3),las=1)
  ys = 1:nrow(.$influences)
  with(.$influences,{
    plot(NA,xlim=range(exp(get(term))),ylim=range(ys),yaxt='n',xlab='Influence')
    abline(h=ys,lty=1,col='grey')
    abline(v=1,lty=2)
    points(exp(get(term)),ys,type='o',pch=16,cex=1.8)
    axis(4,at=ys,labels=level)
  })
  
}

#' Plot a CDI plot for each of the terms in the model
#'
#' @name Influence$cdiPlotAll
#' @param done A function that is called (with the term string as an argument) after each CDI plot is generated. Optional.
#' @return None
Influence$cdiPlotAll <- function(.,done=function(term){
  cat("cdiPlot for",term,". Press enter for next")
  scan()
}){
  for(term in .$terms) {
    if(term!=.$focus){
      .$cdiPlot(term)
      done(term)
    }
  }
}


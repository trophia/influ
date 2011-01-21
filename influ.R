#' Influence in linear models
#'
#' @name influ-package
#' @aliases influ
#' @docType package
#' @title Influence in linear models 
#' @author Nokome Bentley
#' @examples
#' #myModel = glm(catch~year+month)
NULL

library(proto)

#' The influence 
#'
#' Details go here
Influence = proto()

#' Create a new Influence object
#'
#' Details go here
#'
#' @name new
#' @method Influence new
#' @param model The model which will bea
#' @param response The response var
#' @return A new Influence object
Influence$new <- function(.,model,response,focus=NULL){
  instance = proto()
  instance$init(model,response,focus)
  instance
}

#' Initialise
#' @name init
Influence$init <- function(.,model,response,focus){
  .$model = model
  .$response = response
  .$focus = if(is.null(focus)) attr(.$model$terms,"term.labels")[1] else focus
  .$terms = attr(.$model$terms,"term.labels")
  
  .$labels = list()
  for(term in .$terms) .$labels[term] = paste(toupper(substring(term,1,1)), substring(term,2),sep="")
  .$order = list()
  for(term in .$terms) .$order[term] = 'asis'

  #Obtain the response name. This seems the safest way, albeit long winded.
  #variablesList = attr(model$terms,"variables") #List of variables including the response
  #responseIndex = attr(model$terms,"response") #Index of the response variable in variablesList 
  #responseName = as.character(variablesList)[responseIndex+1]
}

#' Coefficients
#' @name coeffs
Influence$coeffs = function(.,model=.$model,term=.$focus){
  coeffs = summary(model)$coeff
  rows = substr(row.names(coeffs),1,nchar(term))==term
  c(0,coeffs[rows,1])
}

#' Standard errors
#' @name ses
Influence$ses = function(.,model=.$model,term=.$focus){
  summ = summary(model)
  V = summ$cov.scaled
  row = substr(row.names(V),1,nchar(term))==term
  V = V[row,row]
  n = sum(row)+1
  Q = matrix(-1/n, nrow=n, ncol=n-1)
  Q[-1,] = Q[-1,] + diag(rep(1,n-1))
  V0 = (Q%*%V) %*% t(Q)
  se = sqrt(diag(V0))
  se
}

#' Effects
#' @name effects
Influence$effects = function(.,model=.$model,term=.$focus){
  coeffs = .$coeffs(model,term)
  exp(coeffs-mean(coeffs))
}

#' Calculate
#' @name calc
Influence$calc <- function(.){

  #Create a summary table that is an augmented ANOVA table
  #TODO to speed up mke this part of term loop
  aov = as.data.frame(anova(.$model))
  .$summary = data.frame(
    term = row.names(aov),
    df = aov$Df,
    dev = aov$Deviance,
    devProp = aov$Deviance/aov[1,'Resid. Dev'],
    aic = NA
  )

  #Create a data frame that is used to store various values for each level of focal term
  .$indices = data.frame(level=levels(.$model$data[,.$focus]))

  #Add an unstandardised index. Currently assumes lognormal and that all response values are >0
  .$indices = merge(.$indices,aggregate(list(unstan=.$model$data[,.$response]),list(level=.$model$data[,.$focus]),function(x)exp(mean(log(x)))))
  .$indices$unstan = with(.$indices,exp(log(unstan)-mean(log(unstan))))

  #Add standardised index. This is done again below when all terms are added but useful to have this name and also have SEs
  coeffs = .$coeffs()
  ses = .$ses()
  base = mean(coeffs)
  .$indices$stan = exp(coeffs-base)
  .$indices$stanLower = exp(coeffs-2*ses-base)
  .$indices$stanUpper = exp(coeffs+2*ses-base)

  #Create models with terms succesively added
  #TODO calculate influence statistics in this loop too
  for(termCount in 1:length(.$terms)){
    term = .$terms[termCount]
    #Update both formula and model
    formula = update(.$model$formula,formula(paste("~",paste(paste(.$terms[1:termCount],collapse='+')))))
    termsModel = update(.$model,formula)
    #Get index for this model
    index = .$effects(termsModel)
    #Add column to .$indices
    .$indices = cbind(.$indices,index)
    #Give index column the right hand side of formula as name
    names(.$indices)[ncol(.$indices)] = if(termCount==1) term else paste('+',term)
    #Calculate AIC
    .$summary$aic[.$summary$term==term] = AIC(termsModel)
  }

  .$preds = data.frame(cbind(.$model$data[,.$terms],predict(.$model,type='terms',se.fit=T)))

  #Calculate influences and statisitcs
  #.$terms[-1] so that .$focus is not included
  .$influences = data.frame(level=levels(.$model$data[,.$focus]))
  overall = c(NA,NA) # NAs for NULL and focus terms
  trend = c(NA,NA)
  for(term in .$terms[-1]){
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
  #Insert statistics into summary table with NAs for NULL and focus terms
  .$summary$overall = overall
  .$summary$trend = trend

}

#' Plot of standardization effect of model
#' Standardized versus unstandardised coefficients
#' @name stanPlot
Influence$stanPlot <- function(.){
  with(.$indices,{
    plot(NA,ylim=c(0,max(unstan,stanUpper)),xlim=c(1,length(level)),las=1,ylab='Index',xlab=.$labels[[.$focus]],xaxt='n')
    lines(as.integer(level),unstan,type='o',pch=1,cex=1.25)
    lines(as.integer(level),stan,type='o',pch=16,cex=1.25)
    segments(as.integer(level),stanLower,as.integer(level),stanUpper)
    axis(side=1,at=1:length(levels(level)),labels=levels(level))
    legend('top',legend=c('Unstandardied','Standardised'),pch=c(1,16),pt.cex=1.25,bty='n')
  })
}

#' Index Plot
#' @name indiPlot
Influence$indiPlot <- function(.){
  startCol = 6
  cols = startCol:ncol(.$indices)
  with(.$indices,{
    plot(NA,ylim=c(0,max(.$indices[,cols])),xlim=c(1,nrow(.$indices)),las=1,ylab='Index',xlab=.$labels[[.$focus]],xaxt='n')
    for(col in cols) lines(as.integer(level),.$indices[,col],type='o',pch=col-startCol+1,cex=1.25)
    axis(side=1,at=1:length(level),labels=level)
    legend('top',legend=names(.$indices)[cols],pch=cols-startCol+1,pt.cex=1.25,bty='n')
  })
}

#' Influence plot
#' @name influPlot
Influence$influPlot <- function(.){
  cols = 2:ncol(.$influences)
  with(.$influences,{
    plot(NA,ylim=c(0,max(exp(.$influences[,cols]))),xlim=c(1,nrow(.$influences)),las=1,ylab='Influence',xlab=.$labels[[.$focus]],xaxt='n')
    for(col in cols) lines(as.integer(level),exp(.$influences[,col]),type='o',pch=col,cex=1.25)
    axis(side=1,at=1:length(level),labels=level)
    legend('top',legend=names(.$influences)[cols],pch=cols,pt.cex=1.25,bty='n')
    abline(h=1,lty=2)
  })
}

#' Generate a coefficient-distribution-influence (CDI) plot for a model term
#' @name cdiPlot
#' @examples
#' #inf$cdiPlot("month")
#' @seealso cdiPlotAll
Influence$cdiPlot <- function(.,term){
  par(oma=c(1,1,1,1),cex.lab=1.25,cex.axis=1.25)
  layout(matrix(c(1,1,0,2,2,3,2,2,3),3,3,byrow=TRUE))
  
  #Define levels of term on which coefficient and distribution plots will be based
  levels = .$preds[,term]
  #Numeric terms are cut into factors
  if(is.numeric(levels)){
    breaks = pretty(levels,30)
    step = breaks[2]-breaks[1]
    labels = breaks+step/2
    breaks = c(breaks,breaks[length(breaks)]+step)
    levels = cut(levels,breaks,labels=labels,include.lowest=T)
  }

  #Reorder levels according to coefficients if necessary
  if(.$order[[term]]=='coef'){
    coeffs = aggregate(.$preds[,paste('fit',term,sep='.')],list(levels),head,1)
    names(coeffs) = c('term','coeff')
    coeffs = coeffs[order(coeffs$coeff),]
    levels = factor(levels,levels=coeffs$term,ordered=T)
  }

  #Coefficients
  coeffs = aggregate(.$preds[,paste(c('fit','se.fit'),term,sep='.')],list(levels),head,1)
  names(coeffs) = c('term','coeff','se')
  coeffs = within(coeffs,{
    lower = coeff-2*se
    upper = coeff+2*se
  })

  par(mar=c(0,5,3,0),las=1)
  with(coeffs,{
    xs = 1:max(as.integer(term))
    plot(as.integer(term),exp(coeff),xlim=range(xs),ylim=c(0,max(exp(upper))),pch=2,cex=1.5,xaxt='n',ylab='Coefficient')
    abline(h=1,lty=2)
    abline(v=xs,lty=1,col='grey')
    segments(as.integer(term),exp(lower),as.integer(term),exp(upper))
    axis(3,at=xs,labels=levels(term)[xs])
  })

  #Distribution
  distrs = aggregate(.$preds[,term],list(levels,.$preds[,.$focus]),length)
  names(distrs) = c('term','focus','count')
  distrs = merge(distrs,aggregate(list(total=distrs$count),list(focus=distrs$focus),sum))
  distrs$prop = with(distrs,count/total)
  par(mar=c(5,5,0,0),las=1)
  xlab = .$labels[[term]]
  with(distrs,{
    xs = 1:max(as.integer(term))
    ys = 1:max(as.integer(focus))
    plot(NA,xlim=range(xs),ylim=range(ys),xaxt='n',yaxt='n',xlab=xlab,ylab=.$labels[[.$focus]])
    abline(v=xs,lty=1,col='grey')
    axis(1,at=xs,labels=levels(term)[xs])
    abline(h=ys,lty=1,col='grey')
    axis(2,at=ys,labels=levels(focus)[ys])
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

#' Plot all cdis
#' @name cdiPlotAll
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


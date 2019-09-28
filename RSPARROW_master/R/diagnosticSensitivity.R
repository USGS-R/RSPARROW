#'@title diagnosticSensitivity
#'@description Calculates the parameter sensitivities (change in load predictions for a a 1% 
#'            unit change in the explanatory variables). Outputs plots to 
#'            ~/estimate/(run_id)_diagnostic_sensitivity.pdf. Outputs `sensitivities.list` as binary file to ~/estimate/(run_id)_sensitivities.list. \\cr \\cr
#'Executed By: estimate.R \\cr
#'Executes Routines: \\itemize\{\\item named.list.R
#'             \\item predictSensitivity.R
#'             \\item unPackList.R\} \\cr
#'@param classvar character vector of user specified spatially contiguous discrete 
#'       classification variables from sparrow_control.  First element is reach classification variable.
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'       parmMin>=0) | parmType!="SOURCE")`
#'@param reach_decay_specification the SAS IML reach decay function code from sparrow_control
#'@param reservoir_decay_specification the SAS IML reservoir decay function code from 
#'       sparrow_control
#'@param subdata data.frame input data (subdata)



diagnosticSensitivity <- function(file.output.list,classvar,estimate.list,DataMatrix.list,SelParmValues,
                                  reach_decay_specification,reservoir_decay_specification,
                                  subdata,sitedata.demtarea.class) {
  
  
  ####################################################################
  # create global variables
  unPackList(lists = list(SelParmValues = SelParmValues,
                          JacobResults = estimate.list$JacobResults,
                          file.output.list = file.output.list),
             parentObj = list(NA,
                              NA,
                              NA))
  
  # contiguous class variables by sites
  class <- array(0,dim=c(nrow=nrow(sitedata),ncol=length(classvar))) 
  for (k in 1:length(classvar)) { 
    for (i in 1:nrow(sitedata)) {
      class[i,k] <- as.numeric(eval(parse(text=paste("sitedata$",classvar[k],"[",i,"]",sep=""))))
    } 
  }
  depvar <- subdata$depvar
  xclass <- eval(parse(text=paste("subdata$",classvar[1],sep="")))
  ####################################################################
  
  filename <- paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_diagnostic_sensitivity.pdf",sep="")
  pdf(file=filename)
  
  
  # required SPARROW estimated coefficients (oEstimate, Parmnames)
  Estimate <- oEstimate  # initial baseline estimates
  
  # obtain baseline predictions all reaches
  predict <- predictSensitivity(Estimate,estimate.list,DataMatrix.list,SelParmValues,
                                reach_decay_specification,reservoir_decay_specification,subdata)
  
  apredict <- predict
  apredict_sum <- matrix(1,nrow=length(depvar),ncol=length(Estimate))
  
  ct <- length(Estimate)
  xiqr <- matrix(0,nrow=4,ncol=sum(ct))
  xmed <- numeric(sum(ct))
  xparm <- character(sum(ct))
  xvalue2 <- numeric(sum(ct))
  xsens <- matrix(0,nrow=sum(depvar > 0),ncol=length(Estimate))
  
  for (i in 1:length(Estimate)) {
    if(betaconstant[i] == 0) {     # an estimated parameter
      #  adjust parameter by 1%
      AEstimate <- Estimate
      AEstimate[i] <- Estimate[i] * 0.99
      apredict <- predictSensitivity(AEstimate,estimate.list,DataMatrix.list,SelParmValues,
                                     reach_decay_specification,reservoir_decay_specification,subdata)
      apredict_sum[,i] <- abs((apredict-predict)/predict*100) / 1.0  # change relative to 1% change
    }
  }
  
  # select site sensitivities 
  #   (only plot for site locations rather than nreach to reduce size of display)
  j<-0
  par(mfrow=c(2,2), pch=16)  # 4 plots on one page
  for (i in 1:length(Estimate)) {
    x1 <- apredict_sum[,i]
    xx <- data.frame(x1,depvar,xclass)
    parmsens <- xx[(xx$depvar > 0), ] 
    boxplot(parmsens$x1 ~ parmsens$xclass,xlab=classvar[1],ylab="Prediction Change (%) Relative to 1% Change",las = 2)
    title(bquote(paste("Parameter Sensitivity:  ",.(Parmnames[i]))))  
    xvalue2[i] <- i
    xiqr[,i] <- quantile(parmsens$x1, c(0.10,0.25,0.75,0.90)) 
    xmed[i] <- median(parmsens$x1)
    xparm[i] <- Parmnames[i]
    xsens[,i] <- parmsens$x1   # sensitivites for all calibration sites
  }
  
  # save results to directory and global environment
  sensitivities.list <- named.list(xparm,xmed,xiqr,xsens)
  objfile <- paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_sensitivities.list",sep="") 
  save(sensitivities.list,file=objfile)
  assign("sensitivities.list",sensitivities.list,envir = .GlobalEnv)
  
  # Plot median and IQR for each parameter
  xx <- xiqr[1,(xiqr[1,]>0)]  
  xminimum <- min(xx)
  xminimum <- ifelse(is.infinite(xminimum),0,xminimum)
  xmed <- ifelse( xmed == 0,xminimum,xmed)
  xiqr <- ifelse( xiqr == 0,xminimum,xiqr)
  
  xupper <- xiqr[3,] - xmed
  xlower <- xmed - xiqr[2,]
  supper <- xiqr[4,] - xmed
  slower <- xmed - xiqr[1,]
  
  xupper <- ifelse(xupper == 0,xminimum,xupper)
  supper <- ifelse(supper == 0,xminimum,supper)
  xlower <- ifelse(xlower == 0,xminimum,xlower)
  slower <- ifelse(slower == 0,xminimum,slower)
  
  xx <- data.frame(xmed,xlower,xupper,supper,slower,xparm)
  xx <- xx[with(xx,order(xx$xmed)), ]  
  
  ymin <- min(xiqr)
  ymax <- max(xiqr)
  
  # Arithmetic y axis
  par(mfrow=c(1,1), pch=16)    # 1 plots on one page
  plotCI(x=xvalue2, y=xx$xmed, uiw=xx$supper,liw=xx$slower,ylim=c(ymin,ymax),
         col="black",barcol="blue",pch=19,
         ylab="CHANGE IN PREDICTED VALUES (%)",xlab=" ",
         gap=0,
         las = 2,     # axis labels vertical
         xaxt = "n",   # Dont print x-axis
         xlim=c(1,length(Estimate))
  )
  axis(side=1, at=1:(length(Estimate)), labels=xx$xparm, cex.axis=0.8, las = 2)
  title("PARAMETER SENSITIVITY TO 1% CHANGE")
  
  plotCI(x=xvalue2, y=xx$xmed, uiw=xx$xupper,liw=xx$xlower,ylim=c(ymin,ymax),
         col="black",barcol="red",pch=19,
         gap=0,
         xaxt = "n",   # Dont print x-axis
         add=TRUE
  )
  
  # Log y axis
  par(mfrow=c(1,1), pch=16)    # 1 plots on one page
  plotCI(x=xvalue2, y=xx$xmed, uiw=xx$supper,liw=xx$slower,ylim=c(ymin,ymax),
         col="black",barcol="blue",pch=19,log="y",
         ylab="CHANGE IN PREDICTED VALUES (%)",xlab=" ",
         gap=0,
         las = 2,     # axis labels vertical
         xaxt = "n",   # Dont print x-axis
         xlim=c(1,length(Estimate))
  )
  axis(side=1, at=1:(length(Estimate)), labels=xx$xparm, cex.axis=0.8, las = 2)
  title("PARAMETER SENSITIVITY TO 1% CHANGE")
  
  plotCI(x=xvalue2, y=xx$xmed, uiw=xx$xupper,liw=xx$xlower,ylim=c(ymin,ymax),
         col="black",barcol="red",pch=19,log="y",
         gap=0,
         xaxt = "n",   # Dont print x-axis
         add=TRUE
  )
  
  dev.off()  # shuts down current graphics device
  graphics.off()  # shuts down all open graphics devices
  
  
}#end function


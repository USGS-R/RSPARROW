#'@title estimateWeightedErrors
#'@description  \\cr \\cr
#'Executed By:  \\cr
#'Executes Routines: \\itemize\{\\item named.list.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input 
#'                        and output of external files.  Created by `generateInputList.R`
#'@param xrun_id current model run_id used by `estimateWeightedErrors.R` executed in 
#'       `userModifyData.R`
#'@param pre_run_id run_id for the prior model residuals used by `estimateWeightedErrors.R` 
#'       executed in `userModifyData.R`
#'@param nreaches number of reaches
#'@param calsites calibration site indicator (NA or 0 = not selected; 1=selected)
#'@return `weight` numeric vector of weights computed as the reciprocal of the 
#'                 squared residutals (variance) predicted by the power function


estimateWeightedErrors <- function(file.output.list,xrun_id,pre_run_id,nreaches,calsites) {
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  # predicted weights developed per method in equation 1.50 (Schwarz et al. 2006) 
  #  using nonlinear power function regression of squared log residuals on log of predicted loads
  
  # read the _residuals.csv file contents from a prior model run (sites in downstream order)
  filename <- paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,pre_run_id,.Platform$file.sep,
                    "estimate",.Platform$file.sep,pre_run_id,"_residuals.csv")
  
  oResids <- read.csv(filename,header=TRUE,                                    # colClasses=Ctype,
                      dec = csv_decimalSeparator,sep=csv_columnSeparator)
  
  # linear regression weights
  y <- oResids$Resids^2
  x <- log(oResids$predict)
  reg <- lm(y ~ x)
  cmin <- min(reg$fitted.values[reg$fitted.values>0])
  reg$fitted.values <- ifelse(reg$fitted.values>0,reg$fitted.values,cmin)
  weights_lr <- 1/reg$fitted.values     # returned reciprocal variance
  
  # nonlinear regression weights
  inits <- numeric(2)
  inits[1] <- 1
  inits[2] <- -0.1   # assume negative relation for initial condition
  sqResids <- y
  lnload <-x
  nls.st <- c(a = inits[1], b1 = inits[2])
  regnls <- nls(sqResids ~ a * lnload**b1,start = nls.st)
  Estimate <- as.double(coef(regnls))
  weights_nlr <- 1/(Estimate[1] * x**Estimate[2]) # returned reciprocal variance
  
  weights_nlr <- weights_nlr * mean(1/weights_nlr)  # normalization by the mean of reciprocal weights (eqtn 1.5, Schwarz et al. 2006)
  
  NLLS_weights <- "lnload"
  Csites.weights.lnload.list <- named.list(NLLS_weights,weights_nlr,sqResids,lnload,regnls)
  assign("Csites.weights.lnload.list",Csites.weights.lnload.list,envir = .GlobalEnv)
  
  # output plot to PDF file
  filename <- paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,xrun_id,.Platform$file.sep,
                    "estimate",.Platform$file.sep,xrun_id,"_weights.pdf")
  pdf(file=filename,font="Helvetica")
  xy <- data.frame(x,y)
  xy <- xy[order(x),]
  plot(xy$x,xy$y,ylab="Squared Log Residuals & Normalized Weights",xlab="Log Predicted Load",
       main="NLLS 'lnload' Weight Function &  Normalized Weights",cex=0.7)
  NLS_predict <- Estimate[1] * xy$x**Estimate[2]
  lines(xy$x,NLS_predict,col="red")
  xy <- data.frame(lnload,weights_nlr)
  xy <- xy[order(lnload),]
  lines(xy$lnload,xy$weights_nlr,col="blue")
  legend("topright",cex=1.1,c("Nonlinear LS fit to log residuals","Normalized weights"),
         lty=c("solid","solid"),col=c("red","blue"),bty = "n")
  dev.off()  # shuts down current graphics device
  graphics.off()  # shuts down all open graphics devices
  
  
  # transfer weights_nlr (length=number calibration sites) to weight (length=number reaches)
  weight <- numeric(nreaches)
  calsites <- ifelse(is.na(calsites),0,calsites)
  weight[calsites==1] <- weights_nlr
  
  
  return(weight)  # return the nonlinear weights
  
}#end function


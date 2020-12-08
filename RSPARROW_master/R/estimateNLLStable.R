#'@title estimateNLLStable
#'@description Outputs all model performance and other summary metrics and diagnostics for the 
#'            estimated model for calibration and validates sites to the ~/estimate/(run_id)_summary.txt 
#'            file.  \\cr \\cr
#'Executed By: estimate.R \\cr
#'Executes Routines: unPackList.R \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param if_estimate yes/no indicating whether or not estimation is run
#'@param if_estimate_simulation character string setting from sparrow_control.R indicating 
#'       whether estimation should be run in simulation mode only.
#'@param ifHess yes/no indicating whether second-order Hessian standard errors should be 
#'       computed
#'@param if_sparrowEsts value of 1 `if_estimate_simulation<-'no'` indicating calculation of 
#'       Jacobian diagnostics and 0 `if_estimate_simulation<-'yes'` indicating no calculation of Jacobian diagnostics
#'@param classvar character vector of user specified spatially contiguous discrete 
#'       classification variables from sparrow_control.  First element is reach classification variable.
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0
#'                & subdata$calsites==1), ]`. The object contains the dataDictionary 
#'                ‘sparrowNames’ variables, with records sorted in hydrological 
#'                (upstream to downstream) order (see the documentation Chapter 
#'                sub-section 5.1.2 for details)
#'@param numsites number of sites selected for calibration
#'@param ANOVA.list list output of  model performance-related variables for the calibration sites 
#'                  from `estimateNLLSmetrics.R` contained in the estimate.list object. For more 
#'                  details see documentation Section 5.2.4.8
#'@param JacobResults list output of Jacobian first-order partial derivatives of the model 
#'       residuals `estimateNLLSmetrics.R` contained in the estimate.list object.  For more details see 
#'       documentation Section 5.2.4.5.
#'@param HesResults list output of Hessian second-order standard errors 
#'       `estimateNLLSmetrics.R` contained in the estimate.list object.  For more details see 
#'       documentation Section 5.2.4.6
#'@param sparrowEsts list object contained in estimate.list `if_estimate<-'yes'`.  For more 
#'       details see documentation Section 5.2.4.4.
#'@param Mdiagnostics.list list output containing summary variables for calibration sites from 
#'                         `estimateNLLSmetrics.R` contained in the estimate.list object.  For 
#'                         more details see documentation Section 5.2.4.7.
#'@param Cor.ExplanVars.list list output from `correlationMatrix.R`
#'@param if_validate yes/no indicating whether or not validation is run
#'@param vANOVA.list list output of  model performance-related variables for the validation sites 
#'                   from `validateMetrics.R` contained in the estimate.list object. For more details 
#'                   see documentation Section 5.2.4.15
#'@param vMdiagnostics.list list output containing summary variables for validation sites from 
#'                          `validateMetrics.R` contained in the estimate.list object.  For more 
#'                          details see documentation Section 5.2.4.14.
#'@param betavalues data.frame of model parameters from parameters.csv
#'@param Csites.weights.list regression weights as proportional to incremental area size


estimateNLLStable <- function(file.output.list,if_estimate,if_estimate_simulation,ifHess,if_sparrowEsts,
                              classvar,sitedata,numsites,
                              ANOVA.list,JacobResults,HesResults,sparrowEsts,Mdiagnostics.list,
                              Cor.ExplanVars.list,
                              if_validate,vANOVA.list,vMdiagnostics.list,
                              betavalues,Csites.weights.list) {
  
  
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  filename <- paste0(path_results,"estimate",.Platform$file.sep,run_id,"_summary.txt")
  dir.create(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"summaryCSV"))
  fileCSV<-paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"summaryCSV",.Platform$file.sep)
  sink(file=filename,split="FALSE",append=FALSE)
  
  # create global variable from lists
  unPackList(lists = list(Csites.weights.list = Csites.weights.list,
                          Mdiagnostics.list = Mdiagnostics.list,
                          JacobResults = JacobResults,
                          ANOVA.list = ANOVA.list),
             parentObj = list(NA,
                              NA,
                              NA,
                              NA))
  
  if(!is.na(Cor.ExplanVars.list)) {
    unPackList(lists = list(Cor.ExplanVars.list = Cor.ExplanVars.list),
               parentObj = list(NA))
  }
  if(ifHess == "yes" & if_estimate_simulation == 'no') {
    unPackList(lists = list(HesResults = HesResults),
               parentObj = list(NA))
  }
  
  #get description and units from betavalues
  betavalues<-betavalues[,which(names(betavalues) %in% c("sparrowNames","description","parmUnits"))]
  
  # define title output function
  outcharfun<-function(char) {
    outchar <- data.frame(char)
    row.names(outchar ) <- c(" ")
    colnames(outchar ) <- c(" ")
    return(outchar)
  }
  
  # define "space" for printing
  ch <- character(1)
  space <- data.frame(ch)
  row.names(space) <- ch
  colnames(space) <- c(" ")
  
  
  ##################################
  options(width = 500, max.print=50000) 
  
  print(outcharfun("SPARROW NLLS MODEL SUMMARY"))
  
  print(outcharfun(paste0("MODEL NAME: ",run_id)))
  
  print(outcharfun(paste0("FILE PATH: ",filename)))
  
  print(space)
  dd <- data.frame(mobs,npar,DF,SSE,MSE,RMSE,RSQ,RSQ_ADJ,RSQ_YLD,PBias)
  colnames(dd) <- c("MOBS","NPARM","DF","SSE","MSE","RMSE","RSQ","RSQ-ADJUST","RSQ-YIELD","PERCENT BIAS")
  ch <- character(1)
  row.names(dd) <- ch
  
  # only output estimation performance is model estimated (i.e., simulation)
  if (if_estimate == 'yes') {
    print(outcharfun("MODEL ESTIMATION PERFORMANCE (Monitoring-Adjusted Predictions)"))
    print(dd)
    fileout<-paste0(fileCSV,"ModelPerformanceMonitoringAdj.csv")
    fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  }
  
  dd <- data.frame(mobs,npar,DF,pSSE,pMSE,pRMSE,pRSQ,pRSQ_ADJ,pRSQ_YLD,pPBias)
  colnames(dd) <- c("MOBS","NPARM","DF","SSE","MSE","RMSE","RSQ","RSQ-ADJUST","RSQ-YIELD","PERCENT BIAS")
  ch <- character(1)
  row.names(dd) <- ch
  print(outcharfun("MODEL SIMULATION PERFORMANCE (Simulated Predictions)"))
  print(dd)
  fileout<-paste0(fileCSV,"ModelPerformanceNoMonitoringAdj.csv")
  fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  if (if_estimate == 'yes') {
    writeLines("\n   Simulated predictions are computed using mean coefficients from the NLLS model \n     that was estimated with monitoring-adjusted (conditioned) predictions\n")
  }
  
  if(if_validate == "yes" & if_estimate_simulation == "no"){
    
    # create global variable from list names (vANOVA.list)
    for(i in 1:length(vANOVA.list)){
      tempobj=vANOVA.list[[i]]
      eval(parse(text=paste(names(vANOVA.list)[[i]],"= tempobj")))
    }
    dd <- data.frame(mobs,pSSE,pMSE,pRMSE,pRSQ,pRSQ_ADJ,pRSQ_YLD,pPBias)
    colnames(dd) <- c("MOBS","SSE","MSE","RMSE","RSQ","RSQ-ADJUST","RSQ-YIELD","PERCENT BIAS")
    ch <- character(1)
    row.names(dd) <- ch
    print(outcharfun("MODEL VALIDATION PERFORMANCE (Simulated Predictions)"))
    print(dd)
    fileout<-paste0(fileCSV,"ModelValidationNoMontinoringAdj.csv")
    fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  }
  
  print(outcharfun("PARAMETER SUMMARY"))
  # print parameter estimates w/o standard errors
  dd <- data.frame(Parmnames,oEstimate,btype,esttype,Beta.inital,bmin,bmax)
  colnames(dd) <- c("PARAMETER","ESTIMATE","PARM TYPE","EST TYPE","INITIAL VALUE","MIN","MAX")
  dd$rname <- as.numeric(row.names(dd))
  dd<-merge(dd,betavalues, by.y ="sparrowNames",by.x="PARAMETER")
  dd <- dd[with(dd,order(dd$rname)), ]
  dd <- within(dd, rm(rname))  # drop rname
  colnames(dd)<-c("PARAMETER","ESTIMATE","PARM TYPE","EST TYPE","INITIAL VALUE","MIN","MAX","DESCRIPTION","PARAMETER UNITS")
  ch <- character(1)                 # changed from (2)  9-11-2014
  for (i in 1:length(Parmnames)) {
    ch[i] <- as.character(i)
  }
  row.names(dd) <- ch
  print(dd,right=FALSE)
  fileout<-paste0(fileCSV,"ParameterSummary.csv")
  fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  if(sum(ifelse(esttype=="Fixed",1,0))>0) {     # at least one Fixed parameter type found
    writeLines("\n   A 'Fixed' parameter estimation type (EST TYPE) indicates a user choice of a constant \n     coefficient value or a coefficient estimate equal to zero, the minimum or maximum  \n     boundary value (this may indicate a statistically insignificant coefficient, a \n     coefficient with a value outside of the bounds, or an unusually small initial \n     parameter value).")
  }
  
  # options for weighted SPARROW optimization
  if(NLLS_weights=="lnload") {     
    writeLines("\n   The model was estimated with a weighted error variance. The weights are proportional \n     to the log predicted load to account for heteroscedasticity.")
    x <- paste0("\n   NLLS_weights control setting = ",NLLS_weights)
    writeLines(x)
  }
  if(NLLS_weights=="user") {    
    writeLines("\n   The model was estimated with a weighted error variance. The weights are assigned by the user, expressed as the \n     reciprocal of the variance proportional to user-selected variables.")
    x <- paste0("\n   NLLS_weights control setting = ",NLLS_weights)
    writeLines(x)
  }
  
  
  if(if_estimate_simulation == "yes") {
    print(outcharfun(" Model simulation executed using intial values of parameters"))
  }
  
  if (if_estimate == "yes" & if_estimate_simulation == 'no') {
    
    # print parameter estimates with Jacobian SEs
    ddJ <- data.frame(Parmnames,btype,oEstimate,oSEj,oTj,opTj,oVIF)
    ddJ$rname <- as.numeric(row.names(ddJ))
    colnames(ddJ) <- c("PARAMETER","PARM TYPE","ESTIMATE","SE(Jcb)","T(Jcb)","P-VALUE(Jcb)","VIF","rname")
    ddJ<-merge(ddJ,betavalues, by.y ="sparrowNames",by.x="PARAMETER")
    colnames(ddJ) <- c("PARAMETER","PARM TYPE","ESTIMATE","SE(Jcb)","T(Jcb)","P-VALUE(Jcb)","VIF","rname","DESCRIPTION","PARAMETER UNITS")
    ch <- character(1)
    for (i in 1:length(Parmnames)) {
      ch[i] <- as.character(i)
    }
    row.names(ddJ) <- ch
    if(ifHess == 'no'){
      print(outcharfun("PARAMETER ESTIMATES"))
      ddJ <- ddJ[with(ddJ,order(ddJ$rname)), ]
      ddJ <- within(ddJ, rm(rname))  # drop rname
      ch <- character(1)
      for (i in 1:length(Parmnames)) {
        ch[i] <- as.character(i)
      }
      row.names(ddJ) <- ch
      print(ddJ,right=FALSE)
      fileout<-paste0(fileCSV,"ParameterEstimates.csv")
      fwrite(ddJ,file=fileout,row.names=F,append=F,quote=T,showProgress = FALSE,
             dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
      print(space)
    }
    
    
    if(ifHess == 'yes' & if_estimate_simulation == 'no'){
      # print parameter estimates with Hessian SEs
      ddH <- data.frame(Parmnames,btype,oEstimate,oSEh,oTh,opTh)
      ddH$rname <- as.numeric(row.names(ddH))
      colnames(ddH) <- c("PARAMETER","PARM TYPE","ESTIMATE","SE","T","P-VALUE","rname")
      dd<-merge(ddH,ddJ, by=c("rname","PARAMETER","PARM TYPE","ESTIMATE"))
      dd <- dd[with(dd,order(dd$rname)), ]
      dd <- within(dd, rm(rname,"SE(Jcb)","T(Jcb)","P-VALUE(Jcb)"))  # drop rname and Jacobian metrics
      ch <- character(1)
      for (i in 1:length(Parmnames)) {
        ch[i] <- as.character(i)
      }
      row.names(dd) <- ch
      print(outcharfun("PARAMETER ESTIMATES"))
      print(dd,right=FALSE)
      fileout<-paste0(fileCSV,"ParameterEstimates.csv")
      fwrite(dd,file=fileout,row.names=F,append=F,quote=T,showProgress = FALSE,
             dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    }
    print(space)
    dd <- data.frame(e_val_spread,ppcc,shap.test,shap.p,mean_exp_weighted_error)
    colnames(dd) <- c("EigenValue Spread"," Normal PPCC"," SWilks W"," P-Value","  Mean Exp Weighted Error")
    ch <- " "
    row.names(dd) <- ch
    print(dd)
    fileout<-paste0(fileCSV,"EigenValueSpread.csv")
    fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    
    # print design matrix selections for model execution
    if(sum(ifelse(JacobResults$btype=="DELIVF",1,0)) > 0) {
      dd <- as.data.frame(odesign)
      ndeliv <- ncol(odesign)
      nsrc <- nrow(odesign)
      row.names(dd) <- Parmnames[1:nsrc]
      colnames(dd) <- Parmnames[nsrc+1:ndeliv]
      print(outcharfun("DESIGN MATRIX"))
      print(dd)
      fileout<-paste0(fileCSV,"DesignMatrix.csv")
      fwrite(dd,file=fileout,row.names=T,append=F,quote=F,showProgress = FALSE,
             dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    }
    
  } # end if_estimate check
  
  
  # Residuals
  print(space)
  print("LOG RESIDUALS, Station quantiles",quote=FALSE)
  print(quantile(round(sparrowEsts$resid,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97)))
  fileout<-paste0(fileCSV,"LogResid_StationQuantiles.csv")
  dd<-as.data.frame(t(quantile(round(sparrowEsts$resid,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97))))
  fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  # Standardized Residuals
  if(if_estimate == "yes" & if_estimate_simulation=="no"){
    x <- Mdiagnostics.list$standardResids
    if(exists("x")) {
      if(is.finite(JacobResults$mean_exp_weighted_error)){
        print(space)
        print("STANDARDIZED RESIDUALS, Station quantiles",quote=FALSE)
        print(quantile(round(Mdiagnostics.list$standardResids,digits=3),c(0.025,0.16,0.2,0.3,0.5,0.7,0.84,0.9,0.97)))
        fileout<-paste0(fileCSV,"StandardResid_StationQuantiles.csv")
        dd<-as.data.frame(t(quantile(round(Mdiagnostics.list$standardResids,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97))))
        fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
               dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
        
        if(JacobResults$mean_exp_weighted_error>1.0E+3) {
          message("
  WARNING: THE Mean Exp Weighted Error PRINTED IN THE SUMMARY TEXT FILE
  IS EXCESSIVELY LARGE. THIS IS CAUSED BY A LARGE LEVERAGE AND MODEL RESIDUAL
  FOR A STATION. CHECK THE DATA FOR THE OUTLYING STATION. ALSO CONSIDER 
  RE-ESTIMATING THE MODEL USING DIFFERENT INITIAL PARAMETER VALUES OR AFTER 
  ELIMINATING VARIABLES WITH SMALL AND STATISTICALLY INSIGNIFICANT 
  ESTIMATED COEFFICIENTS.
  ")
        }
      } else {
        message("
   WARNING: THE Mean Exp Weighted Error IS UNDEFINED, CAUSED BY A LEVERAGE VALUE OF ONE. 
   A PARAMETER MAY HAVE BEEN IMPROPERLY ESTIMATED.
   EVALUATE DIFFERENT INITIAL VALUES FOR THE PARAMETERS, INCLUDING INITIAL VALUES
   CLOSER TO THE ESTIMATED COEFFICIENT VALUES, OR ELIMINATE VARIABLES WITH SMALL
   AND STATISTICALLY INSIGNIFICANT ESTIMATED COEFFICIENTS.
   DIAGNOSTIC PLOTS WERE NOT OUTPUT.")
      }
    }}
  
  # Prediction accuracy statistics
  print(space)
  print("RATIO OF OBSERVED TO PREDICTED LOAD, Station quantiles",quote=FALSE)
  print(quantile(round(ratio.obs.pred,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97)))
  fileout<-paste0(fileCSV,"RatioObsToPredLoad_StationQuantiles.csv")
  dd<-as.data.frame(t(quantile(round(ratio.obs.pred,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97))))
  fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  # Observed yield statistics
  print(space)
  print("OBSERVED YIELD, percentiles",quote=FALSE)
  print(summary(yldobs))
  fileout<-paste0(fileCSV,"ObservedYieldPercentiles.csv")
  dd<-as.data.frame(t(unclass(summary(yldobs))))
  fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  # Prediction yield statistics
  print(space)
  print("PREDICTED YIELD, percentiles",quote=FALSE)
  print(summary(yldpredict))
  fileout<-paste0(fileCSV,"PredictedYieldPercentiles.csv")
  dd<-as.data.frame(t(unclass(summary(yldpredict))))
  fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  
  if(if_validate == "yes" & if_estimate_simulation == "no"){
    vresids <- vMdiagnostics.list$pResids
    vratio <- vMdiagnostics.list$pratio.obs.pred
    # Validation Residuals
    print(space)
    print(outcharfun("MODEL VALIDATION (simulated predictions)"))
    print("LOG RESIDUALS, Station quantiles",quote=FALSE)
    print(quantile(round(vresids,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97)))
    fileout<-paste0(fileCSV,"LogResid_StationQuantiles_NoMonitoringAdj.csv")
    dd<-as.data.frame(t(quantile(round(vresids,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97))))
    fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    # Validation accuracy metrics
    print(space)
    print(outcharfun("MODEL VALIDATION (simulated predictions)"))
    print("RATIO OF OBSERVED TO PREDICTED LOAD, Station quantiles",quote=FALSE)
    print(quantile(round(vratio,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97)))
    fileout<-paste0(fileCSV,"RatioObsToPredLoad_StationQuantiles_NoMonitoringAdj.csv")
    dd<-as.data.frame(t(quantile(round(vratio,digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97))))
    fwrite(dd,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  }
  
  if(NLLS_weights!="default") {    
    # Output weights percentiles
    print(space)
    print(outcharfun("MODEL WEIGHTS"))
    print("Model residual weights, Station quantiles",quote=FALSE)
    print(quantile(round(weight,digits=5),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97)))
  }
  
  sitedata[,"weight"]  <- NULL   # eliminate 'weight' and use more current value from Csites.weights.list object
  
  # output largest outliers
  Resids <- sparrowEsts$resid
  residCheck <- abs(standardResids)
  dd <- data.frame(sitedata,standardResids,Resids,leverage,leverageCrit,CooksD,CooksDpvalue,residCheck,weight,tiarea)
  dd1 <- subset(dd,dd$residCheck > 3 | dd$leverage > dd$leverageCrit | dd$CooksDpvalue < 0.10)
  keeps <- c("waterid","demtarea","rchname","station_id","station_name","staid",
             classvar[1],"standardResids","Resids","leverage","leverageCrit","CooksD","CooksDpvalue","weight","tiarea","residCheck")
  ddnew <- dd1[keeps]
  
  print(space)
  print("LARGEST OUTLIERS",quote=FALSE)
  print("(absolute standardized residual>3, leverage>Critical value, or Cook's D p-value<0.10)",quote=FALSE)
  print(ddnew)
  fileout<-paste0(fileCSV,"LargestSqResid.csv")
  fwrite(ddnew,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  # output CLASS region performance 
  print(space)
  v <- rep(1:length(RMSEnn),1)
  dd <- data.frame(classgrp,RMSEnn,SSEclass)
  colnames(dd) <- c("REGION","NUMBER OF SITES","SSE")
  ch <- character(1)
  for (i in 1:length(v)) {
    ch[i] <- as.character(i)
  }
  row.names(dd) <- ch
  print(outcharfun("REGIONAL MODEL PERFORMANCE (Monitoring-Adjusted Predictions)"))
  print(dd)
  fileout<-paste0(fileCSV,"ClassRegionModelPerformance_MonitoringAdj.csv")
  fwrite(dd,file=fileout,row.names=T,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  
  
  print(space)
  v <- rep(1:length(RMSEnn),1)
  dd <- data.frame(classgrp,pSSEclass)
  colnames(dd) <- c("REGION","SSE")
  ch <- character(1)
  for (i in 1:length(v)) {
    ch[i] <- as.character(i)
  }
  row.names(dd) <- ch
  print(outcharfun("REGIONAL MODEL PERFORMANCE (Simulated Predictions)"))
  print(dd)
  fileout<-paste0(fileCSV,"ClassRegionModelPerformance_NoMonitoringAdj.csv")
  fwrite(dd,file=fileout,row.names=T,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  
  if(ifHess == 'yes' & if_estimate_simulation == 'no'){
    # Output parameter covariances, correlations, and Eigenvalues and Eigenvectors
    # Covariances
    dd <- data.frame(cov2)
    colnames(dd) <- Hesnames
    ch <- character(1)
    ch <- Hesnames
    row.names(dd) <- ch
    
    
    print(outcharfun("PARAMETER COVARIANCES"))
    print(dd)
    print(space)
    fileout<-paste0(fileCSV,"ParameterCovariances.csv")
    fwrite(dd,file=fileout,row.names=T,append=F,quote=F,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    # Correlations
    dd <- data.frame(cor2)
    colnames(dd) <- Hesnames
    ch <- character(1)
    ch <- Hesnames
    row.names(dd) <- ch
    print(outcharfun("PARAMETER CORRELATIONS"))
    print(dd)
    print(space)
    fileout<-paste0(fileCSV,"ParameterCorrelations.csv")
    fwrite(dd,file=fileout,row.names=T,append=F,quote=F,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    # Eigenvectors
    dd <- data.frame(e_vec)
    ch <- " "
    colnames(dd) <- rep(ch,times=ncol(dd))
    rNames <- Hesnames
    ch <- character(1)
    ch[1] <- "EigenValues"
    for (i in 2:(length(rNames)+1) ) {
      ch[i] <- rNames[i-1]
    }
    row.names(dd) <- ch
    print(outcharfun("X'X EIGENVALUES AND EIGENVECTORS"))
    print(dd)
    print(space)
    fileout<-paste0(fileCSV,"EigenvaluesEigenvectors.csv")
    fwrite(dd,file=fileout,row.names=T,append=F,quote=F,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  } # end 'ifHess'
  
  # Explanatory variable correlations
  if (if_corrExplanVars == "yes" & !is.na(Cor.ExplanVars.list)) {
    
    if(numsites>2){
      print(outcharfun("CORRELATION MATRICES FOR EXPLANATORY VARIABLES (Site Incremental Areas)"))
      print(outcharfun("SPEARMAN CORRELATIONS FOR ALL OBSERVATIONS"))
      print(cor.allValuesM)
      
      print(outcharfun("SUMMARY METRICS FOR EXPLANATORY VARIABLES (Site Incremental Areas)"))
      print(summary(cmatrixM_all))
      
      print(outcharfun("FILTERED SUMMARY METRICS FOR EXPLANATORY VARIABLES (zero values converted to minimum of non-zero values)"))
      print(summary(cmatrixM_filter))
    }
    
    print(space)
    print(space)
    print(outcharfun("CORRELATION MATRICES FOR EXPLANATORY VARIABLES (Reaches)"))
    print(outcharfun("SPEARMAN CORRELATIONS FOR ALL OBSERVATIONS"))
    print(cor.allValues)
    xtext <- paste0("SPEARMAN CORRELATIONS FOR SUBSAMPLE OF OBSERVATIONS (n=",nsamples,")")
    print(outcharfun(xtext))
    print(cor.sampleValues)
    print(outcharfun("SPEARMAN CORRELATIONS FOR SUBSAMPLED LOGGED OBSERVATIONS (zero values are converted to minimum of non-zero values)"))
    print(cor.sampleLogValues)
    
    print(outcharfun("SUMMARY METRICS FOR EXPLANATORY VARIABLES (Reaches)"))
    print(summary(cmatrix_all))
    
    print(outcharfun("FILTERED SUMMARY METRICS FOR EXPLANATORY VARIABLES (zero values converted to minimum of non-zero values)"))
    print(summary(cmatrix_filter))
  }
  
  sink(type="message")
  sink()
  
  # output Residuals file
  Resids <- sparrowEsts$resid
  Obsyield <- Obs / sitedata$demtarea
  predictYield <- predict / sitedata$demtarea
  
  
  origWaterid<-sitedata$waterid_for_RSPARROW_mapping
  dd <- data.frame(sitedata,origWaterid,Obs,predict,Obsyield,predictYield,Resids,standardResids,leverage,leverageCrit,
                   CooksD,CooksDpvalue,boot_resid,weight,tiarea,pResids,
                   ratio.obs.pred,pratio.obs.pred,xlat,xlon)
  
  keeps <- c("waterid","origWaterid","demtarea","rchname","station_id","station_name","staid",classvar[1],"Obs",
             "predict","Obsyield","predictYield","Resids","standardResids","leverage","leverageCrit","CooksD","CooksDpvalue","boot_resid","weight","tiarea","pResids",
             "ratio.obs.pred","pratio.obs.pred","xlat","xlon")
  ddnew <- dd[keeps] 
  
  # Add jacobian derivatives to the residuals file for estimated coefficients (JacobResults object variables)
  #  Derivatives not reported for "Fixed" coefficients 
  if (if_estimate == "yes" & if_estimate_simulation == 'no') {
    Parmnames <- Parmnames[JacobResults$bmin < JacobResults$bmax]     # eliminate user-fixed coefficients
    esttype <- esttype[JacobResults$bmin < JacobResults$bmax]
    jcolNames <- Parmnames[esttype == "Estimated"]       # eliminate any Fixed coefficients (zero or outside min/max bounds)
    jcolNames <- paste0(jcolNames,"_Jgradient")
    jacobian <- jacobian[,esttype == "Estimated"] 
    jacobian <- as.matrix(jacobian)
    colnames(jacobian) <- jcolNames
    ddnew <- cbind(ddnew,jacobian)
  }
  
  if (length(na.omit(add_vars))!=0){
    add_data<-data.frame(sitedata[,which(names(sitedata) %in% add_vars)])
    if (length(add_vars)==1){
      names(add_data)<-add_vars
    }
    ddnew<-cbind(ddnew,add_data)
  }
  ddnew$rchname <- str_replace_all(ddnew$rchname,"[[:punct:]]","")
  ddnew$station_name <- str_replace_all(ddnew$station_name,"[[:punct:]]","")
  fileout <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_residuals.csv")
  fwrite(ddnew,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  
  
  
}#end function

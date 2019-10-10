#'@title diagnosticPlotsNLLS
#'@description Creates diagnostic plots and maps output to 
#'            ~/estimate/(run_id)_diagnostic_plots.pdf, and saves residual maps as shape files. \\cr \\cr
#'Executed By: estimate.R \\cr
#'Executes Routines: \\itemize\{\\item checkBinaryMaps.R
#'             \\item diagnosticMaps.R
#'             \\item mapSiteAttributes.R
#'             \\item unPackList.R\} \\cr
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0), ]`
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param add_vars additional variables specified by the setting `add_vars` to be included in 
#'       prediction, yield, and residuals csv and shape files
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



diagnosticPlotsNLLS<- function(file.output.list,class.input.list,sitedata.demtarea.class,
                               sitedata,sitedata.landuse,estimate.list,mapping.input.list,Csites.weights.list,
                               Cor.ExplanVars.list,
                               data_names,add_vars,batch_mode) {
  
  
  
  #########################
  # Create Global Variables
  #########################
  
  
  
  # create global variable from list names (Mdiagnostics.list)
  unPackList(lists = list(mapping.input.list = mapping.input.list,
                          Mdiagnostics.list = estimate.list$Mdiagnostics.list,
                          file.output.list = file.output.list,
                          class.input.list = class.input.list),
             parentObj = list(NA,
                              NA,
                              NA,
                              NA))
  
  # contiguous class variables by sites
  class <- array(0,dim=c(nrow=nrow(sitedata),ncol=length(classvar))) 
  for (k in 1:length(classvar)) { 
    for (i in 1:nrow(sitedata)) {
      class[i,k] <- as.numeric(eval(parse(text=paste("sitedata$",classvar[k],"[",i,"]",sep=""))))
    } 
  } 
  
  # Create 'classvar2' for plotting landuse non-contiguous class
  #   following code executes:  classvar2 <- c("forest_pct","agric_pct","urban_pct","shrubgrass_pct")
  if(!is.na( class_landuse[1])){
    classvar2 <- character(length(class_landuse))
    for (i in 1:length(class_landuse)) {
      classvar2[i] <- paste(class_landuse[i],"_pct",sep="")
    }
  }
  
  
  filename <- paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_diagnostic_plots.pdf",sep="")
  pdf(file=filename,font="Helvetica",paper="special")
  
  ################################    
  #map site attributes
  ################################
  existGeoLines<-checkBinaryMaps(LineShapeGeo,path_gis,batch_mode)
  
  #add text explanation 11.3.17
  par(mfrow=c(1,1)) 
  if (!is.na(map_siteAttributes.list) & existGeoLines==TRUE){
    strExplanation<-paste("
      Output is presented for the following sections:\n
      -Calibration Site Maps for User-Selected Attributes\n
      -Model Estimation Performance Diagnostics\n
      -Model Simulation Performance Diagnostics\n
      -Maps of Model Residuals and Observed to Predicted Ratios 
       for the Calibration Sites")
  }else if (existGeoLines==TRUE){
    strExplanation<-paste("
      Output is presented for the following sections:\n
      -Model Estimation Performance Diagnostics\n
      -Model Simulation Performance Diagnostics\n
      -Maps of Model Residuals and Observed to Predicted Ratios 
       for the Calibration Sites")   
  }else{
    strExplanation<-paste("
      Output is presented for the following sections:\n
      -Model Estimation Performance Diagnostics\n
      -Model Simulation Performance Diagnostics")
  }
  gplots::textplot(strExplanation,valign="top", cex=0.8, halign="left")
  title(main=paste(run_id,"_diagnostic_plots.pdf \n Document Contents",sep=""))
  
  if(existGeoLines==TRUE) { 
    
    #map site attributes
    if (!is.na(map_siteAttributes.list)){
      
      
      #add text explanation 11.3.17
      par(mfrow=c(1,1))
      strExplanation<-paste(" \n")
      gplots::textplot(strExplanation,valign="top", cex=1.1, halign="center")
      title("Calibration Site Maps for User-Selected Attributes")
      
      for (s in 1:length(map_siteAttributes.list)){
        if (length(names(sitedata)[which(names(sitedata)==map_siteAttributes.list[s])])!=0){
          siteAttr<-eval(parse(text= paste("data.frame(",map_siteAttributes.list[s],"=sitedata$",map_siteAttributes.list[s],")",sep="")))
          titleAttr<-data_names[which(data_names$sparrowNames==map_siteAttributes.list[s]),]
          unitAttr<-titleAttr$varunits
          titleAttr<-as.character(titleAttr$explanation)
          mapdata<-data.frame(xlat,xlon,siteAttr)
          input<-list(var=NA, sizeBatch=NA,size=NA)
          mapSiteAttributes(#Rshiny
            input,"", path_gis, sitedata, LineShapeGeo,data_names,FALSE,
            #regular
            names(siteAttr),mapdata,GeoLines,mapping.input.list,titleAttr,unitAttr,batch_mode)
        }else{
          message(paste("WARNING : ",map_siteAttributes.list[s], " DOES NOT EXIST.  MAP SITE ATTRIBUTE TERMINATED",sep=""))
          if (batch_mode=="yes"){
            cat("\n \n")
            cat("WARNING : ",map_siteAttributes.list[s], " DOES NOT EXIST.  MAP SITE ATTRIBUTE TERMINATED",sep="")
            cat("\n \n")
          }#if batch_mode
        }#end if attribute exists
      }#end for each attribute
    }#end if map_siteAttr
  }
  ##################################################
  # PERFORMANCE METRICS FOR MONITORING ADJUSTMENT
  ##################################################
  
  # Full spatial domain
  #add text explanation 11.3.17
  par(mfrow=c(1,1))
  strExplanation<-paste("
        Diagnostics are based on the use of conditioned (monitoring-adjusted) predictions. 
        These predictions provide the most accurate reach predictions for use in calibrating 
        the model. The associated residuals and observed to predicted ratios shown in the 
        following section provide the most relevant measures of the accuracy of the model fit 
        to observed loads. \n
        The diagnostic plots include:
        -Four-plot panel for observed vs. predicted for loads and yields, and log residuals 
          vs. predicted loads and yields\n
        -Four-plot panel for boxplots of residuals and observed/predicted ratios, normal 
          quantile plot of standardized residuals, and plot of squared residuals vs. predicted 
          loads\n
        -Plot of conditioned prediction loads vs. unconditioned (simulated) prediction loads\n
        -Plots of the observed to predicted ratio vs. the area-weighted mean values of the 
          user-selected explanatory variables for the incremental areas between calibration 
          sites (output only if control setting if_corrExplanVars<-'yes' selected and a value 
          of 1 entered for 'parmCorrGroup' column in the 'parameters.csv' file)\n
        -Boxplots of the observed to predicted loads vs. the decile classes of the total 
          drainage area for the calibration sites\n
        -Boxplots of the observed to predicted loads vs. the contiguous spatial classes 
          specified by users in the 'classvar' control setting (e.g., HUC-4)\n
        -Boxplots of the observed to predicted loads vs. the deciles of the land-use class 
          variable specified by users in the 'class_landuse' control setting, with the 
          land-use classes expressed as a percentage of the incremental drainage area 
          extending from the calibration site to the nearest upstream site locations\n
        -Four-plot panels reported separately for each of the contiguous spatial classes 
          specified for the first variable entry for the 'classvar[1]' control setting. The 
          panels include:  observed vs. predicted loads, observed vs. predicted yields, log 
          residuals vs. predicted loads, and log residuals vs. predicted yields ")
  
  gplots::textplot(strExplanation,valign="top", cex=0.7, halign="left")
  title("Model Estimation Performance Diagnostics")
  
  # observed vs predicted 
  par(mfrow=c(2,2), pch=diagnosticPlotPointStyle, cex = diagnosticPlotPointSize)  # 4 plots on one page
  
  # observed vs. predicted mass
  plot(predict,Obs,log="xy",pch=1,main="MODEL ESTIMATION PERFORMANCE \n(Monitoring-Adjusted Predictions) \nObserved vs Predicted Load",
       ylab=paste0("OBSERVED LOAD (",loadUnits,")"),xlab=paste0("PREDICTED LOAD (",loadUnits,")"))
  lines(Obs,Obs, col=2)
  
  # observed vs. predicted yield
  plot(yldpredict,yldobs,log="xy",pch=1,main="MODEL ESTIMATION PERFORMANCE \nObserved vs Predicted Yield",
       ylab=paste0("OBSERVED YIELD (",yieldUnits,")"),xlab=paste0("PREDICTED YIELD (",yieldUnits,")"))
  lines(yldobs,yldobs, col=2)
  
  # mass residual plot
  plot(predict,Resids,log="x",pch=1,main="Residuals vs Predicted \nLoad",
       ylab="LOG RESIDUAL",xlab=paste0("PREDICTED LOAD (",loadUnits,")"))
  abline(h=0,col=2)
  
  # yield residual plot
  plot(yldpredict,Resids,log="x",pch=1,main="Residuals vs Predicted \nYield",
       ylab="LOG RESIDUAL",xlab=paste0("PREDICTED YIELD (",yieldUnits,")"))
  abline(h=0,col=2)
  
  
  ##########################
  par(mfrow=c(2,2), pch=diagnosticPlotPointStyle, cex = diagnosticPlotPointSize)  # 4 plots on one page
  
  # Residual plots
  boxplot(Resids,ylab="LOG RESIDUAL",main="MODEL ESTIMATION PERFORMANCE \nResiduals")
  
  # Obs-Pred ratio boxplot
  boxplot(ratio.obs.pred,ylim = c(0,2),ylab="RATIO OBSERVED TO PREDICTED",
          main="MODEL ESTIMATION PERFORMANCE \nObserved / Predicted Ratio")
  
  # Normality probability plot
  qqnorm(standardResids,ylab="Standardized Residuals")
  qqline(standardResids, col = 2)
  
  # Squared residuals vs predicted
  Resids2 <- Resids**2
  plot(predict,Resids2,log="xy",pch=1,main="Squared Residuals vs Predicted Load",
       ylab="SQUARED LOG RESIDUALS",xlab=paste0("PREDICTED LOAD (",loadUnits,")"))
  lwresids <- lowess(predict,Resids2, f = 0.5, iter = 3)
  lwy <- lwresids$y
  lwx <- lwresids$x
  lines(lwx,lwy,col=2)
  
  par(mfrow=c(1,1), pch=diagnosticPlotPointStyle, cex = diagnosticPlotPointSize)  # 1 plots on one page
  
  plot(ppredict,predict,log="xy",main="Monitoring-Adjusted vs. Simulated Loads",
       ylab=paste0("Monitoring-Adjusted Load (",loadUnits,")"),xlab=paste0("Simulated Load (",loadUnits,")"))
  lines(ppredict,ppredict,col="red")
  
  
  # Plots of the Obs-Pred ratio vs. the area-weighted mean values of the explanatory variables 
  #   for the incremental areas between calibration sites
  if(!is.na(Cor.ExplanVars.list)){ 
    for (i in 1:length(Cor.ExplanVars.list$names)) {
      if(min(Cor.ExplanVars.list$cmatrixM_all[,i])<0 | max(Cor.ExplanVars.list$cmatrixM_all[,i])<0) {
        plot(Cor.ExplanVars.list$cmatrixM_all[,i],ratio.obs.pred,pch=1,
             main=bquote(paste("Observed to Predicted Ratio vs Area-Weighted Explanatory Variable \nFor Incremental Areas between Calibration Sites; Variable Name = ",.(Cor.ExplanVars.list$names[i]) )),
             ylab="RATIO OBSERVED TO PREDICTED",xlab=paste0("AREA-WEIGHTED EXPLANATORY VARIABLE (",Cor.ExplanVars.list$names[i],")"))
      } else {
        plot(Cor.ExplanVars.list$cmatrixM_all[,i],ratio.obs.pred,log="x",pch=1,
             main=bquote(paste("Observed to Predicted Ratio vs Area-Weighted Explanatory Variable \nFor Incremental Areas between Calibration Sites; Variable Name = ",
                               .(Cor.ExplanVars.list$names[i]) )),
             ylab="RATIO OBSERVED TO PREDICTED",xlab=paste0("AREA-WEIGHTED EXPLANATORY VARIABLE (",Cor.ExplanVars.list$names[i],")"))
      }
    }
  }
  
  ####################################################
  # Diagnostics for Ratio by class (one plot per page)
  
  # sitedata.demtarea.class regions
  par(mfrow=c(1,1), pch=diagnosticPlotPointStyle, cex = diagnosticPlotPointSize)  # 1 plots on one page
  vvar <- sitedata.demtarea.class
  boxplot(ratio.obs.pred ~ vvar,log="y",main="Ratio Observed to Predicted by Deciles",
          xlab="Upper Bound for Total Drainage Area Deciles (km2)",ylab="Observed to Predicted Ratio")
  abline(h = 1, col = "red", lwd = 1) 
  
  
  
  # "classvar" regions
  if (classvar!="sitedata.demtarea.class"){
    for (k in 1:length(classvar)) {
      par(mfrow=c(1,1), pch=diagnosticPlotPointStyle, cex = diagnosticPlotPointSize)  # 1 plots on one page
      vvar <- as.numeric(eval(parse(text=paste("sitedata$",classvar[k],sep="") )))
      boxplot(ratio.obs.pred ~ vvar,log="y",main="Ratio Observed to Predicted",
              xlab=classvar[k],ylab="Observed to Predicted Ratio")
      abline(h = 1, col = "red", lwd = 1) 
    }}
  
  # 'classvar2" decile boxplots
  if(!is.na( class_landuse[1])){
    for (k in 1:length(classvar2)) {
      par(mfrow=c(1,1), pch=diagnosticPlotPointStyle, cex = diagnosticPlotPointSize)  # 1 plots on one page
      vvar <- as.numeric(eval(parse(text=paste("sitedata.landuse$",classvar2[k],sep="") )))
      iprob<-10
      chk <- unique(quantile(vvar, probs=0:iprob/iprob))
      chk1 <- 11 - length(chk)
      if(chk1 == 0) {
        qvars <- as.integer(cut(vvar, quantile(vvar, probs=0:iprob/iprob), include.lowest=TRUE)) 
        avars <- quantile(vvar, probs=0:iprob/iprob)
        qvars2 <- numeric(length(qvars))
        for (j in 1:10) {
          for (i in 1:length(qvars)){
            if(qvars[i] == j) {
              qvars2[i] <- round(avars[j+1],digits=0)
            }
          }
        }
        xxlab <- paste("Upper Bound for ",classvar2[k],sep="")  
        boxplot(ratio.obs.pred ~ qvars2,log="y",main="Ratio Observed to Predicted by Deciles",
                xlab=xxlab,ylab="Observed to Predicted Ratio")
        abline(h = 1, col = "red", lwd = 1) 
      } else {  # non-unique classes
        plot(vvar,ratio.obs.pred,log="y",main="Ratio Observed to Predicted",
             xlab=classvar2[k],ylab="Observed to Predicted Ratio")
        abline(h = 1, col = "red", lwd = 1) 
      }
    } # end 'classvar2' loop
  }
  
  ##################################################
  
  # Diagnostics by CLASS (contiguous geographic units)
  
  plotmrb <- function(xmrb,class,Obs,predict,yldobs,yldpredict,Resids) {
    xmrb <- as.double(xmrb)
    # observed vs predicted 
    par(mfrow=c(2,2), pch=diagnosticPlotPointStyle, cex = diagnosticPlotPointSize)  # 4 plots on one page
    
    #  observed vs. predicted mass
    df <- data.frame(predict,Obs)
    df <- subset(df,class == xmrb)
    nsites <- as.numeric(length(df$predict))
    
    if(nsites > 0) {
      plot(df$predict,df$Obs,log="xy",pch=1,
           ylab=paste0("OBSERVED LOAD (",loadUnits,")"),xlab=paste0("PREDICTED LOAD (",loadUnits,")"))
      title(font.main=2,main=bquote(paste("Observed vs Predicted Load \nCLASS Region = ",.(xmrb),"(n=",.(nsites),")") ))
      lines(df$Obs,df$Obs, col=2)
      
      # observed vs. predicted yield
      df <- data.frame(yldpredict,yldobs)
      df <- subset(df,class == xmrb)
      plot(df$yldpredict,df$yldobs,log="xy",pch=1,main="Observed vs Predicted \nYield",
           ylab=paste0("OBSERVED YIELD (",yieldUnits,")"),xlab=paste0("PREDICTED YIELD (",yieldUnits,")"))
      lines(df$yldobs,df$yldobs, col=2)
      
      # mass residual plot
      df <- data.frame(predict,Resids)
      df <- subset(df,class == xmrb)
      plot(df$predict,df$Resids,log="x",pch=1,main="Residuals vs Predicted \nLoad",
           ylab="LOG RESIDUAL",xlab=paste0("PREDICTED LOAD (",loadUnits,")"))
      eq <- rep(0,length(df$predict))
      lines(df$predict,eq, col=2)
      
      # yield residual plot
      df <- data.frame(yldpredict,Resids)
      df <- subset(df,class == xmrb)
      plot(df$yldpredict,df$Resids,log="x",pch=1,main="Residuals vs Predicted \nYield",
           ylab="LOG RESIDUAL",xlab=paste0("PREDICTED YIELD (",yieldUnits,")"))
      eq <- rep(0,length(df$Resids))
      lines(df$yldpredict,eq, col=2)
    }
  } # end function
  
  # Obtain CLASS region numbers
  grp <- table(class[,1])   # get labels
  xx <- as.data.frame(grp)  # convert table to dataframe...
  grp <- as.numeric(levels(xx$Var1)[xx$Var1])  # convert factor levels to numeric values
  
  for (i in 1:length(grp)) {
    plotmrb(grp[i],class[,1],Obs,predict,yldobs,yldpredict,Resids)
  }
  
  
  
  ##################################################
  # PERFORMANCE METRICS FOR NO MONITORING ADJUSTMENT
  ##################################################
  
  #add text explanation 11.3.17
  par(mfrow=c(1,1))
  strExplanation<-paste("
        Diagnostics are based on the use of unconditioned predictions (i.e., predictions 
        that are not adjusted for monitoring loads). These predictions (and the associated 
        residuals and observed to predicted ratios shown in the following section) provide 
        the best measure of the predictive skill of the estimated model in simulation mode. 
        The simulated predictions are computed using mean coefficients from the NLLS model 
        estimated with monitoring-adjusted (conditioned) predictions. \n
        The diagnostic plots include:
        -Four-plot panel for observed vs. predicted for loads and yields, and log residuals 
           vs. predicted loads and yields
        -Four-plot panel for boxplots of residuals and observed/predicted ratios, normal 
           quantile plot of standardized residuals, and plot of squared residuals vs. 
           predicted loads\n
        -Plot of conditioned prediction loads vs. unconditioned (simulated) prediction loads\n
        -Plots of the observed to predicted ratio vs. the area-weighted mean values of the 
           user-selected explanatory variables for the incremental areas between calibration 
           sites (output only if control setting if_corrExplanVars<-'yes' selected and a value 
           of 1 entered for 'parmCorrGroup' column in the 'parameters.csv' file)\n
        -Boxplots of the observed to predicted loads vs. the decile classes of the total 
           drainage area for the calibration sites\n
        -Boxplots of the observed to predicted loads vs. the contiguous spatial classes 
           specified by users in the 'classvar' control setting (e.g., HUC-4)\n
        -Boxplots of the observed to predicted loads vs. the deciles of the land-use class 
           variable specified by users in the 'class_landuse' control setting, with the 
           land-use classes expressed as a percentage of the incremental drainage area 
           extending from the calibration site to the nearest upstream site locations\n
        -Four-plot panels reported separately for each of the contiguous spatial classes 
           specified for the first variable entry for the 'classvar[1]' control setting. 
           The panels include:  observed vs. predicted loads, observed vs. predicted yields, 
           log residuals vs. predicted loads, and log residuals vs. predicted yields ")
  gplots::textplot(strExplanation,valign="top", cex=0.7, halign="left")
  title("Model Simulation Performance Diagnostics")
  
  # Full spatial domain
  
  # observed vs predicted 
  par(mfrow=c(2,2), pch=diagnosticPlotPointStyle, cex = diagnosticPlotPointSize)  # 4 plots on one page
  
  # observed vs. predicted mass
  plot(ppredict,Obs,log="xy",pch=1,main="MODEL SIMULATION PERFORMANCE \nObserved vs Predicted Load",
       ylab=paste0("OBSERVED LOAD (",loadUnits,")"),xlab=paste0("PREDICTED LOAD (",loadUnits,")"))
  lines(Obs,Obs, col=2)
  
  # observed vs. predicted yield
  plot(pyldpredict,pyldobs,log="xy",pch=1,main="MODEL SIMULATION PERFORMANCE \nObserved vs Predicted Yield",
       ylab=paste0("OBSERVED YIELD (",yieldUnits,")"),xlab=paste0("PREDICTED YIELD (",yieldUnits,")"))
  lines(pyldobs,pyldobs, col=2)
  
  # mass residual plot
  plot(ppredict,pResids,log="x",pch=1,main="Residuals vs Predicted \nLoad",
       ylab="LOG RESIDUAL",xlab=paste0("PREDICTED LOAD (",loadUnits,")"))
  abline(h=0,col=2)
  
  # yield residual plot
  plot(pyldpredict,pResids,log="x",pch=1,main="Residuals vs Predicted \nYield",
       ylab="LOG RESIDUAL",xlab=paste0("PREDICTED YIELD (",yieldUnits,")"))
  abline(h=0,col=2)
  
  
  par(mfrow=c(2,2), pch=diagnosticPlotPointStyle, cex = diagnosticPlotPointSize)  # 4 plots on one page
  
  # Residual plots
  boxplot(pResids,ylab="LOG RESIDUAL",main="MODEL SIMULATION PERFORMANCE \nResiduals")
  
  # Obs-Pred ratio boxplot
  boxplot(pratio.obs.pred,ylim = c(0,2),ylab="RATIO OBSERVED TO PREDICTED",
          main="MODEL SIMULATION PERFORMANCE \nObserved / Predicted Ratio")
  
  # Normality probability plot
  qqnorm(pResids,ylab="Log Residuals")
  qqline(pResids, col = 2)
  
  # Squared residuals vs predicted
  Resids2 <- pResids**2
  plot(ppredict,Resids2,log="xy",pch=1,main="Squared Residuals vs Predicted Load",
       ylab="SQUARED LOG RESIDUALS",xlab=paste0("PREDICTED LOAD (",loadUnits,")"))
  lwresids <- lowess(ppredict,Resids2, f = 0.5, iter = 3)
  lwy <- lwresids$y
  lwx <- lwresids$x
  lines(lwx,lwy,col=2)
  
  # Plots of the Obs-Pred ratio vs. the area-weighted mean values of the explanatory variables 
  #   for the incremental areas between calibration sites
  par(mfrow=c(1,1), pch=diagnosticPlotPointStyle, cex = diagnosticPlotPointSize)  # 1 plots on one page
  if(!is.na(Cor.ExplanVars.list)){ 
    for (i in 1:length(Cor.ExplanVars.list$names)) {
      if(min(Cor.ExplanVars.list$cmatrixM_all[,i])<0 | max(Cor.ExplanVars.list$cmatrixM_all[,i])<0) {
        plot(Cor.ExplanVars.list$cmatrixM_all[,i],pratio.obs.pred,pch=1,
             main=bquote(paste("Observed to Predicted Ratio vs Area-Weighted Explanatory Variable \nFor Incremental Areas between Calibration Sites; Variable Name = ",.(Cor.ExplanVars.list$names[i]) )),
             ylab="RATIO OBSERVED TO PREDICTED",xlab=paste0("AREA-WEIGHTED EXPLANATORY VARIABLE (",Cor.ExplanVars.list$names[i],")"))
      } else {
        plot(Cor.ExplanVars.list$cmatrixM_all[,i],pratio.obs.pred,log="x",pch=1,
             main=bquote(paste("Observed to Predicted Ratio vs Area-Weighted Explanatory Variable \nFor Incremental Areas between Calibration Sites; Variable Name = ",
                               .(Cor.ExplanVars.list$names[i]) )),
             ylab="RATIO OBSERVED TO PREDICTED",xlab=paste0("AREA-WEIGHTED EXPLANATORY VARIABLE (",Cor.ExplanVars.list$names[i],")"))
      }
    }
  }
  
  ##########################
  # Diagnostics for Ratio by class (one plot per page)
  
  # sitedata.demtarea.class regions
  #for (k in 1:length(classvar)) {
  par(mfrow=c(1,1), pch=diagnosticPlotPointStyle, cex = diagnosticPlotPointSize)  # 1 plots on one page
  vvar <- sitedata.demtarea.class
  boxplot(pratio.obs.pred ~ vvar,log="y",main="Ratio Observed to Predicted by Deciles",
          xlab="Upper Bound for Total Drainage Area Deciles (km2)",ylab="Observed to Predicted Ratio")
  abline(h = 1, col = "red", lwd = 1) 
  #}
  
  # "classvar" regions
  if (classvar!="sitedata.demtarea.class"){
    for (k in 1:length(classvar)) {
      par(mfrow=c(1,1), pch=diagnosticPlotPointStyle, cex = diagnosticPlotPointSize)  # 1 plots on one page
      vvar <- as.numeric(eval(parse(text=paste("sitedata$",classvar[k],sep="") )))
      boxplot(pratio.obs.pred ~ vvar,log="y",main="Ratio Observed to Predicted",
              xlab=classvar[k],ylab="Observed to Predicted Ratio")
      abline(h = 1, col = "red", lwd = 1) 
    }}
  
  # 'classvar2" decile boxplots
  if(!is.na( class_landuse[1])){
    for (k in 1:length(classvar2)) {
      par(mfrow=c(1,1), pch=diagnosticPlotPointStyle, cex = diagnosticPlotPointSize)  # 1 plots on one page
      vvar <- as.numeric(eval(parse(text=paste("sitedata.landuse$",classvar2[k],sep="") )))
      iprob<-10
      chk <- unique(quantile(vvar, probs=0:iprob/iprob))
      chk1 <- 11 - length(chk)
      if(chk1 == 0) {
        qvars <- as.integer(cut(vvar, quantile(vvar, probs=0:iprob/iprob), include.lowest=TRUE)) 
        avars <- quantile(vvar, probs=0:iprob/iprob)
        qvars2 <- numeric(length(qvars))
        for (j in 1:10) {
          for (i in 1:length(qvars)){
            if(qvars[i] == j) {
              qvars2[i] <- round(avars[j+1],digits=0)
            }
          }
        }
        xxlab <- paste("Upper Bound for ",classvar2[k],sep="")  
        boxplot(pratio.obs.pred ~ qvars2,log="y",main="Ratio Observed to Predicted by Deciles",
                xlab=xxlab,ylab="Observed to Predicted Ratio")
        abline(h = 1, col = "red", lwd = 1) 
      } else {  # non-unique classes
        plot(vvar,pratio.obs.pred,log="y",main="Ratio Observed to Predicted",
             xlab=classvar2[k],ylab="Observed to Predicted Ratio")
        abline(h = 1, col = "red", lwd = 1) 
      }
    } # end 'classvar2' loop
  }
  
  ##################################################
  # Diagnostics by CLASS (contiguous geographic units)
  
  for (i in 1:length(grp)) {
    plotmrb(grp[i],class[,1],Obs,ppredict,pyldobs,pyldpredict,pResids)
  }
  
  
  #################
  # Residual MAPS
  #################
  
  
  # Setup GEOLINES basemap, if available
  
  #8.8.17 if(!is.na(LineShapeGeo)) {
  if(existGeoLines==TRUE) { 
    
    #add text explanation 11.3.17
    par(mfrow=c(1,1))
    strExplanation<-paste("
        The maps include:
        -Log residuals, based on monitoring conditioned predictions 
           (i.e., Model Estimation Log Residuals)\n
        -Log residuals, based on the unconditioned predictions 
           (i.e., Model Simulation Log Residuals)\n
        -Standardized residuals based on the monitoring conditioned predictions\n
        -Ratio of observed to predicted loads for the conditioned predictions 
           (i.e., Model Estimation Ratio)\n
        -Ratio of observed to predicted load for the unconditioned predictions 
           (i.e., Model Simulation Ratio) ")
    gplots::textplot(strExplanation,valign="top", cex=0.7, halign="left")
    title("Maps of Model Residuals and Observed to Predicted Ratios\n for the Calibration Sites")
    
    mapdata <- data.frame(xlat,xlon,Resids,ratio.obs.pred)
    diagnosticMaps("Resids",mapdata,GeoLines,
                   c("threshold","all"),"Model Estimation Log Residuals",mapping.input.list)
    ##########################  
    
    # Single map of prediction residuals (8 classes) - no monitoring adjustment
    pmapdata <- data.frame(xlat,xlon,pResids,pratio.obs.pred)
    diagnosticMaps("pResids",pmapdata,GeoLines,
                   c("threshold","all"),"Model Simulation Log Residuals",mapping.input.list)
    
    
    
    #Map Standardized Residuals
    smapdata <- data.frame(xlat,xlon,standardResids)
    diagnosticMaps("standardResids",smapdata,GeoLines,
                   c("threshold","all"),"Model Estimation Standardized Residuals",mapping.input.list)
    
    
    ##########################
    # Map Ratios observed to predicted 
    diagnosticMaps("ratio.obs.pred",mapdata,GeoLines,
                   c("threshold","all"),"Model Estimation Obs/Pred Ratio",mapping.input.list)
    
    ##########################
    # Map Ratios observed to predicted for no monitoring adjustment
    diagnosticMaps("pratio.obs.pred",pmapdata,GeoLines,
                   c("threshold","all"),"Model Simulation Obs/Pred Ratio",mapping.input.list)
    
    ########################
    #Map of predicted studentized residuals
    
    
  }  # end check for existence of line map
  
  dev.off()  # shuts down current graphics device
  graphics.off()  # shuts down all open graphics devices
  
  
  #output siteAttr shapefile
  if (outputERSImaps[4]=="yes"){
    siteAttrshape<-data.frame(waterid = sitedata$waterid,
                              originalWaterid = sitedata$waterid_for_RSPARROW_mapping,
                              xlat,xlon)
    for (s in 1:length(map_siteAttributes.list)){
      if (length(names(sitedata)[which(names(sitedata)==map_siteAttributes.list[s])])!=0){
        siteAttr<-eval(parse(text= paste("data.frame(",map_siteAttributes.list[s],"=sitedata$",map_siteAttributes.list[s],")",sep="")))
        siteAttrshape<-data.frame(siteAttrshape,siteAttr)
        names(siteAttrshape)[length(siteAttrshape)]<-map_siteAttributes.list[s]
      }
    }
    
    
    
    siteAttrshape<-SpatialPointsDataFrame(siteAttrshape[,c("xlon","xlat")],siteAttrshape[,which(!names(siteAttrshape) %in% c("xlat","xlon"))],proj4string=CRS(CRStext))
    
    if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""))){
      dir.create(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""),showWarnings = FALSE)
    }
    if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep,sep=""))){
      dir.create(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep,sep=""),showWarnings = FALSE)
    }
    
    maptools::writeSpatialShape(siteAttrshape,paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep,"siteAttrshape",sep=""))
    cat(showWKT(proj4string(siteAttrshape)),file=paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep,"siteAttrshape.prj",sep="")) 
  }
  
  #output residuals shapefile
  if (outputERSImaps[3]=="yes"){
    Resids <- estimate.list$sparrowEsts$resid
    Obsyield <- Obs / sitedata$demtarea
    
    predictYield <- predict / sitedata$demtarea
    leverage<-estimate.list$JacobResults$leverage
    boot_resid<-estimate.list$JacobResults$boot_resid
    tiarea<-Csites.weights.list$tiarea
    weight<-Csites.weights.list$weight
    origWaterid<-sitedata$waterid_for_RSPARROW_mapping
    
    dd <- data.frame(sitedata,origWaterid,Obs,predict,Obsyield,predictYield,Resids,standardResids,leverage,boot_resid,weight,tiarea,pResids,ratio.obs.pred,pratio.obs.pred,xlat, xlon)
    keeps <- c("waterid","origWaterid","demtarea","rchname","station_id","station_name","staid",classvar[1],"Obs",
               "predict","Obsyield","predictYield","Resids","standardResids","leverage","boot_resid","weight","tiarea","pResids","ratio.obs.pred","pratio.obs.pred","xlat","xlon")
    residShape <- dd[keeps]
    
    if (length(na.omit(add_vars))!=0){
      add_data<-data.frame(sitedata[,which(names(sitedata) %in% add_vars)])
      if (length(add_vars)==1){
        names(add_data)<-add_vars
      }
      residShape<-cbind(residShape,add_data)
    }
    
    residShape <-SpatialPointsDataFrame(residShape[,c("xlon","xlat")],residShape[,which(!names(residShape) %in% c("xlat","xlon"))],proj4string=CRS(CRStext))
    
    if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""))){
      dir.create(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""),showWarnings = FALSE)
    }
    if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep,sep=""))){
      dir.create(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep,sep=""),showWarnings = FALSE)
    }
    
    maptools::writeSpatialShape(residShape,paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep,"residShape",sep=""))
    cat(showWKT(proj4string(residShape)),file=paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep,"residShape.prj",sep="")) 
    
  }
  
  
}#end function

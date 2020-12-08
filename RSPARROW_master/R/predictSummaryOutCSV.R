#'@title predictSummaryOutCSV
#'@description Calculates and outputs to the ~/estimate/(run_id)_summary_predictions.csv file 
#'            the percentile summaries of reach predictions of load, yield, concentration, and delivery 
#'            fraction for the control settings if_estimate<-"yes" and if_predict<-"yes", and outputs percentile 
#'            summaries for the yields  by predominant land-use type specified in the 'class_landuse' control setting. \\cr \\cr
#'Executed By: estimate.R \\cr
#'Executes Routines: \\itemize\{\\item calcClassLandusePercent.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param estimate.input.list named list of sparrow_control settings: ifHess, s_offset, 
#'                           NLLS_weights,if_auto_scaling, and if_mean_adjust_delivery_vars
#'@param estimate.list list output from `estimate.R`
#'@param predict.list archive with all load and yield prediction variables to provide for 
#'                    the efficient access and use of predictions in subsequent execution 
#'                    of the parametric bootstrap predictions and uncertainties, mapping, 
#'                    and scenario evaluations.  For more details see documentation Section 
#'                    5.3.1.5
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'       parmMin>=0) | parmType!="SOURCE")`
#'@param subdata data.frame input data (subdata)
#'@param class.input.list list of control settings related to classification variables


predictSummaryOutCSV <- function(file.output.list,estimate.input.list,
                                 SelParmValues,estimate.list,predict.list,
                                 subdata,class.input.list) {
  
  
  #################################################
  
  
  # create global variable from list names (JacobResults)
  # 'oEstimate' containing the estimated mean parameters for all non-constant and constant parameters
  # 'Parmnames' list of variable names 
  # create global variable from list names
  # create global variable from SelParmValues
  unPackList(lists = list(JacobResults = estimate.list$JacobResults,
                          SelParmValues = SelParmValues,
                          estimate.input.list = estimate.input.list,
                          predict.list = predict.list,
                          file.output.list = file.output.list,
                          class.input.list = class.input.list),
             parentObj = list(NA,
                              NA,
                              NA,
                              NA,
                              NA,
                              NA)) 
  
  # Transfer loads to matrix and compute summary percentiles
  omatrix <- matrix(0,nrow=nrow(predmatrix),ncol=(5+length(srcvar)))
  
  #   deliv_frac                 Fraction of total load delivered to terminal reach
  for (i in 1:length(oparmlist)) {
    if(oparmlist[i] == "deliv_frac"){
      omatrix[,5] <- predmatrix[,i]
    }
  }
  
  #   Concentration              Concentration based on decayed total load and discharge
  for (i in 1:length(oyieldlist)) {
    if(oyieldlist[i] == "concentration"){
      omatrix[,4] <- yldmatrix[,i]
    }
  }
  
  #   pload_total                Total load (fully decayed)
  for (i in 1:length(oparmlist)) {
    if(oparmlist[i] == "pload_total"){
      omatrix[,1] <- predmatrix[,i] 
    }
  }
  
  #   yield_total                Total yield (fully decayed)
  for (i in 1:length(oyieldlist)) {
    if(oyieldlist[i] == "yield_total"){
      omatrix[,2] <- yldmatrix[,i] 
    }
  }
  
  #   yield_inc                  Total incremental yield delivered to streams
  for (i in 1:length(oyieldlist)) {
    if(oyieldlist[i] == "yield_inc"){
      omatrix[,3] <- yldmatrix[,i]
    }
  }
  
  # Number of fixed output metrics
  nfixed <- 5
  
  #   share_inc_(sources)        Source share for incremental load (percent)
  for (j in 1:length(srcvar)){
    dname <- paste0("share_inc_",srcvar[j])  
    for (i in 1:length(oparmlist)) {
      if(oparmlist[i] == dname){
        omatrix[,(nfixed+j)] <- predmatrix[,i]
      }
    }
  }
  name1 <- paste0("Load Total (",loadUnits,")")
  name2 <- paste0("Yield Total (",yieldUnits,")")
  name3 <- paste0("Yield Incremental (",yieldUnits,")")
  name4 <- paste0("Flow-Weighted Concentration (",ConcUnits,")")
  sumlist <- c(name1,name2,name3,name4,"Delivery Fraction")
  
  for (i in 1:length(srcvar)){
    sumlist[nfixed+i] <- paste0("Incremental Share (%) ",srcvar[i])  
  }
  
  # Compute percentiles
  sstats <- matrix(0,nrow=(nfixed+length(srcvar)),ncol=9)
  for (i in 1:(nfixed+length(srcvar))){
    sstats[i,] <- quantile(round(omatrix[,i],digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97))
  }
  mstats <- matrix(0,nrow=(nfixed+length(srcvar)),12)
  for (i in 1:(nfixed+length(srcvar))){
    mstats[i,1] <- nrow(omatrix)
    mstats[i,2] <- round(mean(omatrix[,i]),digits=3)
    mstats[i,3] <- round(sd(omatrix[,i]),digits=3)
  }
  for (i in nfixed:12) {
    mstats[,i] <- sstats[,i-3]
  }
  headlist <- c("Number Watersheds","Mean","Standard Deviation","2.5th","10th","20th","30th","50th",
                "70th","80th","90th","97th")
  
  # Output summary predictions
  # prep for output to CSV
  outvars <- as.data.frame(mstats,sstats)
  rownames(outvars) <- sumlist
  colnames(outvars) <- headlist
  
  #calculate yields for user selected class_landuse_percent
  if (!is.na(class_landuse_percent) & !is.na(class_landuse)){
    classLandusePercent<-calcClassLandusePercent(subdata,class.input.list)
    #for each class_landuse cget yield add to outvars
    for (c in class_landuse){
      subWaterid<-classLandusePercent[which(classLandusePercent$landuse==c),]$waterid
      subYield<-yldmatrix[yldmatrix[,1] %in% subWaterid,]
      
      # Compute percentiles
      sstats<- matrix(quantile(round(subYield[,3],digits=3),c(0.025,0.1,0.2,0.3,0.5,0.7,0.8,0.9,0.97)),nrow=1)
      mstats <- matrix(0,nrow=1,ncol = 3)
      mstats[1,1] <- nrow(subYield)
      mstats[1,2] <- round(mean(subYield[,3]),digits=3)
      mstats[1,3] <- round(sd(subYield[,3]),digits=3)
      
      headlist <- c("Number Watersheds","Mean","Standard Deviation","2.5th","10th","20th","30th","50th",
                    "70th","80th","90th","97th")
      
      #make dataframe
      LUoutvars <- as.data.frame(matrix(c(mstats,sstats),nrow=1))
      rownames(LUoutvars) <- paste0(class_landuse_percent[which(class_landuse==c)],"%",c,"_",name2)
      colnames(LUoutvars) <- headlist 
      #bind to outvars
      outvars<-rbind(outvars,LUoutvars)
      
    }#end for each class_landuse
  }#end if class_landuse_percent
  
  fileout <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_summary_predictions.csv")
  fwrite(outvars,file=fileout,row.names=T,append=F,quote=F,col.names=TRUE,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")
  
  
}#end function


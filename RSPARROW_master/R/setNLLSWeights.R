#'@title setNLLSWeights
#'@description Assigns the user-defined weights to the system variable 'weight', used to 
#'            execute a weighted NLLS model estimation. Also calculates the variable 'tiarea' (sum of the 
#'            incremental drainage area of reaches located between monitoring sites), used to calculate weighting 
#'            expressions based on the size of the intervening areas between sites. \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'Executes Routines: \\itemize\{\\item assignIncremSiteIDs.R
#'             \\item errorOccurred.R
#'             \\item named.list.R
#'             \\item unPackList.R\} \\cr
#'@param NLLS_weights character string control setting to select regression weights
#'@param run_id character string control setting indicating the current model name
#'@param subdata data.frame input data (subdata)
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0
#'                & subdata$calsites==1), ]`. The object contains the dataDictionary 
#'                ‘sparrowNames’ variables, with records sorted in hydrological 
#'                (upstream to downstream) order (see the documentation Chapter 
#'                sub-section 5.1.2 for details)
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param minimum_reaches_separating_sites number indicating the minimum number of reaches 
#'       separating sites
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@return `Csites.weights.list` regression weights as proportional to incremental area size



setNLLSWeights <- function(NLLS_weights,run_id,subdata,sitedata,data_names,
                           minimum_reaches_separating_sites,batch_mode) {
  
  
  #############################################
  # create global variables for calculations
  unPackList(lists = list(datalstreq = data_names$sparrowName),
             parentObj = list(subdata = subdata)) 
  ############################################
  minnum <- minimum_reaches_separating_sites
  staidseq <- assignIncremSiteIDs(minnum,staid,waterid,tnode,fnode)   # call function to assign sites to reaches
  xx <- data.frame(staidseq,demiarea)
  count<-ddply(xx,.(staidseq), summarize, nirchs=length(staidseq))      # get count for unique staids
  count <- count[-1,]  # delete first row
  siteiarea<-ddply(xx,.(staidseq),summarize,tiarea=sum(demiarea))    # sum incr areas for unique staids
  siteiarea <- siteiarea[-1,]  # delete first row
  
  # Update SITEDATA, merge COUNT and SITEIAREA by staidseq  
  xx <- sitedata
  xx <- merge(xx,count,by="staidseq",all.y=FALSE,all.x=TRUE)  
  xx <- merge(xx,siteiarea,by="staidseq",all.y=FALSE,all.x=TRUE) 
  xx <- xx[with(xx,order(xx$hydseq)), ]   # resort dataframe by the original HYDSEQ order
  tiarea <- xx$tiarea
  
  # select user-specified weight
  if(NLLS_weights=="lnload" | NLLS_weights=="user") {  # values set in userModifyData.R function
    # "lnload" estimated in 'estimateWeightedErrors' function called from userModifyData.R
    weight <- sitedata$weight     # extract weight from userModifyData.R function  
    message("Storing weights for weighted NLLS...with 'NLLS_weights' setting option: ",NLLS_weights)
  } else {   
    weight<-1  # default setting where monitoring sites exist
  }    
  
  Csites.weights.list <- named.list(NLLS_weights,tiarea,count,weight)
  
  # Check for NA weights
  if(NLLS_weights != "default") {
    if(sum(ifelse(is.na(weight),1,0))>0 | is.null(weight)) {
      Csites.weights.list <- named.list(NLLS_weights,tiarea,count,weight)
      assign("Csites.weights.list",Csites.weights.list,envir = .GlobalEnv)
      cat("\n \n")
      message("ERROR: Missing values found for calibration sites in the system variable 'weight' \n that is used in the requested weighted estimation (see 'NLLS_weights' control setting). \n For the NLLS_weights option, the variable 'weight' must be computed in userModifyData and \n entered into the dataDictionary.csv as sparrowNames with FIXED varType. \nRUN EXECUTION TERMINATED.")
      if (batch_mode=="yes"){#if batch output message to log
        cat(" \nERROR: Missing values found for calibration sites in the system variable 'weight' \n that is used in the requested weighted estimation (see 'NLLS_weights' control setting) \n For the NLLS_weights option, the variable 'weight' must be computed in userModifyData and \n entered into the dataDictionary.csv as sparrowNames with FIXED varType. \nRUN EXECUTION TERMINATED.")
      }
      errorOccurred("setNLLSWeights.R",batch_mode)
    }
  }
  
  
  
  
  return(Csites.weights.list)
  
}#end function

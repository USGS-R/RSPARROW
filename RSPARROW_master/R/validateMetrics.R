#'@title validateMetrics
#'@description Computes all model performance and other summary metrics and diagnostics for 
#'            the estimated model for the validation sites, as reported in the ~/estimate/(run_id)_summary.txt 
#'            file.  \\cr \\cr
#'Executed By: estimate.R \\cr
#'Executes Routines: \\itemize\{\\item getVarList.R
#'             \\item named.list.R
#'             \\item unPackList.R
#'             \\item validateFevalNoadj.R\} \\cr
#'@param classvar character vector of user specified spatially contiguous discrete 
#'       classification variables from sparrow_control.  First element is reach classification variable.
#'@param estimate.list list output from `estimate.R`
#'@param dlvdsgn design matrix imported from design_matrix.csv
#'@param Vsites.list named list of sites for validation
#'@param yieldFactor numeric control setting to specify the yield conversion factor, computed 
#'       as `Yield = load / demtarea * yieldFactor`
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'       parmMin>=0) | parmType!="SOURCE")`
#'@param subdata data.frame input data (subdata)
#'@param vsitedata sitedata for validation. Calculated by `subdata[(subdata$vdepvar > 0), ]`
#'@param DataMatrix.list named list of 'data' and 'beta' matrices and 'data.index.list' 
#'                       for optimization
#'@return `validate.metrics.list` named list including the vANOVA.list and vMdiagnostics.list



validateMetrics <- function(classvar,estimate.list,dlvdsgn,Vsites.list,yieldFactor,
                            SelParmValues,subdata,vsitedata,DataMatrix.list) {
  
  
  # contiguous class variables by sites
  class <- array(0,dim=c(nrow=nrow(vsitedata),ncol=length(classvar))) 
  for (k in 1:length(classvar)) { 
    for (i in 1:nrow(vsitedata)) {
      class[i,k] <- as.numeric(eval(parse(text=paste0("vsitedata$",classvar[k],"[",i,"]"))))
    } 
  } 
  # contiguous class variables by reach
  classrch <- as.numeric(eval(parse(text=paste0("subdata$",classvar[1]))))  # used to compute RMSE by class
  
  data <- DataMatrix.list$data
  
  # create global variable from list names
  # transfer required variables to global environment from 'DataMatrix.list$data.index.list'
  # transfer required variables to global environment from SUBDATA
  unPackList(lists = list(data.index.list = DataMatrix.list$data.index.list,
                          SelParmValues = SelParmValues,
                          datalstCheck = c(as.character(getVarList()$varList),"vdepvar","vstaid")),
             parentObj = list(NA,
                              NA,
                              subdata = subdata)) 
  
  ##########################################################
  # SUMMARY PERFORMANCE METRICS FOR NO MONITORING ADJUSTMENT
  
  Estimate <- estimate.list$JacobResults$oEstimate[SelParmValues$betaconstant==0]
  pResids <- validateFevalNoadj(Estimate,vdepvar,
                                SelParmValues)
  
  # Obtain predicted, observed, residual values
  nreach <- length(DataMatrix.list$data[,1])
  numsites <- Vsites.list$vic
  npar <- length(Estimate)
  mobs <- numsites
  Obs <- numeric(numsites)
  xstaid <- numeric(numsites)
  tarea <- numeric(numsites)
  xlat <- numeric(numsites)
  xlon <- numeric(numsites)
  ppredict <- numeric(numsites)
  pyldobs <- numeric(numsites)
  pyldpredict <- numeric(numsites)
  pratio.obs.pred <- numeric(numsites)
  pxssemrb <- numeric(max(class[,1]))
  
  ins <- 0
  for (k in 1:nreach) {
    if (vdepvar[k] > 0) {
      ins <- ins+1
      obs <- vdepvar[k]                        # data[k,jdepvar]
      Obs[ins] <- vdepvar[k]                   # data[k,jdepvar]
      ppredict[ins] <- (exp(log(obs) - pResids[ins]))  
      pyldobs[ins] <- Obs[ins] / data[k,jtotarea] * yieldFactor
      pyldpredict[ins] <- ppredict[ins] / data[k,jtotarea] * yieldFactor
      pratio.obs.pred[ins] <- Obs[ins] / (ppredict[ins])
      
      xlat[ins] <- lat[k]
      xlon[ins] <- lon[k]
      xstaid[ins] <- vstaid[k]
      tarea[ins] <- data[k,jtotarea]
      
      # compute SSE by class
      if(class[ins] > 0) {
        pxssemrb[class[ins,1]] <- pxssemrb[class[ins,1]] + pResids[ins]^2
      }
    }                                 
  }  #  reach counter 
  pxssemrb[is.na(pxssemrb)] <- 0
  
  
  pSSE <- sum(pResids**2)
  pMSE <- pSSE / mobs
  pRMSE <- sqrt(pMSE)
  
  pRSQ <- 1 - pSSE / (sum(log(Obs)^2) - sum(log(Obs))^2/mobs)
  pRSQ_ADJ <- 1 - ((mobs - 1)/mobs)*(1 - pRSQ) 
  pRSQ_YLD <- 1 - pSSE / (sum(log(pyldobs)^2) - sum(log(pyldobs))^2/mobs)
  
  NSn <- 0
  NSd <- 0
  PBiasn <- 0
  PBiasd <- 0
  Obsmean <- mean(log(Obs))
  for (i in 1:mobs) {
    NSn <- NSn + (log(Obs[i]) - log(ppredict[i]))^2
    NSd <- NSd + (log(Obs[i]) - Obsmean)^2
    PBiasn <- PBiasn + (Obs[i] - ppredict[i])
    PBiasd <- PBiasd + Obs[i]
  }
  pNSeff <- 1 - (NSn / NSd)    # overall Nash-Sutcliffe model efficiency
  pPBias <- PBiasn / PBiasd * 100   # Percent bias
  
  classgrp <- table(class[,1])   # get labels
  xx <- as.data.frame(classgrp)  # convert table to dataframe...
  classgrp <- as.numeric(levels(xx$Var1)[xx$Var1]) 
  
  
  # Compute SSE and RMSE for class regions
  pSSEclass <- numeric(length(classgrp))
  ii <- 0
  for (k in 1:length(pxssemrb)) {
    if(pxssemrb[k] != 0) {
      ii <- ii+1
      pSSEclass[ii] <- pxssemrb[k]
    }
  }
  
  vANOVA.list <- named.list(mobs,pSSE,pMSE,pRMSE,pRSQ,pRSQ_ADJ,pRSQ_YLD,pNSeff,pPBias)
  vMdiagnostics.list <- named.list(Obs,xstaid,tarea,xlat,xlon,
                                   ppredict,pyldobs,pyldpredict,pratio.obs.pred,
                                   classgrp,pSSEclass,pResids)
  
  validate.metrics.list <- named.list(vANOVA.list,vMdiagnostics.list)
  
  
  
  return(validate.metrics.list)
  
}#end function

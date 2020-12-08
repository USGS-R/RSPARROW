#'@title estimateBootstraps
#'@description Executes all model tasks related to obtaining model estimation uncertainties 
#'            using parametric bootstrap methods, for the control setting if_boot_estimate<-"yes". \\cr \\cr
#'Executed By: controlFileTasksModel.R \\cr
#'Executes Routines: \\itemize\{\\item estimateFeval.R
#'             \\item getVarList.R
#'             \\item named.list.R
#'             \\item unPackList.R\} \\cr
#'@param iseed User specified initial seed for the bootstraps from sparrow_control
#'@param biters User specified number of parametric bootstrap iterations from sparrow_control
#'@param estimate.list list output from `estimate.R`
#'@param DataMatrix.list named list of 'data' and 'beta' matrices and 'data.index.list' 
#'                       for optimization
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'       parmMin>=0) | parmType!="SOURCE")`
#'@param Csites.weights.list regression weights as proportional to incremental area size
#'@param estimate.input.list named list of sparrow_control settings: ifHess, s_offset, 
#'                           NLLS_weights,if_auto_scaling, and if_mean_adjust_delivery_vars
#'@param dlvdsgn design matrix imported from design_matrix.csv
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@return `BootResults` data archive using the control setting `if_boot_estimate <- "yes"` for 
#'            use in subsequent execution of parametric bootstrap predictions. For more details see 
#'            documenation Section 5.2.6.2.



estimateBootstraps <- function(iseed,biters,estimate.list,
                               DataMatrix.list,SelParmValues,Csites.weights.list,
                               estimate.input.list,dlvdsgn,file.output.list){
  
  
  # create global variable from list names (JacobResults)
  # transfer required variables to global environment from SUBDATA
  unPackList(lists = list(JacobResults = estimate.list$JacobResults,
                          datalstCheck = as.character(getVarList()$varList),
                          file.output.list = file.output.list),
             parentObj = list(NA,
                              subdata = subdata,
                              NA))
  
  # execute parametric Monte Carlo sampling of model coefficients
  set.seed(iseed)
  
  # Check for esttype=="Estimated" coefficients to setup up parameter vector
  #   to be consistent with Hessian covariance matrix dimensions
  
  k<-0
  ndim <- sum(ifelse(esttype=="Estimated",1,0))
  sEstimate <- numeric(ndim)
  stype <- numeric(ndim)
  sMEstimate <- matrix(0,nrow=biters,ncol=ndim)
  sbmin <- numeric(ndim)
  sbmax <- numeric(ndim)
  for (i in 1:length(oEstimate)) {
    if(esttype[i]=="Estimated") {
      k<-k+1
      stype[k] <- btype[i]
      sEstimate[k] <- oEstimate[i]
      sbmin[k] <- bmin[i]
      sbmax[k] <- bmax[i]
    }
  }
  
  # Function for sampling coefficients and computing Jacobian matrix
  sEst <- function(sMestimate,i,sEstimate,sbmin,sbmax,DataMatrix.list,SelParmValues,Csites.weights.list,
                   estimate.input.list,dlvdsgn) {
    sMestimate[i,] <- sEstimate + H %*% rnorm(length(sEstimate),mean=0,sd=1)
    # Ensure that the coefficients are consistent with the parameter constraints
    sMestimate[i,] <- pmax(sbmin,sMestimate[i,])
    sMestimate[i,] <- pmin(sbmax,sMestimate[i,])
    
    # Jacobian function
    jacfun<-function(beta0) {
      Jac<-jacobian(estimateFeval,beta0,method="Richardson",
                    DataMatrix.list = DataMatrix.list,
                    SelParmValues = SelParmValues,
                    Csites.weights.list = Csites.weights.list,
                    estimate.input.list = estimate.input.list,
                    dlvdsgn = dlvdsgn)
    }
    
    # check for singular Jacobian matrix
    BetaEst <- sMestimate[i,]  # Thinned ("Estimated") coefficients as required for estimateFeval.R
    Jacob <- jacfun(BetaEst)
    ErrorSingular <- grep("singular",try({Jacob_inv <- solve(t(Jacob) %*% Jacob)},TRUE))
    
    if(length(ErrorSingular)>0) {
      message("    Singular Jacobian matrix found. Resampling model coefficients for iteration ",i)
      Recall(sMestimate,i,sEstimate,sbmin,sbmax) # run the function again
    } else {
      sEst.list <- named.list(sMestimate,Jacob_inv,Jacob,BetaEst)
      return(sEst.list)
    }
  }
  
  # Jacobian function
  jacfun<-function(beta0) {
    Jac<-jacobian(estimateFeval,beta0,method="Richardson",DataMatrix.list = DataMatrix.list)
  }
  
  
  # Run boot iterations to obtain leverage and 'mean_exp_weighted_error' for "Estimated" coefficients
  
  # Obtain coefficients with uncertainties that reflect variance-covariance between all coefficients
  # Coefficients are for thinned set of parameters, as required for estimateFeval.R
  
  cov2 <- estimate.list$HesResults$cov2   # coefficient variance from Hessian
  H <- decomp.cov(cov2) # decomp.cov returns a decomposition matrix H such that V <- H %*% t(H)
  sMestimate <- matrix(0,nrow=biters,ncol=ndim)
  bootmean_exp_weighted_error <- numeric(biters)
  
  BetaEst <- numeric(ndim)
  Resids <- matrix(0,nrow=biters,ncol=length(estimate.list$sparrowEsts$resid))
  boot_resids <- matrix(0,nrow=biters,ncol=length(estimate.list$sparrowEsts$resid))
  boot_lev <- matrix(0,nrow=biters,ncol=length(estimate.list$sparrowEsts$resid))
  for (iter in 1:biters) {
    message(" Sampling coefficients and computing bootstrap residuals for iteration ",iter)
    
    sEst.list <- sEst(sMestimate,iter,sEstimate,sbmin,sbmax,DataMatrix.list,SelParmValues,Csites.weights.list,
                      estimate.input.list,dlvdsgn)
    sMestimate[iter,] <- sEst.list$sMestimate[iter,]
    Jacob_inv <- sEst.list$Jacob_inv
    Jacob <- sEst.list$Jacob
    BetaEst <- sEst.list$BetaEst
    
    hlev <- rowSums( Jacob %*% Jacob_inv * Jacob)
    
    Resids[iter,] <- estimateFeval(BetaEst,
                                   DataMatrix.list,SelParmValues,Csites.weights.list,
                                   estimate.input.list,dlvdsgn)  # executed with monitoring load adjustment and thinned set of parameters
    boot_resid <- Resids[iter,] / sqrt(1 - hlev)
    boot_resids[iter,] <- boot_resid
    boot_lev[iter,] <- hlev
    if(max(hlev)>1) {
      bootmean_exp_weighted_error[iter] <- estimate.list$JacobResults$mean_exp_weighted_error
    } else {
      boot_weights <- rep(1,length(hlev))
      bootmean_exp_weighted_error[iter] <- (t(boot_weights) %*% exp(boot_resid)) / sum(boot_weights) 
    }
  }
  
  
  # Remap coefficients to full set of Estimated and Fixed coefficients,
  #  in form suitable for calculating predictions
  k<-0
  bEstimate <- matrix(0,nrow=biters,ncol=length(oEstimate))
  for (i in 1:length(oEstimate)) {
    if(esttype[i]=="Estimated") {
      k<-k+1
      bEstimate[,i] <- sMestimate[,k]
    } else { 
      # transfer "Fixed" coefficients
      bEstimate[,i] <- oEstimate[i]
    }
  }
  
  # store bootstrap parameter estimates in object as list and save
  
  BootResults <- named.list(bEstimate,bootmean_exp_weighted_error,boot_resids,boot_lev)
  objfile <- paste0(path_results,"estimate",.Platform$file.sep,run_id,"_BootBetaest")
  save(BootResults,file=objfile)
  assign("BootResults",BootResults,envir = .GlobalEnv)
  
  # output to CSV
  outvars <- data.frame(rep(1:biters),bEstimate,bootmean_exp_weighted_error)
  headlist <- character(length(Parmnames)+2)
  headlist[1] <- "biters"
  for (i in 1:length(Parmnames)){
    headlist[i+1] <- Parmnames[i]
  }
  headlist[i+2] <- "bootmean_exp_weighted_error"
  colnames(outvars) <- headlist
  fileout <- paste0(path_results,"estimate",.Platform$file.sep,run_id,"_bootbetaest.csv")
  fwrite(outvars,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  
  return(BootResults)
  
}#end function

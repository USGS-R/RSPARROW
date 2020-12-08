#'@title estimateNLLSmetrics
#'@description Computes all model performance and other summary metrics and diagnostics for 
#'            the estimated model for calibration sites, as reported in the ~/estimate/(run_id)_summary.txt 
#'            file.  \\cr \\cr
#'Executed By: estimate.R \\cr
#'Executes Routines: \\itemize\{\\item eigensort.R
#'             \\item errorOccurred.R
#'             \\item estimateFeval.R
#'             \\item estimateFevalNoadj.R
#'             \\item getVarList.R
#'             \\item named.list.R
#'             \\item unPackList.R\} \\cr
#'@param if_estimate yes/no indicating whether or not estimation is run
#'@param if_estimate_simulation character string setting from sparrow_control.R indicating 
#'       whether estimation should be run in simulation mode only.
#'@param if_sparrowEsts value of 1 `if_estimate_simulation<-'no'` indicating calculation of 
#'       Jacobian diagnostics and 0 `if_estimate_simulation<-'yes'` indicating no calculation of Jacobian diagnostics
#'@param sparrowEsts list object contained in estimate.list `if_estimate<-'yes'`.  For more 
#'       details see documentation Section 5.2.4.4.
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param classvar character vector of user specified spatially contiguous discrete 
#'       classification variables from sparrow_control.  First element is reach classification variable.
#'@param dlvdsgn design matrix imported from design_matrix.csv
#'@param Csites.weights.list regression weights as proportional to incremental area size
#'@param estimate.input.list named list of sparrow_control settings: ifHess, s_offset, 
#'                           NLLS_weights,if_auto_scaling, and if_mean_adjust_delivery_vars
#'@param Csites.list list output from `selectCalibrationSites.R` modified in `startModelRun.R`
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & 
#'       ((parmType=="SOURCE" & parmMin>=0) | parmType!="SOURCE")`
#'@param subdata data.frame input data (subdata)
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0
#'                & subdata$calsites==1), ]`. The object contains the dataDictionary 
#'                ‘sparrowNames’ variables, with records sorted in hydrological 
#'                (upstream to downstream) order (see the documentation Chapter 
#'                sub-section 5.1.2 for details)
#'@param DataMatrix.list named list of 'data' and 'beta' matrices and 'data.index.list' 
#'                       for optimization
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@return `estimate.metrics.list` named list of summary metrics. For more details see 
#'            documentation section 5.2.4



estimateNLLSmetrics <- function(if_estimate,if_estimate_simulation,if_sparrowEsts,sparrowEsts,
                                file.output.list,classvar,dlvdsgn,
                                Csites.weights.list,estimate.input.list,
                                Csites.list,SelParmValues,subdata,sitedata,DataMatrix.list,
                                batch_mode) {
  
  
  exit <- function() {
    .Internal(.invokeRestart(list(NULL, NULL), NULL))
  }
  
  unPackList(lists = list(file.output.list = file.output.list,
                          estimate.input.list = estimate.input.list),
             parentObj = list(NA,NA))     
  
  message("Computing NLLS metrics...")
  
  
  # contiguous class variables by sites
  class <- array(0,dim=c(nrow=nrow(sitedata),ncol=length(classvar))) 
  for (k in 1:length(classvar)) { 
    for (i in 1:nrow(sitedata)) {
      class[i,k] <- as.numeric(eval(parse(text=paste0("sitedata$",classvar[k],"[",i,"]"))))
    } 
  } 
  
  # contiguous class variables by reach
  classrch <- as.numeric(eval(parse(text=paste0("subdata$",classvar[1]))))  # used to compute RMSE by class
  
  
  # Obtain predicted, observed, residual values
  nreach <- length(DataMatrix.list$data[,1])
  numsites <- Csites.list$nMon
  npar <- length(sparrowEsts$coefficients)
  mobs <- numsites
  Obs <- numeric(numsites)
  predict <- numeric(numsites)
  yldobs <- numeric(numsites)
  yldpredict <- numeric(numsites)
  xstaid <- numeric(numsites)
  tarea <- numeric(numsites)
  ratio.obs.pred <- numeric(numsites)
  xlat <- numeric(numsites)
  xlon <- numeric(numsites)
  
  
  data <- DataMatrix.list$data
  
  unPackList(lists = list(SelParmValues = SelParmValues,
                          data.index.list = DataMatrix.list$data.index.list,
                          datalstCheck = as.character(getVarList()$varList)),
             parentObj = list(NA,
                              NA,
                              subdata = subdata))
  
  
  xssemrb <- numeric(max(class[,1]))
  
  Resids <- sparrowEsts$resid
  ins <- 0
  for (k in 1:nreach) {
    if (data[k,jdepvar] > 0) {
      ins <- ins+1
      obs <- data[k,jdepvar]
      Obs[ins] <- data[k,jdepvar]
      predict[ins] <- (exp(log(obs) - Resids[ins]))  
      yldobs[ins] <- Obs[ins] / data[k,jtotarea] * yieldFactor
      yldpredict[ins] <- predict[ins] / data[k,jtotarea] * yieldFactor
      xstaid[ins] <- staid[k]
      tarea[ins] <- data[k,jtotarea]
      ratio.obs.pred[ins] <- Obs[ins] / (predict[ins])
      xlat[ins] <- lat[k]
      xlon[ins] <- lon[k]
      
      # compute SSE by class regions
      if(class[ins,1] > 0) {
        xssemrb[class[ins,1]] <- xssemrb[class[ins,1]] + Resids[ins]^2
      }
    }                                 
  }  #  reach counter 
  xssemrb[is.na(xssemrb)] <- 0
  
  ################################################
  # COMPUTE SUMMARY STATISTICS (ONLY ESTIMATION)
  
  SSE <- sum(Resids^2)
  DF <- mobs-npar
  MSE <- SSE / DF
  RMSE <- sqrt(MSE)
  
  RSQ <- 1 - SSE / (sum(log(Obs)^2) - sum(log(Obs))^2/mobs)
  RSQ_ADJ <- 1 - ((mobs - 1)/DF)*(1 - RSQ) 
  RSQ_YLD <- 1 - SSE / (sum(log(yldobs)^2) - sum(log(yldobs))^2/mobs)
  
  NSn <- 0
  NSd <- 0
  PBiasn <- 0
  PBiasd <- 0
  Obsmean <- mean(log(Obs))
  for (i in 1:mobs) {
    NSn <- NSn + (log(Obs[i]) - log(predict[i]))^2
    NSd <- NSd + (log(Obs[i]) - Obsmean)^2
    PBiasn <- PBiasn + (Obs[i] - predict[i])
    PBiasd <- PBiasd + Obs[i]
  }
  NSeff <- 1 - (NSn / NSd)    # overall Nash-Sutcliffe model efficiency
  PBias <- PBiasn / PBiasd * 100   # Percent bias
  
  ####################################################
  
  singularMessage <- "SINGULAR (ILL-CONDITIONED) HESSIAN MATRIX FOUND.
   MODEL ESTIMATION SUMMARY METRICS ARE NOT OUTPUT. 
   RUN EXECUTION TERMINATED.
   
   A SINGULARITY CONDITION INDICATES THAT AT LEAST ONE OF THE EXPLANATORY VARIABLES IS A
   LINEAR COMBINATION OF ONE OR MORE OF THE OTHER EXPLANATORY VARIABLES. 
   
   THESE CONDITIONS CAN BE CAUSED BY: 
   (1) A SMALL ESTIMATED COEFFICIENT OR INITIAL PARAMETER VALUE;
   (2) PROBLEMS WITH THE EXPLANATORY DATA, INCLUDING A CONSTANT EXPLANATORY VARIABLE 
   OR TWO OR MORE EXPLANATORY VARIABLES THAT SUM TO A CONSTANT OR ARE IDENTICAL
   
   USERS SHOULD CHECK THE EXPLANATORY VARIABLES FOR CONSTANT OR IDENTICAL VALUES. 
   RE-ESTIMATE THE MODEL AFTER ADJUSTING SMALL INITIAL PARAMETER VALUES OR
   AFTER ELIMINATING VARIABLES WITH SMALL NEAR-ZERO COEFFICIENTS."
  
  
  ####################################################  
  # Obtain SEs for coefficients using Jacobian
  Estimate <- sparrowEsts$coefficients                 
  
  
  SEj <- numeric(length(Estimate))  
  
  if (if_estimate == "yes" & if_estimate_simulation == 'no') {
    
    # remove parameters that hit min or max boundary, or are too close to zero (added 7-29-2016)
    k <- 0
    for (i in 1:npar) { 
      if ((Estimate[i] != sparrowEsts$betamn[i] & Estimate[i] != sparrowEsts$betamx[i])) { 
        k <- k+1 
      } 
    } 
    if (k != npar) {
      tJTJ <- array(0,dim=c(k,k))
      tEstimate <- numeric(k)
      tjacobian <- array(0,dim=c(mobs,k))
      ii <- 0
      for (i in 1:npar) {
        if ((Estimate[i] != sparrowEsts$betamn[i] & Estimate[i] != sparrowEsts$betamx[i])) {
          ii <- ii+1
          tEstimate[ii] <- Estimate[i]
          tjacobian[,ii] <- sparrowEsts$jacobian[,i]
        }
      }
      tJTJ <- crossprod(tjacobian)   # crossproduct of jacobian and its transpose
      
      ErrorSolve <- grep("singular",try({covmat <- MSE * solve(tJTJ)},TRUE))
      if(length(ErrorSolve)>0) {
        if(ErrorSolve==1) {
          
          cat("\n \n")
          message(singularMessage)
          
          Message <- "ERROR"
          JacobResults <- named.list(Message)
          estimate.metrics.list <- named.list(JacobResults)
          
          errorOccurred("estimateNLLSmetrics.R",batch_mode)
        }
      }
      
      tSEj <- sqrt(diag(covmat))
      
      ii <- 0
      for (i in 1:npar) {
        if ((Estimate[i] != sparrowEsts$betamn[i] & Estimate[i] != sparrowEsts$betamx[i]))  {
          ii <- ii+1
          SEj[i] <- tSEj[ii]
        } else {
          SEj[i] <- 0
        }
      } 
    } else {
      JTJ <- crossprod(sparrowEsts$jacobian)   # crossproduct of jacobian and its transpose
      
      ErrorSolve <- grep("singular",try({covmat <- MSE * solve(JTJ)},TRUE))
      if(length(ErrorSolve)>0) {
        if(ErrorSolve==1) {
          
          cat("\n \n")
          message(singularMessage)
          
          Message <- "ERROR"
          JacobResults <- named.list(Message)
          estimate.metrics.list <- named.list(JacobResults)
          
          errorOccurred("estimateNLLSmetrics.R",batch_mode)
        }
      }
      
      
      SEj <- sqrt(diag(covmat))
    }
    
    Tj <- Estimate / SEj
    pTj <- 2*(1-pt(abs(Tj),DF))
    
    
    #####################################################
    # Leverage statistics  (dim object)
    if(k != npar) {
      Jacob <- tjacobian
      Est <- tEstimate
    } else {
      Jacob <- sparrowEsts$jacobian                   # equivalent to the SAS IML matrix 'g'
      Est <- Estimate
    }
    Jacob_inv <- solve(t(Jacob) %*% Jacob)
    leverage <- rowSums( Jacob %*% Jacob_inv * Jacob)
    
    #  Derivation of Variance Inflation Factors, and X`X correlation matrix, eigenvalues and eigenvectors
    cJacob <- scale(Jacob,center=TRUE,scale=TRUE)  # center by subtracting mean and scaling by SD
    cJacob2 <- Jacob[,1:length(Est)]          # no centering allowed (SAS centering function needs checking)
    cJ <- t(cJacob2) %*% cJacob2   # compute the covariances
    c2 <- diag(diag(cJ)^(-0.5),nrow=length(Est),ncol=length(Est)) %*% cJ %*% diag(diag(cJ)^(-0.5),nrow=length(Est),ncol=length(Est))  # compute the correlation matrix
    
    # Initialize the VIF and eigenvalue vectors and corr and eigenvector matrices 
    tvif <- diag( solve(c2) )
    corr <- rbind( t(tvif), c2)    # vertical concatenation of two matrices
    Eigen <- eigen(c2)
    Eigen <- eigensort(Eigen)  # sort values (and corresponding vectors) in descending order
    te_val <- Eigen$values
    te_vec <- Eigen$vectors
    e_val_spread <- max(Eigen$values) / min(Eigen$values)
    
    # reset matrices to missing parameters
    e_vec <- array(0,dim=c(npar,npar))
    e_val <- numeric(npar)
    vif <- numeric(npar)
    ii <- 0
    for (i in 1:npar) {
      if ((Estimate[i] != sparrowEsts$betamn[i] & Estimate[i] != sparrowEsts$betamx[i])) {
        ii <- ii+1
        vif[i] <- tvif[ii]
        e_val[i] <- te_val[ii]
        jj <- 0
        for (j in 1:npar) {
          if ((Estimate[j] != sparrowEsts$betamn[j] & Estimate[j] != sparrowEsts$betamx[j])) { 
            jj <- jj+1
            e_vec[i,j] <- te_vec[ii,jj]
          }
        }
      }
    }
    e_vec <- rbind(t(e_val),e_vec)
    
    
    # Create errors for bootstrapping. The weighted_resid includes the square root of the observation weight and the bootstrap
    # occurance weight. h_lev includes the number of occurrences in the bootstrap sample. Therefore, to remove the occurrence
    # factor it is necessary to substitute the maximum of the boot_weight or 1 in place of the 1 in the denominator term. This
    # effectively removes the boot_weight from weighted_resid. 
    weights <- Csites.weights.list$weight
    standardResids <- Resids * sqrt(weights / (RMSE^2 * (1 - leverage)))
    boot_resid <- Resids / sqrt(1 - leverage)
    boot_weights <- rep(1,length(leverage))
    mean_exp_weighted_error <- (t(boot_weights) %*% exp(boot_resid)) / sum(boot_weights) 
    
    CooksD <- ((Resids/(1-leverage))^2 * leverage) / (npar * RMSE^2)   # Helsel and Hirsch (2002)
    CooksDpvalue <- 1-pf(CooksD,(npar+1),DF)   # Cook's D p value
    
    leverageCrit <- (3*npar)/mobs   # high leverage critical value 
    
    # Residual normality metrics and test
    x1 <- sort(Resids)
    x2 <- sort(rnorm(n=length(leverage),mean=0,sd=1))
    ppcc <- cor(x1,x2,method="pearson") 
    shap <- shapiro.test(Resids)
    shap.p <- shap$p.value
    shap.test <- unname(shap$statistic)
    
    #####################################################
    # transfer estimated parameters into complete parameter vector
    
    parmtotal <- length(srcvar) + length(dlvvar) + length(decvar) + length(resvar) + length(othervar)
    oParmnames <- noquote(c(srcvar,dlvvar,decvar,resvar,othervar))
    
    ndeliv <- length(dlvvar)
    nsrc <- length(srcvar)
    
    Parmnames <- character(parmtotal)
    oEstimate <- numeric(parmtotal)
    odesign <- matrix(0,nrow=nsrc,ncol=ndeliv)
    Beta.inital <- numeric(parmtotal)
    bmin <- numeric(parmtotal)
    bmax <- numeric(parmtotal)
    
    oSEj <- numeric(parmtotal)
    oTj <- numeric(parmtotal)
    opTj <- numeric(parmtotal)
    oVIF <- numeric(parmtotal)
    esttype <- character(parmtotal)
    btype <- character(parmtotal)
    oNames <- character(length(beta0))
    
    # Transfer parameter values
    j <- 0
    kparm <- 0
    for (i in 1:bcols) {
      if(betaconstant[i] == 0) { 
        j <- j+1
        kparm <- kparm+1
        Parmnames[j] <- oParmnames[i]
        
        oEstimate[j] <- Estimate[kparm]
        
        if(i <= nsrc & ndeliv > 0) { odesign[j,] <- dlvdsgn[i,] }
        Beta.inital[j] <- beta0[i]
        bmin[j] <- sparrowEsts$betamn[kparm]
        bmax[j] <- sparrowEsts$betamx[kparm]
        oSEj[j] <- SEj[kparm]
        oTj[j] <- Tj[kparm]
        opTj[j] <- pTj[kparm]
        oVIF[j] <- vif[kparm]
        if(Estimate[kparm] != 0) {
          esttype[j] <- "Estimated"
        } else {
          esttype[j] <- "Fixed"  # condition where parameter is zero (equals min or max boundary)
        }
        btype[j] <- betatype[i]
        
        oNames[kparm] <- oParmnames[i]    # names for output with parameter correlations and Eigenvectors
        
      } else {                     # non-estimated (constant) parameters
        j <- j+1
        Parmnames[j] <- oParmnames[i]
        oEstimate[j] <- beta0[i]  # SelParmValues object
        if(i <= nsrc & ndeliv > 0) {odesign[j,] <- dlvdsgn[i,]}
        Beta.inital[j] <- beta0[i]
        bmin[j] <- betamin[i]  # SelParmValues object
        bmax[j] <- betamax[i]  # SelParmValues object
        oSEj[j] <- 0
        oTj[j] <- NA
        opTj[j] <- NA
        oVIF[j] <- NA
        esttype[j] <- "Fixed"
        btype[j] <- betatype[i]    # SelParmValues object
        
        # bscale[j] <- bsrcscale[i]  # SelParmValues object    (DELETE)
        
      }
    }
    
  }  # end if_estimate check
  
  ##############################
  # Obtain CLASS region numbers
  classgrp <- table(class[,1])   # get labels
  xx <- as.data.frame(classgrp)  # convert table to dataframe...
  classgrp <- as.numeric(levels(xx$Var1)[xx$Var1])  # convert factor levels to numeric values
  
  
  # Compute SSE and RMSE for CLASS regions
  SSEclass <- numeric(length(classgrp))
  RMSEnn <-  numeric(length(classgrp))
  ii <- 0
  for (k in 1:length(xssemrb)) {
    if(xssemrb[k] != 0) {
      ii <- ii+1
      SSEclass[ii] <- xssemrb[k]
      RMSEnn[ii] <- xx$Freq[ii]
    }
  }
  
  ####################################################
  HesResults <- alist(HesResults=)$SelParmValues$beta0   # set to NULL
  
  # load the Hessian results if object exists
  objfile <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_HessianResults")
  if(file.exists(objfile) == TRUE) {
    load(objfile)
  }
  
  if(ifHess == 'yes' & if_estimate_simulation == 'no'){
    # Obtain SEs for coefficients using Hessian 
    
    message("Computing Hessian standard errors for parameter estimates...")
    
    ptm <- proc.time()
    
    Hssobj<-function(Estimate,
                     DataMatrix.list,SelParmValues,Csites.weights.list,
                     estimate.input.list,dlvdsgn){                  # changed beta0 to Estimate 9-6-16
      crossprod(estimateFeval(Estimate,
                              DataMatrix.list,SelParmValues,Csites.weights.list,
                              estimate.input.list,dlvdsgn))          # changed beta0 to Estimate 9-6-16
    }
    HH<-hessian(Hssobj, Estimate,
                DataMatrix.list = DataMatrix.list,
                SelParmValues = SelParmValues,
                Csites.weights.list = Csites.weights.list,
                estimate.input.list = estimate.input.list,
                dlvdsgn = dlvdsgn)    # n x n matrix
    
    ErrorSolve <- grep("singular",try({Hinv<-solve(HH)},TRUE))
    if(length(ErrorSolve)>0) {
      if(ErrorSolve==1) {
        
        cat("\n \n")
        message(singularMessage)
        
        Message <- "ERROR"
        JacobResults <- named.list(Message)
        estimate.metrics.list <- named.list(JacobResults)
        #       return(estimate.metrics.list)
        errorOccurred("estimateNLLSmetrics.R",batch_mode)
      }
    }
    
    HesRunTime <- proc.time() - ptm
    
    cov2<-Hinv*MSE*2                # multiplier of 2 added for parameter covariances
    cvar <- diag(cov2)
    cvar <- ifelse(cvar < 0,1,cvar) # negative variance cases coded as non-significant
    SEh<-sqrt(cvar)
    
    Hesnames <- Parmnames[SelParmValues$betaconstant==0]
    
    Th<-Estimate/SEh
    pTh <- 2*(1-pt(abs(Th),DF))
    
    # parameter correlations
    cor2 <- matrix(1,nrow=length(SEh),ncol=length(SEh))
    if(length(SEh)>1) {
      for (i in 2:length(SEh)) {
        for (j in 1:i-1) {
          cor2[i,j] <- cov2[i,j] / (sqrt(cvar[i]) * sqrt(cvar[j]))
          cor2[j,i] <- cor2[i,j]
        }
      }
    }
    
    
    # transfer estimated values into completed parameter set
    oSEh <- numeric(parmtotal)
    oTh <- numeric(parmtotal)
    opTh <- numeric(parmtotal)
    
    # estimated parameters
    j <- 0
    kparm <- 0
    for (i in 1:bcols) {
      if(betaconstant[i] == 0) { 
        j <- j+1
        kparm <- kparm+1
        oSEh[j] <- SEh[kparm]
        oTh[j] <- Th[kparm]
        opTh[j] <- pTh[kparm]
      } else {       # Non-estimated (constant) parameters
        j <- j+1
        oSEh[j] <- 0
        oTh[j] <- NA
        opTh[j] <- NA
      }
    }
    
    message("   Seconds elapsed in Hessian execution... ",round(HesRunTime[[3]],digits=5))
    
    HesResults <- named.list(Parmnames,Hesnames,oEstimate,oSEh,oTh,opTh,cov2,cor2,HesRunTime)
    
    # store Hessian estimates in object as list
    objfile <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_HessianResults")
    save(HesResults,file=objfile)
    
  }   # end ifHess check
  
  ##########################################################
  ##########################################################
  # SUMMARY PERFORMANCE METRICS FOR NO MONITORING ADJUSTMENT
  
  pResids <- estimateFevalNoadj(Estimate,
                                DataMatrix.list,SelParmValues,Csites.weights.list,
                                estimate.input.list,dlvdsgn)
  
  # Obtain summary metrics
  ppredict <- numeric(numsites)
  pyldobs <- numeric(numsites)
  pyldpredict <- numeric(numsites)
  pratio.obs.pred <- numeric(numsites)
  pxssemrb <- numeric(max(class[,1]))
  
  ins <- 0
  for (k in 1:nreach) {
    if (data[k,jdepvar] > 0) {
      ins <- ins+1
      obs <- data[k,jdepvar]
      Obs[ins] <- data[k,jdepvar]
      ppredict[ins] <- (exp(log(obs) - pResids[ins]))  
      pyldobs[ins] <- Obs[ins] / data[k,jtotarea] * yieldFactor
      pyldpredict[ins] <- ppredict[ins] / data[k,jtotarea] * yieldFactor
      pratio.obs.pred[ins] <- Obs[ins] / (ppredict[ins])
      
      # compute SSE by MRB
      if(class[ins] > 0) {
        pxssemrb[class[ins,1]] <- pxssemrb[class[ins,1]] + pResids[ins]^2
      }
    }                                 
  }  #  reach counter 
  pxssemrb[is.na(pxssemrb)] <- 0
  
  
  
  pSSE <- sum(pResids**2)
  pMSE <- pSSE / DF
  pRMSE <- sqrt(pMSE)
  
  pRSQ <- 1 - pSSE / (sum(log(Obs)^2) - sum(log(Obs))^2/mobs)
  pRSQ_ADJ <- 1 - ((mobs - 1)/DF)*(1 - pRSQ) 
  pRSQ_YLD <- 1 - pSSE / (sum(log(pyldobs)^2) - sum(log(pyldobs))^2/mobs)
  
  NSn <- 0
  NSd <- 0
  Obsmean <- mean(log(Obs))
  PBiasn <- 0
  PBiasd <- 0
  for (i in 1:mobs) {
    NSn <- NSn + (log(Obs[i]) - log(ppredict[i]))^2
    NSd <- NSd + (log(Obs[i]) - Obsmean)^2
    PBiasn <- PBiasn + (Obs[i] - ppredict[i])
    PBiasd <- PBiasd + Obs[i]
  }
  pNSeff <- 1 - (NSn / NSd)    # overall Nash-Sutcliffe model efficiency
  pPBias <- PBiasn / PBiasd * 100   # Percent bias  
  
  # Compute SSE and RMSE for class regions
  pSSEclass <- numeric(length(classgrp))
  ii <- 0
  for (k in 1:length(pxssemrb)) {
    if(pxssemrb[k] != 0) {
      ii <- ii+1
      pSSEclass[ii] <- pxssemrb[k]
    }
  }
  
  ######################################################################################
  if (if_sparrowEsts == 1) {
    
    jacobian <- sparrowEsts$jacobian
    JacobResults <- named.list(Parmnames,Beta.inital,bmin,bmax,esttype,btype,
                               oEstimate,oSEj,oTj,opTj,oVIF,odesign,oNames,e_val_spread,ppcc,
                               shap.test,shap.p,mean_exp_weighted_error,boot_resid,e_vec,leverage,
                               jacobian)
    
  } else {
    Parmnames <- noquote(c(srcvar,dlvvar,decvar,resvar))
    oEstimate <- SelParmValues$beta0                              # starting values 
    Beta.inital <- SelParmValues$beta0   # starting values
    esttype <- rep("Fixed",length(SelParmValues$beta0))
    btype <- SelParmValues$betatype
    
    # bscale <- SelParmValues$bsrcscale  (DELETE)
    
    bmin <- SelParmValues$betamin
    bmax <- SelParmValues$betamax
    odesign <- dlvdsgn
    leverage <- rep(1,mobs)
    boot_resid <- rep(1,mobs)
    standardResids <- rep(1,mobs)
    CooksD <- rep(1,mobs)
    CooksDpvalue <- rep(1,mobs)
    leverageCrit <- rep(1,mobs)
    JacobResults <- named.list(Parmnames,Beta.inital,bmin,bmax,esttype,btype,
                               oEstimate,odesign,leverage,boot_resid)
  }
  
  ANOVA.list <- named.list(mobs,npar,DF,SSE,MSE,RMSE,RSQ,RSQ_ADJ,RSQ_YLD,NSeff,PBias,
                           pSSE,pMSE,pRMSE,pRSQ,pRSQ_ADJ,pRSQ_YLD,pNSeff,pPBias)
  Mdiagnostics.list <- named.list(Obs,predict,yldobs,yldpredict,
                                  xstaid,tarea,ratio.obs.pred,xlat,xlon,
                                  ppredict,pyldobs,pyldpredict,pratio.obs.pred,
                                  classgrp,RMSEnn,SSEclass,pSSEclass,
                                  Resids,pResids,standardResids,CooksD,CooksDpvalue,
                                  leverage,leverageCrit)
  
  # store Jacobian estimates in object as list
  objfile <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_JacobResults")
  save(JacobResults,file=objfile)
  
  
  
  estimate.metrics.list <- named.list(JacobResults,HesResults,ANOVA.list,Mdiagnostics.list)
  
  
  
  
  return(estimate.metrics.list)    
  
}#end function

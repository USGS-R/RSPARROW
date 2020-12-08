#'@title predictBootstraps
#'@description Executes all model tasks related to obtaining model prediction uncertainties 
#'            using parametric bootstrap methods, for the control setting if_boot_predict<-"yes". \\cr \\cr
#'Executed By: controlFileTasksModel.R \\cr
#'Executes Routines: \\itemize\{\\item getVarList.R
#'             \\item named.list.R
#'             \\item predictBoot.R
#'             \\item unPackList.R\} \\cr
#'@param iseed User specified initial seed for the bootstraps from sparrow_control
#'@param biters User specified number of parametric bootstrap iterations from sparrow_control
#'@param estimate.list list output from `estimate.R`
#'@param estimate.input.list named list of sparrow_control settings: ifHess, s_offset, 
#'                           NLLS_weights,if_auto_scaling, and if_mean_adjust_delivery_vars
#'@param predict.list archive with all load and yield prediction variables to provide for the 
#'                    efficient access and use of predictions in subsequent execution of the parametric
#'                    bootstrap predictions and uncertainties, mapping, and scenario evaluations.  
#'                    For more details see documentation Section 5.3.1.5
#'@param BootResults data archive using the control setting `if_boot_estimate <- "yes"` for 
#'       use in subsequent execution of parametric bootstrap predictions. For more details see 
#'       documenation Section 5.2.6.2.
#'@param DataMatrix.list named list of 'data' and 'beta' matrices and 'data.index.list' 
#'                       for optimization
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'       parmMin>=0) | parmType!="SOURCE")`
#'@param subdata data.frame input data (subdata)
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@return `predictBoots.list` contains parametric bootstrap predictions for load and yield. 
#'            For more details see documentation Section 5.3.2.3



predictBootstraps <- function(iseed,biters,estimate.list,estimate.input.list,predict.list,BootResults,
                              DataMatrix.list,SelParmValues,subdata,
                              file.output.list) {
  
  
  # create global variable from list names (JacobResults)
  # create global variable from list names (predict.source.list)
  # create global variable from list names (SelParmValues)
  # transfer required variables to global environment from SUBDATA
  unPackList(lists = list(JacobResults = estimate.list$JacobResults,
                          datalstCheck = as.character(getVarList()$varList),
                          SelParmValues = SelParmValues,
                          predict.source.list = predict.list$predict.source.list,
                          file.output.list = file.output.list),
             parentObj = list(NA,
                              subdata = subdata,
                              NA,
                              NA,
                              NA))
  #################
  # SETUP VARIABLES
  #################
  
  ConcFactor <- estimate.input.list$ConcFactor
  yieldFactor <- estimate.input.list$yieldFactor
  
  nreach <- length(DataMatrix.list$data[,1])
  numsites <- sum(DataMatrix.list$data[,10] > 0 & DataMatrix.list$data[,13]==1)

  
  nnrows <- nreach
  ncols.load <- length(predict.list$oparmlist)
  ncols.yield <- length(predict.list$oyieldlist)
  
  nquant<-confInterval   # user-defined CI  
  nmax <- biters # number of iterations
  
  # Set CI index and count values
  bound <- ceiling((1-nquant)*nmax/2)+1 # bound index for CIs
  ntail <- ceiling(bound*2)+1           # number of CI tail values (always odd)
  xoutlo <- (1-nquant)*nmax/2           # lower bound index interpolation point
  xouthi <- ntail - (1-nquant)*nmax/2   # upper bound index interpolation point
  bootIndex <- ntail + 2  # index total for mean, variance, and ntail CI tail values (for biters=200, bootIndex=23+2)
  centIndex <- bound+1+2  # central index for CI values
  xCI <- matrix(0,nrow=nreach,ncol=ntail)   # temporary CI vector for sorting
  # Method for incrementally determining the CI bounds for 'biters' boot iterations:
  #  1. The lower and upper bound indices impose equal probability in each tail and correspond to the 
  #     (1-nquant)*nmax/2' bootstrap replications in each tail.
  #  2. The first 'ntail' boot iterations populate the 'bootpred' matrix with the CI tail values (always of odd length).
  #     The 'ntail' length is determined using a modification of the method described in Appendix A (Schwarz et al. 2006) 
  #     to store additional tail values to support linear interpolation of the CI tail values. 
  #  3. The middle index location for the matrix tail CI values ('centIndex') stores the newly added boot iteration value, 
  #     subsequent to a sort of the 'ntail' tail values from the prior boot iteration.
  #  4. The final lower and upper CIs are determined from linear interpolation of the sorted prediction metric tail CI values,
  #     based on interpolation of the 'floor' and 'ceiling' rounded values of the lower and upper bound indices that are used 
  #     as interpolation points ('xoutlo', 'xouthi'). The interpolation uses the two tail CI and index values that are located
  #     to the right and left of the middle index value, 'centIndex'. Confidence interval values obtained with this method 
  #     generally agree within less than 1% with those based on use of the R quantile (type=7) function; reach comparisons
  #     for the MRB3 training dataset give a median difference of 0.11% (IQR is 0.23% to 0.05%; maximum difference=1.4%). 
  
  bootpred <- array(0, dim=c(nreach, ncols.load, bootIndex)) # storage of first stage bootstrap metrics
  
  # mean exponentiated error (JacobResults$boot_resid) from the NLLS (applied to standard predictions)
  bootcorrection <- mean(exp(boot_resid))    
  
  # Obtain sample of size 'biter' for the exponentiated NLLS leverage-weighted log 
  #   model error ('estimate.list$JacobResults$boot_resid')
  # Random exponentiated errors used to compute the SE sample error variance and CI error variation
  set.seed(iseed)
  error.sample <- matrix(0,nrow=nreach,ncol=biters)  
  for (i in 1:nreach) {
    error.sample[i,] <- sample(exp(boot_resid),size=biters,replace=TRUE)
  }
  
  # set indexes for monitored load
  index.mpload.start <- 3 + length(predict.list$predict.source.list$srclist_total)
  index.mpload.end <- index.mpload.start + length(predict.list$predict.source.list$srclist_total)
  
  mpload_fraction <- predict.list$mpload_fraction  # reach vector of monitoring-load fraction of total load
  
  # load variables
  incMean <- numeric(nreach)
  seDenom <- matrix(0,nrow=nreach,ncol=ncols.load)
  sumBoot <- numeric(nreach)
  incSum <- numeric(nreach)
  bcmean <- numeric(nreach)
  ciParmLoadNM <- numeric(nreach)
  ciParmLoadM <- numeric(nreach)
  ciBootLoad <- numeric(nreach)
  
  
  ###########################
  # Interpolation CI function
  ###########################
  
  interpCItails <- function(xoutlo,xouthi,xCI) {
    # function to interpolate the specified lower/upper CI quantiles from the incrementally accumulated CI tail values
    # xoutlo is the lower bound index interpolation point
    # xouthi is the upper bound index interpolation point
    # xCI is the sorted vector with prediction metric CI tail values
    # CIs data.frame with lower and upper quantile values associated with the xCI vector
    
    # interpolate adjacent quantile values based on CIalpha and number of biters
    x1 <- c(floor(xoutlo),ceiling(xoutlo))
    y1 <- c(xCI[floor(xoutlo)+1],xCI[ceiling(xoutlo)+1])
    if(x1[1] != x1[2]) {
      xinterp <- approx(x1,y1,xoutlo,method="linear")
      xlo <- xinterp$y
    } else {
      xlo <- y1[1]
    }
    x1 <- c(floor(xouthi),ceiling(xouthi))
    y1 <- c(xCI[floor(xouthi)],xCI[ceiling(xouthi)])
    if(x1[1] != x1[2]) {
      xinterp <- approx(x1,y1,xouthi,method="linear")
      xhi <- xinterp$y
    } else {
      xhi <- y1[1]
    }
    
    CIs <- data.frame(xlo,xhi)
    return(CIs)
  }
  
  #################
  # BOOT ITERATIONS (stage 1)
  #################
  
  for (iter in 1:biters) {
    message(" Iteration ",iter)
    oEstimate <- BootResults$bEstimate[iter,] # obtain model coefficients generated in 'estimateBootstraps.R'
    Parmnames <- as.character(Parmnames)
    
    # Obtain matrices for uncorrected predictions for computing MEAN and SEs
    #   bootstrap mean exponentiated error for R iterations from the boot_resid associated with the 
    #   randomly selected coefficient estimates in estimateBootstraps.R
    #   No errors applied.
    # BootResults$bootmean_exp_weighted_error[iter] contains the associated exponentiated errors
    bootcorrectionR <- 1
    predBoots.list <- predictBoot(oEstimate,estimate.list,estimate.input.list,bootcorrectionR,
                                  DataMatrix.list,SelParmValues,subdata)
    
    index.deliv_frac <- predBoots.list$index.deliv_frac
    
    #  Note that "predict.list$predmatrix[i,j]" contains the standard NLLS estimated loads.
    #  Both non-monitored (simulated) and monitored-adjusted (mpload) loads and sources include the 
    #  mean exp leverage-weighted NLLS log residual (bias-correction factor). No bias adjustment 
    #  applied to deliv_frac.
    
    # LOADS
    for(j in 2:ncols.load) {
      
      if(j >= index.mpload.start & j<= index.mpload.end) { # select only the monitored load and source predictions
        
        # MONITORING-ADJUSTED LOADS AND UNCERTAINTIES
        
        # incremental mean load only for monitored load and source predictions
        #  sum non-monitored load proportion and monitored load proportion and store in incremental mean 'meanBoot'
        meanNM <- predBoots.list$predmatrix[,j]*(1-mpload_fraction)*BootResults$bootmean_exp_weighted_error[iter]
        sumBoot <- meanNM + predBoots.list$predmatrix[,j]*mpload_fraction
        incSum <- bootpred[,j,1]
        bootpred[,j,1] <- bootpred[,j,1] + 1/iter * (sumBoot - bootpred[,j,1])
        
        #  incremental mean for non-monitored load for SE calculation
        seDenom[,j] <- seDenom[,j] + 1/iter * (meanNM - seDenom[,j])
        
        # incremental variance sum for the sum of the non-monitored and monitored load
        bootpred[,j,2] <- bootpred[,j,2] + (sumBoot-incSum) * (sumBoot-bootpred[,j,1])
        
        # incremental storage of CI tails
        if(iter <= ntail) {
          # add the first ntail values to the CIs
          ciParmLoadNM <- (predict.list$predmatrix[,j]/bootcorrection)*(1-mpload_fraction)*error.sample[,iter]
          ciParmLoadM <- (predict.list$predmatrix[,j]/bootcorrection)*mpload_fraction
          ciBootLoad <- predBoots.list$predmatrix[,j]*(1-mpload_fraction) + predBoots.list$predmatrix[,j]*mpload_fraction # no error applied to bootpred(mpload)
          ciParmLoadNoerror <- ciParmLoadNM / error.sample[,iter] + ciParmLoadM
          bootpred[,j,(iter+2)] <- ifelse((ciParmLoadNM + ciParmLoadM)>0,ciBootLoad * ciParmLoadNoerror / (ciParmLoadNM + ciParmLoadM),0)
          
        } else {
          # add new value to central index of tail CI values
          ciParmLoadNM <- (predict.list$predmatrix[,j]/bootcorrection)*(1-mpload_fraction)*error.sample[,iter]
          ciParmLoadM <- (predict.list$predmatrix[,j]/bootcorrection)*mpload_fraction
          ciBootLoad <- predBoots.list$predmatrix[,j]*(1-mpload_fraction) + predBoots.list$predmatrix[,j]*mpload_fraction  # no error applied to bootpred(mpload)
          xy <- bootpred[,j,(3:(ntail+2))]
          xCI <- ifelse(is.na(xy),0,xy)  # transfer prior tail value for sorting
          bootpred[,j,(3:(ntail+2))] <- t(apply(t(xCI),2,sort))  # 2 indicates column sort for each reach
          ciParmLoadNoerror <- ciParmLoadNM / error.sample[,iter] + ciParmLoadM
          bootpred[,j,centIndex] <- ifelse((ciParmLoadNM + ciParmLoadM)>0,ciBootLoad * ciParmLoadNoerror / (ciParmLoadNM + ciParmLoadM),0)
        } 
      } else {
        # NON-MONITORED (SIMULATED) LOADS AND UNCERTAINTIES
        
        # incremental mean
        incMean <- bootpred[,j,1]
        if (j == index.deliv_frac) {  # no bias correction applied to non-mass metric
          bcmean <- predBoots.list$predmatrix[,j]
        } else {
          bcmean <- predBoots.list$predmatrix[,j]*BootResults$bootmean_exp_weighted_error[iter]
        }
        bootpred[,j,1] <- bootpred[,j,1] + 1/iter * (bcmean - bootpred[,j,1])
        
        # incremental variance sum
        bootpred[,j,2] <- bootpred[,j,2] + (bcmean-incMean)*(bcmean-bootpred[,j,1])
        
        # incremental storage of CI tails
        if(iter <= ntail) {
          # add the first ntail values to the CIs
          if (j == index.deliv_frac) {  # no bias correction applied to non-mass metric
            bootpred[,j,(iter+2)] <- predBoots.list$predmatrix[,j]
          } else {
            bootpred[,j,(iter+2)] <- predBoots.list$predmatrix[,j]/error.sample[,iter]
          }
        } else {
          # add new value to central index of tail CIs
          xCI <- bootpred[,j,(3:(ntail+2))]  # transfer prior tail value for sorting
          bootpred[,j,(3:(ntail+2))] <- t(apply(t(xCI),2,sort))  # 2 indicate column sort for each reach    
          if (j == index.deliv_frac) {  # no bias correction applied to non-mass metric
            bootpred[,j,centIndex] <- predBoots.list$predmatrix[,j]  # overwrite central value with new value  
          } else {
            bootpred[,j,centIndex] <- predBoots.list$predmatrix[,j]/error.sample[,iter]  # overwrite central value with new value               
          }
        } # end check for first ntail iters vs subsequent ntail+ iters
        
      }  # end check for monitored vs non-monitored (simulated loads)
    } # end LOAD variables
    
  } # end boot iter loop
  
  
  ####################
  # PREDICTION METRICS (stage 2)
  ####################
  
  message(" Computing bootstrap prediction load metrics (Bias-corrected means, SEs, CIs)...")
  
  meanNLLS <- numeric(nreach)
  mratio <- numeric(nreach)
  
  # Model error variance per equation 1.144 (Schwarz et al. 2006)
  #   Computed as ratio of variance of JacobResults$boot_resid to the squared mean exponentiated log residual from the NLLS
  model.error.var <- var(exp(boot_resid)) / bootcorrection^2
  
  sample.error.var.boots <- numeric(nnrows)  # error variance for total load for all reaches
  sample.error.var <- numeric(nnrows)  # error variance for total load for all reaches
  
  # LOADS
  
  bootpred <- ifelse(is.na(bootpred),0,bootpred)   # check for NAs and replace with zero
  
  bootmatrix <- matrix(0,nrow=nnrows,ncol=((ncols.load-1)*4+1)) 
  bootmatrix[,1] <- predBoots.list$predmatrix[,1]  # set reach ID
  
  k <- 2
  for (j in 2:ncols.load) {
    
    if(j >= index.mpload.start & j<= index.mpload.end) { # select only the monitored load and source predictions
      # MONITORING-ADJUSTED LOADS AND UNCERTAINTIES (Schwarz, written communication: May 9, 2017; SAS methods)
      #   Method: Approach applies the model error only to the non-monitored portion of the load
      
      # REACHES     
      for (i in 1:nnrows) {
        if(bootpred[i,j,1] > 0) {
          
          # bias-corrected mean load (apportion load to monitored and unmonitored fractions)
          meanNLLS <- (predict.list$predmatrix[i,j]/bootcorrection)*(1-mpload_fraction[i])*bootcorrection + (predict.list$predmatrix[i,j]/bootcorrection)*mpload_fraction[i]
          bootmatrix[i,k] <- meanNLLS^2 / bootpred[i,j,1]
          
          # SE calculation
          seNum <- ((predict.list$predmatrix[i,j]/bootcorrection*(1-mpload_fraction[i]))*bootcorrection)^2 
          mratio <- ifelse(seDenom[i,j]>0,(seNum/seDenom[i,j])/bootmatrix[i,k],0)
          sample.error.var <- (bootpred[i,j,2]/(biters-1)) / meanNLLS^2  
          # variance of the model error is apportioned only to the non-monitored component of load
          bootmatrix[i,k+1] <- bootmatrix[i,k] * sqrt(mratio^2 * model.error.var + sample.error.var)
          
          # CIs
          # Quantiles based on tail CI values with a randomly selected error from exponentiated NLLS log residuals         
          # first, compute the quantiles 
          xCI <- bootpred[i,j,(3:(ntail+2))]  # transfer prior tail value for sorting
          xCI <- sort(xCI)
          CIs <- interpCItails(xoutlo,xouthi,xCI)
          # second, compute the load metric as the ratio of the squared NLLS prediction (with mean error correction removed) 
          #   to the CI values
          bootmatrix[i,k+2] <- (predict.list$predmatrix[i,j]/bootcorrection)^2 / CIs$xhi    
          bootmatrix[i,k+3] <- (predict.list$predmatrix[i,j]/bootcorrection)^2 / CIs$xlo 
          
        } # end only positive loads processed
      }  # end reach loop (i)      
      
    } else {   
      # NON-MONITORED (SIMULATED) LOADS AND UNCERTAINTIES (Schwarz et al. 2006)
      
      # REACHES     
      for (i in 1:nnrows) {
        if(bootpred[i,j,1] > 0) {
          
          # bias-corrected mean load per equation 1.138
          bootmatrix[i,k] <- predict.list$predmatrix[i,j]^2 / bootpred[i,j,1]
          
          # SE calculation
          # Calculate sampling error variance defined as the ratio of variance of bootstrap flux (with bootstrap 
          #  varying mean exponentiated error correction) to the squared least-squares prediction (with the 
          #  mean exponentiated error correction) per equation 1.144
          #  Variance is computed with n-1 denominator
          sample.error.var <- (bootpred[i,j,2]/(biters-1)) / predict.list$predmatrix[i,j]^2  
          if(j==2) {sample.error.var.boots[i] <- sample.error.var}  # reach error variance for total load
          # SE per equation 1.144
          bootmatrix[i,k+1] <- bootmatrix[i,k] * sqrt(model.error.var + sample.error.var)        
          
          # CIs
          # Quantiles based on tail values of bootstrap distribution with a randomly selected error from exponentiated NLLS log residuals        
          # first, compute the quantiles for the denominator of equation 1.150 for CI
          xCI <- bootpred[i,j,(3:(ntail+2))]  # transfer prior tail value for sorting
          xCI <- sort(xCI)
          CIs <- interpCItails(xoutlo,xouthi,xCI)
          # second, compute the load metric as the ratio of the squared NLLS prediction (with mean error correction removed) 
          #   to the product of the bootstrap prediction with a randomly selected exponentiated NLLS log residual error 
          #   per equation 1.150 for CI (Schwarz et al. 2006)
          if (j == index.deliv_frac) {  # no bias correction applied previously to non-mass metric
            bootmatrix[i,k+2] <- predict.list$predmatrix[i,j]^2 / CIs$xhi    
            bootmatrix[i,k+3] <- predict.list$predmatrix[i,j]^2 / CIs$xlo
          } else {  # bias correction applied previously to mass metric
            bootmatrix[i,k+2] <- (predict.list$predmatrix[i,j]/bootcorrection)^2 / CIs$xhi    
            bootmatrix[i,k+3] <- (predict.list$predmatrix[i,j]/bootcorrection)^2 / CIs$xlo 
          }
          
        } # end only positive loads processed
      }  # end reach loop (i)
      
    } # end check for monitoring prediction metrics
    k <- k+4
  }  # end metric loop (j)
  
  
  # YIELDS
  
  message(" Computing bootstrap prediction yield metrics (Bias-corrected means, SEs, CIs)...")
  
  bootyldmatrix <- matrix(0,nrow=nnrows,ncol=((ncols.yield-1)*4+1)) 
  bootyldmatrix[,1] <- predBoots.list$predmatrix[,1]  # set reach ID
  
  srcNum <- length(predict.list$predict.source.list$srclist_total)
  tarea <- subdata$demtarea
  iarea <- subdata$demiarea
  
  # REACHES     
  for (i in 1:nnrows) {
    
    # Concentration computed from Total load 
    if(subdata$meanq[i]>0) {
      bootyldmatrix[i,2:5] <- bootmatrix[i,2:5]/subdata$meanq[i]*ConcFactor
    }
    
    # Total yield and sources from Total load and source block for mean, SE, and CIs
    sIndex <- 2
    eIndex <- sIndex + 4+4*srcNum-1
    ysIndex <- 5 + 1                     # inclusive of concentration index
    yeIndex <- ysIndex + 4+4*srcNum-1    # inclusive of concentration index
    if(tarea[i]>0) {
      bootyldmatrix[i,ysIndex:yeIndex] <- bootmatrix[i,sIndex:eIndex]/tarea[i] * yieldFactor      
    }
    
    # Conditional total yield and sources from conditional load and sources 
    sIndex <- eIndex+1
    eIndex <- sIndex + 4+4*srcNum-1
    ysIndex <- yeIndex+1
    yeIndex <- ysIndex + 4+4*srcNum-1
    if(tarea[i]>0) {
      bootyldmatrix[i,ysIndex:yeIndex] <- bootmatrix[i,sIndex:eIndex]/tarea[i] * yieldFactor
    }
    
    # Incremental yield and sources from incremental load and sources
    sIndex <- (eIndex+1) + (4+4*srcNum-1) + 1   # inclusive of "nd" load and sources
    eIndex <- sIndex + 4+4*srcNum-1             # inclusive of "nd" load and sources
    ysIndex <- yeIndex+1
    yeIndex <- ysIndex + 4+4*srcNum-1 
    if(iarea[i]>0) {
      bootyldmatrix[i,ysIndex:yeIndex] <- bootmatrix[i,sIndex:eIndex]/iarea[i] * yieldFactor
    }
    
    # Delivered incremental yield and sources from delivered incremental load and sources
    sIndex <- (eIndex+1) + 4            # inclusive of deliv fraction
    eIndex <- sIndex + 4+4*srcNum-1     # inclusive of deliv fraction
    ysIndex <- yeIndex+1
    yeIndex <- ysIndex + 4+4*srcNum-1
    if(iarea[i]>0) {
      bootyldmatrix[i,ysIndex:yeIndex] <- bootmatrix[i,sIndex:eIndex]/iarea[i] * yieldFactor
    }
    
  } # end reach loop
  
  
  ##########################################################################################   
  # Output loads (bootmatrix):
  #   pload_total                Total load (fully decayed)                           
  #   pload_(sources)            Source load (fully decayed)                             
  #   mpload_total               Monitoring-adjusted total load (fully decayed)          
  #   mpload_(sources)           Monitoring-adjusted source load (fully decayed)         
  #   pload_nd_total             Total load delivered to streams (no stream decay)
  #   pload_nd_(sources)         Source load delivered to streams (no stream decay)
  #   pload_inc                  Total incremental load delivered to streams
  #   pload_inc_(sources)        Source incremental load delivered to streams
  #   deliv_frac                 Fraction of total load delivered to terminal reach
  #   pload_inc_deliv            Total incremental load delivered to terminal reach
  #   pload_inc_(sources)_deliv  Source incremental load delivered to terminal reach
  #   share_total_(sources)      Source shares for total load (percent)
  #   share_inc_(sources)        Source share for incremental load (percent)
  
  # Output yields (bootyldmatrix):
  #   Concentration              Concentration based on decayed total load and discharge
  #   yield_total                Total yield (fully decayed)
  #   yield_(sources)            Source yield (fully decayed)
  #   myield_total               Monitoring-adjusted total yield (fully decayed)
  #   myield_(sources)           Monitoring-adjusted source yield (fully decayed)
  #   yield_inc                  Total incremental yield delivered to streams
  #   yield_inc_(sources)        Source incremental yield delivered to streams
  #   yield_inc_deliv            Total incremental yield delivered to terminal reach
  #   yield_inc_(sources)_deliv  Source incremental yield delivered to terminal reach
  
  ##########################################################################################
  
  message(" Storing bootstrap prediction summary metrics...")
  
  # Output bootstrap load predictions
  
  # Total load and sources
  mean1 <- character(4)
  mean1[1] <- "mean_pload_total"
  mean1[2] <- "se_pload_total"
  mean1[3] <- "ci_lo_pload_total"
  mean1[4] <- "ci_hi_pload_total"
  k <- 1
  mean2 <- character(length(srclist_total)*4)
  for (i in 1:length(srclist_total)){
    mean2[k] <- paste0("mean_",srclist_total[i]) 
    mean2[k+1] <- paste0("se_",srclist_total[i]) 
    mean2[k+2] <- paste0("ci_lo_",srclist_total[i]) 
    mean2[k+3] <- paste0("ci_hi_",srclist_total[i]) 
    k <- k+4
  }
  
  # Monitoring-adjusted total load and sources
  mean3 <- character(4)
  mean3[1] <- "mean_mpload_total"
  mean3[2] <- "se_mpload_total"
  mean3[3] <- "ci_lo_mpload_total"
  mean3[4] <- "ci_hi_mpload_total"
  k <- 1
  mean4 <- character(length(srclist_mtotal)*4)
  for (i in 1:length(srclist_mtotal)){
    mean4[k] <- paste0("mean_",srclist_mtotal[i]) 
    mean4[k+1] <- paste0("se_",srclist_mtotal[i]) 
    mean4[k+2] <- paste0("ci_lo_",srclist_mtotal[i]) 
    mean4[k+3] <- paste0("ci_hi_",srclist_mtotal[i]) 
    k <- k+4
  }
  
  # Non-decayed total load and sources
  mean5 <- character(4)
  mean5[1] <- "mean_pload_nd_total"
  mean5[2] <- "se_pload_nd_total"
  mean5[3] <- "ci_lo_pload_nd_total"
  mean5[4] <- "ci_hi_pload_nd_total"
  k <- 1
  mean6 <- character(length(srclist_nd_total)*4)
  for (i in 1:length(srclist_nd_total)){
    mean6[k] <- paste0("mean_",srclist_nd_total[i]) 
    mean6[k+1] <- paste0("se_",srclist_nd_total[i]) 
    mean6[k+2] <- paste0("ci_lo_",srclist_nd_total[i]) 
    mean6[k+3] <- paste0("ci_hi_",srclist_nd_total[i]) 
    k <- k+4
  }
  
  # Incremental load and sources
  mean7 <- character(4)
  mean7[1] <- "mean_pload_inc"
  mean7[2] <- "se_pload_inc"
  mean7[3] <- "ci_lo_pload_inc"
  mean7[4] <- "ci_hi_pload_inc"
  k <- 1
  mean8 <- character(length(srclist_inc)*4)
  for (i in 1:length(srclist_inc)){
    mean8[k] <- paste0("mean_",srclist_inc[i]) 
    mean8[k+1] <- paste0("se_",srclist_inc[i]) 
    mean8[k+2] <- paste0("ci_lo_",srclist_inc[i]) 
    mean8[k+3] <- paste0("ci_hi_",srclist_inc[i]) 
    k <- k+4
  }
  
  # Delivery fraction
  mean9 <- character(4)
  mean9[1] <- "mean_deliv_frac"
  mean9[2] <- "se_deliv_frac"
  mean9[3] <- "ci_lo_deliv_frac"
  mean9[4] <- "ci_hi_deliv_frac"
  
  
  # Incremental delivered load and sources
  mean10 <- character(4)
  mean10[1] <- "mean_pload_inc_deliv"
  mean10[2] <- "se_pload_inc_deliv"
  mean10[3] <- "ci_lo_pload_inc_deliv"
  mean10[4] <- "ci_hi_pload_inc_deliv"
  k <- 1
  mean11 <- character(length(srclist_inc)*4)
  for (i in 1:length(srclist_inc)){
    mean11[k] <- paste0("mean_",srclist_inc[i],"_deliv") 
    mean11[k+1] <- paste0("se_",srclist_inc[i],"_deliv") 
    mean11[k+2] <- paste0("ci_lo_",srclist_inc[i],"_deliv") 
    mean11[k+3] <- paste0("ci_hi_",srclist_inc[i],"_deliv") 
    k <- k+4
  }
  
  # Source shares for total load (percent)
  k <- 1
  mean12 <- character(length(srclist_inc)*4)
  for (i in 1:length(srclist_inc)){
    mean12[k] <- paste0("mean_","share_total_",srcvar[i]) 
    mean12[k+1] <- paste0("se_","share_total_",srcvar[i]) 
    mean12[k+2] <- paste0("ci_lo_","share_total_",srcvar[i]) 
    mean12[k+3] <- paste0("ci_hi_","share_total_",srcvar[i]) 
    k <- k+4
  }
  
  #  Source share for incremental load (percent)
  k <- 1
  mean13 <- character(length(srclist_inc)*4)
  for (i in 1:length(srclist_inc)){
    mean13[k] <- paste0("mean_","share_inc_",srcvar[i]) 
    mean13[k+1] <- paste0("se_","share_inc_",srcvar[i]) 
    mean13[k+2] <- paste0("ci_lo_","share_inc_",srcvar[i]) 
    mean13[k+3] <- paste0("ci_hi_","share_inc_",srcvar[i]) 
    k <- k+4
  }
  
  boparmlist <- c("waterid",mean1,mean2,mean3,mean4,mean5,mean6,mean7,mean8,mean9,
                  mean10,mean11,mean12,mean13) 
  
  ###############################################
  # Store load uncertainties for mapping routine
  ###############################################  
  # Uncertainty predictions:
  #   se_pload_total             Standard error of the total load (percent of mean)
  #   ci_pload_total             prediction interval of the total load (percent of mean)
  
  se_pload_total <- numeric(length(bootmatrix[,1]))
  ci_pload_total <- numeric(length(bootmatrix[,1]))
  se_mpload_total <- numeric(length(bootmatrix[,1]))
  ci_mpload_total <- numeric(length(bootmatrix[,1]))
  mploadIndex <- 1 + 4 + length(srclist_total)*4 + 1
  for (i in 1:length(bootmatrix[,1])) {
    if(!is.na(bootmatrix[i,2])) {
      if(bootmatrix[i,2] > 0) {
        se_pload_total[i] <- bootmatrix[i,3] / bootmatrix[i,2] * 100
        ci_pload_total[i] <- (bootmatrix[i,5] - bootmatrix[i,4]) / bootmatrix[i,2] * 100
        se_mpload_total[i] <- bootmatrix[i,(mploadIndex+1)] / bootmatrix[i,mploadIndex] * 100
        ci_mpload_total[i] <- (bootmatrix[i,(mploadIndex+3)] - bootmatrix[i,(mploadIndex+2)]) / bootmatrix[i,mploadIndex] * 100
      }
    }
  }
  
  # store bootstrap uncertainty estimates in object as list and save
  
  BootUncertainties <- named.list(se_pload_total,ci_pload_total,se_mpload_total,ci_mpload_total,
                                  model.error.var,sample.error.var.boots)
  objfile <- paste0(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_BootUncertainties")
  save(BootUncertainties,file=objfile)
  assign("BootUncertainties",BootUncertainties,envir = .GlobalEnv)
  
  ##########################################################################################
  
  # Output bootstrap yield predictions
  
  # Total concentration
  mean0 <- character(4)
  mean0[1] <- "mean_conc_total"
  mean0[2] <- "se_conc_total"
  mean0[3] <- "ci_lo_conc_total"
  mean0[4] <- "ci_hi_conc_total"
  
  # Total yield and sources
  mean1 <- character(4)
  mean1[1] <- "mean_yield_total"
  mean1[2] <- "se_yield_total"
  mean1[3] <- "ci_lo_yield_total"
  mean1[4] <- "ci_hi_yield_total"
  k <- 1
  mean2 <- character(length(srclist_total)*4)
  for (i in 1:length(srclist_total)){
    mean2[k] <- paste0("mean_",srclist_yield[i]) 
    mean2[k+1] <- paste0("se_",srclist_yield[i]) 
    mean2[k+2] <- paste0("ci_lo_",srclist_yield[i]) 
    mean2[k+3] <- paste0("ci_hi_",srclist_yield[i]) 
    k <- k+4
  }
  
  # Monitoring-adjusted total yield and sources
  mean3 <- character(4)
  mean3[1] <- "mean_myield_total"
  mean3[2] <- "se_myield_total"
  mean3[3] <- "ci_lo_myield_total"
  mean3[4] <- "ci_hi_myield_total"
  k <- 1
  mean4 <- character(length(srclist_myield)*4)
  for (i in 1:length(srclist_mtotal)){
    mean4[k] <- paste0("mean_",srclist_myield[i]) 
    mean4[k+1] <- paste0("se_",srclist_myield[i]) 
    mean4[k+2] <- paste0("ci_lo_",srclist_myield[i]) 
    mean4[k+3] <- paste0("ci_hi_",srclist_myield[i]) 
    k <- k+4
  }
  
  # Incremental yield and sources
  mean7 <- character(4)
  mean7[1] <- "mean_yield_inc"
  mean7[2] <- "se_yield_inc"
  mean7[3] <- "ci_lo_yield_inc"
  mean7[4] <- "ci_hi_yield_inc"
  k <- 1
  mean8 <- character(length(srclist_inc)*4)
  for (i in 1:length(srclist_inc)){
    mean8[k] <- paste0("mean_",srclist_yldinc[i]) 
    mean8[k+1] <- paste0("se_",srclist_yldinc[i]) 
    mean8[k+2] <- paste0("ci_lo_",srclist_yldinc[i]) 
    mean8[k+3] <- paste0("ci_hi_",srclist_yldinc[i]) 
    k <- k+4
  }
  
  # Incremental delivered yield and sources
  mean10 <- character(4)
  mean10[1] <- "mean_yield_inc_deliv"
  mean10[2] <- "se_yield_inc_deliv"
  mean10[3] <- "ci_lo_yield_inc_deliv"
  mean10[4] <- "ci_hi_yield_inc_deliv"
  k <- 1
  mean11 <- character(length(srclist_inc)*4)
  for (i in 1:length(srclist_inc)){
    mean11[k] <- paste0("mean_",srclist_yldinc_deliv[i]) 
    mean11[k+1] <- paste0("se_",srclist_yldinc_deliv[i]) 
    mean11[k+2] <- paste0("ci_lo_",srclist_yldinc_deliv[i]) 
    mean11[k+3] <- paste0("ci_hi_",srclist_yldinc_deliv[i]) 
    k <- k+4
  }
  
  byldoparmlist <- c("waterid",mean0,mean1,mean2,mean3,mean4,mean7,mean8,mean10,mean11) 
  
  
  predictBoots.list <- named.list(boparmlist,bootmatrix,byldoparmlist,bootyldmatrix,error.sample)
  assign("predictBoots.list",predictBoots.list,envir = .GlobalEnv)
  
  
  return(predictBoots.list)
  
}#end function

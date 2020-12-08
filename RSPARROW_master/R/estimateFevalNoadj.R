#'@title estimateFevalNoadj
#'@description Accumulates loads in the reach network according to the user's model 
#'            specification, making comparisons of the unconditioned predictions of  load to the actual loads for 
#'            monitored (calibration site) reaches to return a vector of weighted residuals (difference between 
#'            the actual and predicted loads). No monitoring load substitution is performed (ifadjust=0).  \\cr \\cr
#'Executed By: \\itemize\{\\item estimate.R
#'             \\item estimateNLLSmetrics.R\} \\cr
#'Executes Routines: \\itemize\{\\item estimateFeval.R
#'             \\item unPackList.R
#'             \\item tnoder.for\} \\cr
#'@param beta0 estimated parameters (no constants)
#'@param DataMatrix.list named list of 'data' and 'beta' matrices and 'data.index.list' 
#'                       for optimization
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'       parmMin>=0) | parmType!="SOURCE")`
#'@param Csites.weights.list regression weights as proportional to incremental area size
#'@param estimate.input.list named list of sparrow_control settings: ifHess, s_offset, 
#'                           NLLS_weights,if_auto_scaling, and if_mean_adjust_delivery_vars
#'@param dlvdsgn design matrix imported from design_matrix.csv
#'@return `e` vector of weighted residuals



estimateFevalNoadj <- function(beta0,
                               DataMatrix.list,SelParmValues,Csites.weights.list,
                               estimate.input.list,dlvdsgn) {
  
  
  # setup global variables in function environment
  data <- DataMatrix.list$data
  beta <- DataMatrix.list$beta
  betaconstant <- SelParmValues$betaconstant
  nreach <- length(data[,1])
  bcols <- length(beta[1,])
  
  weight <- rep(1,length(Csites.weights.list$weight))  # no weights applied
  ifadjust <- 0  # no adjustment for monitoring loads
  
  unPackList(lists = list(data.index.list = DataMatrix.list$data.index.list, 
                          estimate.input.list = estimate.input.list),
             parentObj = list(NA,NA))
  
  # transfer estimated parameters into complete parameter vector (with constant non-estimated parameters)
  betalst <- numeric(bcols)   # bcols length
  k <- 0
  for (i in 1:bcols) {
    betalst[i] <- beta[1,i]
    if(betaconstant[i] == 0) { 
      k <- k+1
      betalst[i] <- beta0[k]
    }
  }
  
  # Load the parameter estimates to BETA1
  beta1<-t(matrix(betalst, ncol=nreach, nrow=bcols))
  
  # setup for REACH decay
  jjdec <- length(jdecvar)
  if(sum(jdecvar) > 0) { 
    rchdcayf <- matrix(1,nrow=nreach,ncol=1)
    for (i in 1:jjdec){
      rchdcayf[,1] <- rchdcayf[,1] * eval(parse(text=reach_decay_specification))
    }  
  } else {  
    rchdcayf <- matrix(1,nrow=nreach,ncol=1)
  }
  
  # setup for RESERVOIR decay
  jjres <- length(jresvar)
  if(sum(jresvar) > 0) {
    resdcayf <- matrix(1,nrow=nreach,ncol=1)
    for (i in 1:jjres){
      resdcayf[,1] <- resdcayf[,1] * eval(parse(text=reservoir_decay_specification))
    }  
  } else { 
    resdcayf <- matrix(1,nrow=nreach,ncol=1)
  } 
  
  # Setup for SOURCE DELIVERY # (nreach X nsources)
  jjdlv <- length(jdlvvar)
  jjsrc <- length(jsrcvar)
  
  ddliv1 <- matrix(0,nrow=nreach,ncol=jjdlv)
  if(sum(jdlvvar) > 0) {
    for (i in 1:jjdlv){
      ddliv1[,i] <- (beta1[,jbdlvvar[i]] * data[,jdlvvar[i]])
    }
    ddliv2 <- matrix(0,nrow=nreach,ncol=jjsrc)
    ddliv2 <- eval(parse(text=incr_delivery_specification))     # "exp(ddliv1 %*% t(dlvdsgn))"

  } else {
    ddliv2 <- matrix(1,nrow=nreach,ncol=jjsrc)   # change ncol from =1 to =jjsrc to avoid non-conformity error (2-19-2013)
  }
  
  
  # Setup for SOURCE
  ddliv3 <- (ddliv2 * data[,jsrcvar]) * beta1[,jbsrcvar]
  if(sum(jsrcvar) > 0) {
    dddliv <- matrix(0,nrow=nreach,ncol=1)
    for (i in 1:jjsrc){
      dddliv[,1] <- dddliv[,1] + ddliv3[,i]
    }
  } else {
    dddliv <- matrix(1,nrow=nreach,ncol=1)
  }
  
  
  # incremental delivered load
  incddsrc <- rchdcayf**0.5 * resdcayf * dddliv
  
  # Compute the reach transport factor
  carryf <- data[,jfrac] * rchdcayf * resdcayf
  
  nstaid <- max(data[,jstaid])
  nnode <- max(data[,jtnode],data[,jfnode])
  ee <- matrix(0,nrow=nstaid,ncol=1)
  e <- matrix(0,nrow=nstaid,ncol=1)
  data2 <- matrix(0,nrow=nreach,ncol=4)
  data2[,1] <- data[,jfnode]
  data2[,2] <- data[,jtnode]
  data2[,3] <- data[,jdepvar]
  data2[,4] <- data[,jiftran]
  incddsrc <- ifelse(is.na(incddsrc),0,incddsrc)
  carryf <- ifelse(is.na(carryf),0,carryf)
  
  # Fortran subroutine to accumulate mass climbing down the reach network
  #   compute and accumulate incremental RCHLD
  return_data <- .Fortran('tnoder',
                          ifadjust=as.integer(ifadjust),
                          nreach=as.integer(nreach),
                          nnode=as.integer(nnode),
                          data2=as.double(data2),
                          incddsrc=as.double(incddsrc),
                          carryf=as.double(carryf),
                          ee=as.double(ee),PACKAGE="tnoder") 
  e <- return_data$ee
  
  e <- sqrt(weight) * e
  
  
  
  return(e) 
  
}#end function

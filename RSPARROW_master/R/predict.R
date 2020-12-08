#'@title predict
#'@description Calculates all conditioned and unconditioned model predictions for reaches for 
#'            the control setting if_predict<-"yes".  \\cr \\cr
#'Executed By: \\itemize\{\\item controlFileTasksModel.R
#'             \\item estimate.R\} \\cr
#'Executes Routines: \\itemize\{\\item getVarList.R
#'             \\item named.list.R
#'             \\item unPackList.R
#'             \\item deliv_fraction.for
#'             \\item mptnoder.for
#'             \\item ptnoder.for\} \\cr
#'@param estimate.list list output from `estimate.R`
#'@param estimate.input.list named list of sparrow_control settings: ifHess, s_offset, 
#'                           NLLS_weights,if_auto_scaling, and if_mean_adjust_delivery_vars
#'@param bootcorrection numeric vector equal to 
#'       `estimate.list$JacobResults$mean_exp_weighted_error` unless NULL, then reset to 1.0
#'@param DataMatrix.list named list of 'data' and 'beta' matrices and 'data.index.list' 
#'                       for optimization
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'       parmMin>=0) | parmType!="SOURCE")`
#'@param subdata data.frame input data (subdata)
#'@return `predict.list` archive with all load and yield prediction variables to provide for 
#'            the efficient access and use of predictions in subsequent execution of the parametric bootstrap 
#'            predictions and uncertainties, mapping, and scenario evaluations.  For more details see 
#'            documentation Section 5.3.1.5



predict <- function(estimate.list,estimate.input.list,bootcorrection,DataMatrix.list,
                    SelParmValues,subdata) {
  
  
  #################################################
  
  
  data <- DataMatrix.list$data  
  
  # create global variable from list names (JacobResults)
  # 'oEstimate' containing the estimated mean parameters for all non-constant and constant parameters
  # 'Parmnames' list of variable names 
  # transfer required variables to global environment from SUBDATA
  # create global variable from list names
  # transfer required variables to global environment from 'DataMatrix.list$data.index.list'
  unPackList(lists = list(JacobResults = estimate.list$JacobResults,
                          datalstCheck = as.character(getVarList()$varList),
                          SelParmValues = SelParmValues,
                          estimate.input.list = estimate.input.list,
                          data.index.list = DataMatrix.list$data.index.list),
             parentObj = list(NA,
                              subdata = subdata,
                              NA,
                              NA,
                              NA))
  
  # Setup variables for prediction 
  
  nreach <- length(data[,1])
  numsites <- sum(ifelse(data[,10] > 0,1,0))  # jdepvar site load index
  
  # transfer estimated parameters into complete parameter vector (inclusive of non-estimated constants)
  betalst <- oEstimate   # JacobResults object
  
  # Load the parameter estimates to BETA1
  beta1<-t(matrix(betalst, ncol=nreach, nrow=length(oEstimate)))
  
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
  
  ####################################################
  # incremental delivered load for decayed and nondecayed portions
  
  incdecay <- rchdcayf**0.5 * resdcayf   # incremental reach and reservoir decay
  totdecay <- rchdcayf * resdcayf        # total reach and reservoir decay
  
  incddsrc <- rchdcayf**0.5 * resdcayf * dddliv
  incddsrc_nd <- dddliv
  
  # Compute the reach transport factor
  carryf <- data[,jfrac] * rchdcayf * resdcayf
  carryf_nd <- data[,jfrac] 
  
  ####################################################
  # Store the incremental loads for total and sources
  
  pload_inc <- as.vector(dddliv)  # create incremental load variable
  
  srclist_inc <- character(length(jsrcvar))
  
  for (j in 1:length(jsrcvar)) {  
    ddliv <- as.matrix((ddliv2[,j] * data[,jsrcvar[j]]) * beta1[,jbsrcvar[j]] ) 
    assign(paste0("pload_inc_",Parmnames[j]),as.vector(ddliv))   # create variable 'pload_inc_(source name)'
    srclist_inc[j] <- paste0("pload_inc_",Parmnames[j])
  }
  
  ####################################################
  # Store the total decayed and nondecayed loads
  
  nnode <- max(data[,jtnode],data[,jfnode])
  ee <- matrix(0,nrow=nreach,ncol=1)
  pred <- matrix(0,nrow=nreach,ncol=1)
  
  i_obs <- 1
  
  data2 <- matrix(0,nrow=nreach,ncol=4)
  data2[,1] <- data[,jfnode]
  data2[,2] <- data[,jtnode]
  data2[,3] <- data[,jdepvar]
  data2[,4] <- data[,jiftran]
  
  
  # Total decayed load (no monitoring adjustment)
  
  incddsrc <- ifelse(is.na(incddsrc),0,incddsrc)
  carryf <- ifelse(is.na(carryf),0,carryf)
  ifadjust <- 0     # no monitoring load adjustment
  
  # accumulate loads
  return_data <- .Fortran('ptnoder',
                          ifadjust=as.integer(ifadjust),
                          nreach=as.integer(nreach),
                          nnode=as.integer(nnode),
                          data2=as.double(data2),
                          incddsrc=as.double(incddsrc),
                          carryf=as.double(carryf),
                          ee=as.double(ee),PACKAGE="ptnoder") 
  pred <- return_data$ee
  
  pload_total <- pred   # nonadjusted total load 
  
  
  # Total decayed load (with monitoring adjustment)
  
  incddsrc <- ifelse(is.na(incddsrc),0,incddsrc)
  carryf <- ifelse(is.na(carryf),0,carryf)
  ifadjust <- 1     # monitoring load adjustment
  
  # Fortran subroutine to accumulate mass climbing down the reach network, compute and accumulate incremental RCHLD
  #   tnoder.dll must be placed in SYSTEM PATH accessible directory
  
  return_data <- .Fortran('ptnoder',
                          ifadjust=as.integer(ifadjust),
                          nreach=as.integer(nreach),
                          nnode=as.integer(nnode),
                          data2=as.double(data2),
                          incddsrc=as.double(incddsrc),
                          carryf=as.double(carryf),
                          ee=as.double(ee),PACKAGE="ptnoder") 
  pred <- return_data$ee
  
  mpload_total <- pred  # monitoring-adjusted total load
  
  
  # Total decayed monitored load and fraction
  #  (fraction is the total monitored reach load contributed by the decayed
  #      upstream monitored load)
  
  inczero <- rep(0,length(incddsrc))
  ifadjust <- 1     # monitoring load adjustment
  
  return_data <- .Fortran('ptnoder',
                          ifadjust=as.integer(ifadjust),
                          nreach=as.integer(nreach),
                          nnode=as.integer(nnode),
                          data2=as.double(data2),
                          incddsrc=as.double(inczero),
                          carryf=as.double(carryf),
                          ee=as.double(ee),PACKAGE="ptnoder") 
  pred <- return_data$ee
  mpload_decay <- pred  # decayed monitoring total load
  
  if(mpload_total > 0) {
    mpload_fraction <- mpload_decay / mpload_total
  }
  
  # Total nondecayed load 
  
  incddsrc_nd <- ifelse(is.na(incddsrc_nd),0,incddsrc_nd)
  carryf_nd <- ifelse(is.na(carryf_nd),0,carryf_nd)
  pred <- matrix(0,nrow=nreach,ncol=1)
  ifadjust <- 0     # no monitoring load adjustment
  
  # Fortran subroutine to accumulate mass climbing down the reach network, compute and accumulate incremental RCHLD
  #   tnoder.dll must be placed in SYSTEM PATH accessible directory
  
  return_data <- .Fortran('ptnoder',
                          ifadjust=as.integer(ifadjust),
                          nreach=as.integer(nreach),
                          nnode=as.integer(nnode),
                          data2=as.double(data2),
                          incddsrc_nd=as.double(incddsrc_nd),
                          carryf_nd=as.double(carryf_nd),
                          ee=as.double(ee),PACKAGE="ptnoder") 
  pred <- return_data$ee
  
  pload_nd_total <- pred   
  
  
  # Total load for each SOURCE (decayed and nondecayed)
  
  srclist_total <- character(length(jsrcvar))
  srclist_nd_total <- character(length(jsrcvar))
  srclist_mtotal <- character(length(jsrcvar))
  
  for (j in 1:length(jsrcvar)) { 
    
    ddliv <- as.matrix((ddliv2[,j] * data[,jsrcvar[j]]) * beta1[,jbsrcvar[j]] ) 
    
    # incremental delivered load
    incddsrc <- rchdcayf**0.5 * resdcayf * ddliv
    incddsrc_nd <- ddliv
    
    # Compute the reach transport factor
    carryf <- data[,jfrac] * rchdcayf * resdcayf
    carryf_nd <- data[,jfrac] 
    
    # Decayed total source load
    pred <- matrix(0,nrow=nreach,ncol=1)
    i_obs <- 1
    
    incddsrc <- ifelse(is.na(incddsrc),0,incddsrc)
    carryf <- ifelse(is.na(carryf),0,carryf)
    ifadjust <- 0     # no monitoring load adjustment
    
    return_data <- .Fortran('ptnoder',
                            ifadjust=as.integer(ifadjust),
                            nreach=as.integer(nreach),
                            nnode=as.integer(nnode),
                            data2=as.double(data2),
                            incddsrc=as.double(incddsrc),
                            carryf=as.double(carryf),
                            ee=as.double(ee),PACKAGE="ptnoder") 
    pred <- return_data$ee
    
    assign(paste0("pload_",Parmnames[j]),pred)   # create variable 'pload_(source name)'
    srclist_total[j] <- paste0("pload_",Parmnames[j])
    
    assign(paste0("mpload_",Parmnames[j]),pred)   # create variable 'mpload_(source name)'
    srclist_mtotal[j] <- paste0("mpload_",Parmnames[j])
    
    # Nondecayed total source load
    pred <- matrix(0,nrow=nreach,ncol=1)
    i_obs <- 1
    
    incddsrc_nd <- ifelse(is.na(incddsrc_nd),0,incddsrc_nd)
    carryf_nd <- ifelse(is.na(carryf_nd),0,carryf_nd)
    ifadjust <- 0     # no monitoring load adjustment
    
    return_data <- .Fortran('ptnoder',
                            ifadjust=as.integer(ifadjust),
                            nreach=as.integer(nreach),
                            nnode=as.integer(nnode),
                            data2=as.double(data2),
                            incddsrc_nd=as.double(incddsrc_nd),
                            carryf_nd=as.double(carryf_nd),
                            ee=as.double(ee),PACKAGE="ptnoder") 
    pred <- return_data$ee
    
    assign(paste0("pload_nd_",Parmnames[j]),pred)   # create variable 'pload_(source name)'
    srclist_nd_total[j] <- paste0("pload_nd_",Parmnames[j])
  } 
  
  
  # Monitoring-adjusted source loads (computed as shares of nonadjusted total loads)
  if(numsites > 0) {
    srclist_mtotal <- character(length(jsrcvar))
    for (j in 1:length(jsrcvar)) {
      share <- eval(parse(text=srclist_total[j])) / pload_total   # source share of total load
      share <- ifelse(is.na(share),0,share)
      
      ddliv <- as.matrix((ddliv2[,j] * data[,jsrcvar[j]]) * beta1[,jbsrcvar[j]] ) 
      
      # incremental delivered load
      incddsrc <- rchdcayf**0.5 * resdcayf * ddliv
      incddsrc_nd <- ddliv
      
      # Compute the reach transport factor
      carryf <- data[,jfrac] * rchdcayf * resdcayf
      carryf_nd <- data[,jfrac] 
      
      # Decayed total source load
      pred <- matrix(0,nrow=nreach,ncol=1)
      
      incddsrc <- ifelse(is.na(incddsrc),0,incddsrc)
      carryf <- ifelse(is.na(carryf),0,carryf)
      ifadjust <- 1     # monitoring load adjustment
      
      return_data <- .Fortran('mptnoder',
                              ifadjust=as.integer(ifadjust),
                              share=as.double(share),
                              nreach=as.integer(nreach),
                              nnode=as.integer(nnode),
                              data2=as.double(data2),
                              incddsrc=as.double(incddsrc),
                              carryf=as.double(carryf),
                              ee=as.double(ee),PACKAGE="mptnoder") 
      pred <- return_data$ee
      
      assign(paste0("mpload_",Parmnames[j]),pred)   # create variable 'mpload_(source name)'
      srclist_mtotal[j] <- paste0("mpload_",Parmnames[j])
    }
  }
  
  # Delivery fraction
  data2 <- matrix(0,nrow=nreach,ncol=5) 
  data2[,1] <- data[,jfnode]
  data2[,2] <- data[,jtnode]
  data2[,3] <- data[,jfrac]
  data2[,4] <- data[,jiftran]
  data2[,5] <- data[,jtarget]    # termflag indicators (=1, =3)
  
  deliver <- function(incdecay) { 
    sumatt <- matrix(0,nrow=nreach,ncol=1)
    fsumatt <- matrix(0,nrow=nreach,ncol=1)
    return_data <- .Fortran('deliv_fraction',
                            numrchs=as.integer(nreach),
                            waterid=as.integer(waterid),
                            nnode=as.integer(nnode),
                            data2=as.double(data2),
                            incdecay=as.double(incdecay),
                            totdecay=as.double(totdecay),
                            sumatt=as.double(sumatt),PACKAGE="deliv_fraction") 
    fsumatt <- return_data$sumatt
    return(fsumatt) 
  }  # end sumatts function
  deliv_frac <- deliver(incdecay)
  
  #######################################
  # Output load predictions
  
  srclist_inc_deliv <- character(length(jsrcvar))   # delivered incremental load
  for (i in 1:length(jsrcvar)) {
    srclist_inc_deliv[i] <- paste0(srclist_inc[i],"_deliv")
  }
  
  srclist_inc_share <- character(length(jsrcvar))   # incremental source share (percent)
  for (i in 1:length(jsrcvar)) {
    srclist_inc_share[i] <- paste0("share_inc_",srcvar[i])
  }
  
  srclist_total_share <- character(length(jsrcvar))   # total source share (percent)
  for (i in 1:length(jsrcvar)) {
    srclist_total_share[i] <- paste0("share_total_",srcvar[i])
  }
  
  oparmlist <- c("waterid","pload_total",srclist_total,
                 "mpload_total",srclist_mtotal, 
                 "pload_nd_total",srclist_nd_total, 
                 "pload_inc",srclist_inc, 
                 "deliv_frac",
                 "pload_inc_deliv",srclist_inc_deliv,
                 srclist_total_share,srclist_inc_share) 
  
  oparmlistExpl <- character(length(oparmlist))
  
  # create matrix with predictions
  ncols <- 7 + length(srclist_total) + length(srclist_mtotal) + length(srclist_nd_total) + 
    length(srclist_inc) + length(srclist_inc) + length(srclist_inc) + length(srclist_inc)
  predmatrix <- matrix(0,nrow=length(pload_total),ncol=ncols)
  loadunits <- rep(loadUnits,ncols)
  loadunits[1] <- "Reach ID Number"
  
  
  predmatrix[,1] <- subdata$waterid
  oparmlistExpl[1] <- "Reach ID Number"
  # total load
  predmatrix[,2] <- pload_total * as.vector(bootcorrection)
  oparmlistExpl[2] <- "Total load (fully decayed)"
  for (i in 1:length(srclist_total)){
    predmatrix[,2+i] <- eval(parse(text=srclist_total[i])) * as.vector(bootcorrection)
    oparmlistExpl[2+i] <- "Total source load (fully decayed)"
    
    predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+i)] <- 
      predmatrix[,2+i] / predmatrix[,2] * 100  # source share
    
    # avoids reporting NAs for share
    predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+i)] <- 
      ifelse(is.na(predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+i)]),
             0,predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+i)]) 
    
    loadunits[(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+i)] <- "Percent"  # source share
    oparmlistExpl[(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+i)] <- "Source shares for total load"
  }
  # monitored-adjusted total load
  predmatrix[,(3+length(srclist_total))] <- mpload_total * as.vector(bootcorrection)
  oparmlistExpl[(3+length(srclist_total))] <- "Monitoring-adjusted (conditional) total load (fully decayed)"
  for (i in 1:length(srclist_mtotal)){
    predmatrix[,(3+length(srclist_total)+i)] <- eval(parse(text=srclist_mtotal[i])) * as.vector(bootcorrection)
    oparmlistExpl[(3+length(srclist_total)+i)] <- "Monitoring-adjusted (conditional) total source load (fully decayed)"
  } 
  # nondecayed (ND) total load
  predmatrix[,(4+length(srclist_total)+length(srclist_mtotal))] <- pload_nd_total * as.vector(bootcorrection)
  oparmlistExpl[(4+length(srclist_total)+length(srclist_mtotal))] <- "Total load delivered to streams (no stream decay)"
  for (i in 1:length(srclist_nd_total)){
    predmatrix[,(4+length(srclist_total)+length(srclist_mtotal)+i)] <- eval(parse(text=srclist_nd_total[i])) * as.vector(bootcorrection)
    oparmlistExpl[(4+length(srclist_total)+length(srclist_mtotal)+i)] <- "Total source load delivered to streams (no stream decay)"
  }
  # incremental load
  predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total))] <- pload_inc * as.vector(bootcorrection)
  oparmlistExpl[(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total))] <- "Total incremental load delivered to reach (with one-half of reach decay)"
  for (i in 1:length(srclist_inc)){
    predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+i)] <- eval(parse(text=srclist_inc[i])) * as.vector(bootcorrection) 
    oparmlistExpl[(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+i)] <- "Total incremental source load delivered to reach (with one-half of reach decay)"
    predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+
                   length(srclist_inc)+length(srclist_inc)+length(srclist_inc)+i)] <- 
      predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+i)] / 
      predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total))] * 100  # source share
    
    # avoids reporting NAs for share
    predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+
                   length(srclist_inc)+length(srclist_inc)+length(srclist_inc)+i)] <- 
      ifelse(is.na(predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+length(srclist_inc)+i)]),
             0,predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+length(srclist_inc)+i)])
    
    loadunits[(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+
                 length(srclist_inc)+length(srclist_inc)+length(srclist_inc)+i)] <- "Percent"  # source share
    oparmlistExpl[(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+
                     length(srclist_inc)+length(srclist_inc)+length(srclist_inc)+i)] <- "Source shares for incremental load"
  }
  # delivery fraction
  predmatrix[,(6+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc))] <- deliv_frac
  loadunits[(6+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc))] <- "Fraction"
  oparmlistExpl[(6+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc))] <- "Fraction of total load delivered to terminal reach"
  
  # delivered incremental load
  dload <- predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total))] * deliv_frac
  predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc))] <- dload
  oparmlistExpl[(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc))] <- "Total incremental load delivered to terminal reach"
  for (i in 1:length(srclist_inc)){
    dload <- predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+i)] * deliv_frac
    predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+i)] <- dload
    oparmlistExpl[(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+i)] <- "Total incremental source load delivered to terminal reach"
  }
  
  
  # Output yield predictions
  
  # replace "pload" with "yld" in each string
  srclist_yield <- gsub("pload", "yield", srclist_total)
  srclist_myield <- gsub("pload", "yield", srclist_mtotal)
  srclist_yldinc <- gsub("pload", "yield", srclist_inc)
  srclist_yldinc_deliv <- gsub("pload", "yield", srclist_inc_deliv)
  
  oyieldlist <- c("waterid","concentration","yield_total",srclist_yield,
                  "myield_total",srclist_myield, 
                  "yield_inc",srclist_yldinc, 
                  "yield_inc_deliv",srclist_yldinc_deliv) 
  
  oyieldlistExpl <- character(length(oyieldlist))
  
  ncols <- 6 + length(srclist_yield) + length(srclist_myield) + length(srclist_yldinc) + length(srclist_yldinc_deliv)
  yldmatrix <- matrix(0,nrow=length(pload_total),ncol=ncols)
  yieldunits <- rep(yieldUnits,ncols)
  yieldunits[1] <- "Reach ID Number"
  yieldunits[2] <- ConcUnits
  
  yldmatrix[,1] <- subdata$waterid
  oyieldlistExpl[1] <- "Reach ID Number"
  
  # total yield
  for (i in 1:length(subdata$waterid)) {
    if(demtarea[i] > 0) {
      if(meanq[i] > 0) { yldmatrix[i,2] <- predmatrix[i,2] / meanq[i] * ConcFactor }   # concentration
      oyieldlistExpl[2] <- "Flow-weighted concentration based on decayed total load and mean discharge"
      
      yldmatrix[i,3] <- predmatrix[i,2] / demtarea[i] * yieldFactor
      oyieldlistExpl[3] <- "Total yield (fully decayed)"
      
      for (j in 1:length(srclist_total)){
        yldmatrix[i,3+j] <- predmatrix[i,2+j] / demtarea[i] * yieldFactor
        oyieldlistExpl[3+j] <- "Total source yield (fully decayed)"
      }
      # monitored-adjusted total yield
      yldmatrix[i,(4+length(srclist_total))] <- predmatrix[i,(3+length(srclist_total))] / demtarea[i] * yieldFactor
      oyieldlistExpl[(4+length(srclist_total))] <- "Monitoring-adjusted (conditional) total yield (fully decayed)"
      for (j in 1:length(srclist_mtotal)){
        yldmatrix[i,(4+length(srclist_total)+j)] <- predmatrix[i,(3+length(srclist_total)+j)] / demtarea[i] * yieldFactor
        oyieldlistExpl[(4+length(srclist_total)+j)] <- "Monitoring-adjusted (conditional) total source yield (fully decayed)"
      }
    }
  }
  # incremental yield
  for (i in 1:length(subdata$waterid)) {
    if(demiarea[i] > 0) {
      yldmatrix[i,(5+length(srclist_total)+length(srclist_mtotal))] <- predmatrix[i,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total))] / demiarea[i] * yieldFactor
      oyieldlistExpl[(5+length(srclist_total)+length(srclist_mtotal))] <- "Total incremental yield delivered to reach (with one-half of reach decay)"
      for (j in 1:length(srclist_inc)){
        yldmatrix[i,(5+length(srclist_total)+length(srclist_mtotal)+j)] <- predmatrix[i,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+j)] / demiarea[i] * yieldFactor
        oyieldlistExpl[(5+length(srclist_total)+length(srclist_mtotal)+j)] <- "Total incremental source yield delivered to reach (with one-half of reach decay)"
      }
      
      # delivered incremental yield
      yldmatrix[i,(6+length(srclist_total)+length(srclist_mtotal)+length(srclist_inc))] <- 
        predmatrix[i,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc))] / demiarea[i] * yieldFactor
      oyieldlistExpl[(6+length(srclist_total)+length(srclist_mtotal)+length(srclist_inc))] <- "Total incremental yield delivered to terminal reach"
      for (j in 1:length(srclist_inc)){
        yldmatrix[i,(6+length(srclist_total)+length(srclist_mtotal)+length(srclist_inc)+j)] <- 
          predmatrix[i,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+j)]  / demiarea[i] * yieldFactor
        oyieldlistExpl[(6+length(srclist_total)+length(srclist_mtotal)+length(srclist_inc)+j)] <- "Total incremental source yield delivered to terminal reach"
      }
    }
  }
  
  
  predict.source.list <- named.list(srclist_total,srclist_mtotal,srclist_inc,srclist_inc_deliv,
                                    srclist_nd_total,srclist_yield,srclist_myield,srclist_yldinc,
                                    srclist_yldinc_deliv) 
  
  
  predict.list <- named.list(oparmlist,loadunits,predmatrix,oyieldlist,yieldunits,yldmatrix,predict.source.list,
                             oparmlistExpl,oyieldlistExpl,
                             mpload_decay,mpload_fraction)
  assign("predict.list",predict.list, envir=.GlobalEnv)
  
  
  return(predict.list)
  
}#end function


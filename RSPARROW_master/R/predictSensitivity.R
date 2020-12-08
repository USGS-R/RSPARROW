#'@title predictSensitivity
#'@description Supports the calculation of parameter sensitivities, executed by the 
#'            'diagnosticSensitivity' function, by calculating the unconditioned predictions for an individual parameter. \\cr \\cr
#'Executed By: diagnosticSensitivity.R \\cr
#'Executes Routines: \\itemize\{\\item getVarList.R
#'             \\item unPackList.R
#'             \\item ptnoder.for\} \\cr
#'@param AEstimate parameter estimates (original or adjusted by 1 percent)
#'@param estimate.list list output from `estimate.R`
#'@param DataMatrix.list named list of 'data' and 'beta' matrices and 'data.index.list' 
#'                       for optimization
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'       parmMin>=0) | parmType!="SOURCE")`
#'@param reach_decay_specification the SAS IML reach decay function code from sparrow_control
#'@param reservoir_decay_specification the SAS IML reservoir decay function code from 
#'       sparrow_control
#'@param subdata data.frame input data (subdata)
#'@return `pload_total` nonadjusted total load



predictSensitivity <- function(AEstimate,estimate.list,DataMatrix.list,SelParmValues,
                               reach_decay_specification,reservoir_decay_specification,subdata) {
  
  
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
                          data.index.list = DataMatrix.list$data.index.list),
             parentObj = list(NA,
                              subdata = subdata,
                              NA,
                              NA))
  
  # Setup variables for prediction 
  
  nreach <- length(data[,1])
  numsites <- sum(ifelse(data[,10] > 0,1,0))  # jdepvar site load index
  
  # transfer estimated parameters into complete parameter vector (inclusive of non-estimated constants)
  # values are perturbed in subsequent function calls in sensitivity analysis
  betalst <- AEstimate 
  
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
    ddliv2 <-eval(parse(text=incr_delivery_specification))     # "exp(ddliv1 %*% t(dlvdsgn))"
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
  
  
  
  return(pload_total)
  
}#end function


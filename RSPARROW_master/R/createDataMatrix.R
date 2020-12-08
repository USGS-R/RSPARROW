#'@title createDataMatrix
#'@description Creates the R `DataMatrix.list` object, containing five data elements that are 
#'            used to estimate the model and produce model predictions. \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'Executes Routines: \\itemize\{\\item getVarList.R
#'             \\item named.list.R
#'             \\item unPackList.R\} \\cr
#'@param if_mean_adjust_delivery_vars yes/no character string indicating if the delivery 
#'       variables are to be mean adjusted from sparrow_control
#'@param subdata data.frame input data (subdata)
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'       parmMin>=0) | parmType!="SOURCE")`
#'@param betavalues data.frame of model parameters from parameters.csv
#'@return `DataMatrix.list`  list containing objects `dataNames` (Names of the data matrix 
#'            columns),



createDataMatrix <- function(if_mean_adjust_delivery_vars,subdata,SelParmValues,betavalues) {
  
  
  # setup the 12 required variables
  datalst <- as.character(getVarList()$matrixlst)
  
  #########################################################
  # create global variable from list names
  unPackList(lists = list(SelParmValues = SelParmValues),
             parentObj = list(NA))
  # transfer required variables to global environment from SUBDATA
  unPackList(lists = list(master.list = c(datalst,srcvar,dlvvar,decvar,resvar,othervar)),
             parentObj = list(subdata = subdata))
  
  #########################################################
  # setup index values for DATA matrix by name 
  
  jwaterid <- which(datalst%in%"waterid") # SPARROW Reach Identifier
  jstaid <- which(datalst%in%"staid")     # SPARROW Monitoring Station Identifier 
  jfnode <- which(datalst%in%"fnode")     # Upstream Reach Node Identifier
  jtnode <- which(datalst%in%"tnode")     # Downstream Reach Node Identifier 
  jfrac <- which(datalst%in%"frac")       # Fraction Upstream Flux Diverted to Reach
  jiftran <- which(datalst%in%"iftran")   # If Reach Transmits Flux (1=yes, 0=no)
  jtarget <- which(datalst%in%"target")   # Downstream target reach
  jtotarea <- which(datalst%in%"demtarea")# Total upstream drainage area (km2)
  jiarea <- which(datalst%in%"demiarea")  # Incremental reach drainage area (km2)
  jdepvar <- which(datalst%in%"depvar")   # Dependent variable load (kg/yr)
  jhydseq <- which(datalst%in%"hydseq")   # SPARROW Reach Hydrologic Sequencing Code
  jmean_flow <- which(datalst%in%"meanq") # Mean flow (cfs)
  jcalsites <- which(datalst%in%"calsites") #Calibration site index
  
  ivar <- 13   # number of required network variables
  
  # pselect created in 'selectParmValues.R'
  srcselect <- ifelse(betavalues$parmType == "SOURCE" & pselect == 1,1,0)
  dlvselect <- ifelse(betavalues$parmType == "DELIVF" & pselect == 1,1,0)
  decselect <- ifelse(betavalues$parmType == "STRM" & pselect == 1,1,0)
  resselect <- ifelse(betavalues$parmType == "RESV" & pselect == 1,1,0)
  otherselect <- ifelse(betavalues$parmType == "OTHER" & pselect == 1,1,0)
  
  # set index values for variable types selected
  if (sum(srcselect) > 0) {
    jbsrcvar <- rep(1:sum(srcselect),1)
    jsrcvar <- jbsrcvar + ivar
  } else {
    jbsrcvar <- 0
    jsrcvar <- 0
  }
  if (sum(dlvselect) > 0) {
    jbdlvvar <- rep(max(jbsrcvar)+1:sum(dlvselect),1)
    jdlvvar <- jbdlvvar + ivar
  } else {
    jbdlvvar <- 0
    jdlvvar <- 0
  }
  if (sum(decselect) > 0) {
    jbdecvar <- rep((max(jbsrcvar)+sum(dlvselect))+1:sum(decselect),1)
    jdecvar <- jbdecvar + ivar
  } else {
    jbdecvar <- 0
    jdecvar <- 0
  }
  if (sum(resselect) > 0) {
    jbresvar <- rep((max(jbsrcvar)+sum(dlvselect)+sum(decselect))+1:sum(resselect),1)
    jresvar <- jbresvar + ivar
  } else {
    jbresvar <- 0
    jresvar <- 0
  }
  
  if (sum(otherselect) > 0) {
    jbothervar <- rep((max(jbsrcvar)+sum(dlvselect)+sum(decselect)+sum(resselect))+1:sum(otherselect),1)
    jothervar <- jbothervar + ivar
  } else {
    jbothervar <- 0
    jothervar <- 0
  }
  
  data.index.list <- named.list(jwaterid,jstaid,jfnode,jtnode,jfrac,jiftran,jtarget,
                                jtotarea,jiarea,jdepvar,jhydseq,jmean_flow,
                                jsrcvar,jdlvvar,jdecvar,jresvar,jothervar,
                                jbsrcvar,jbdlvvar,jbdecvar,jbresvar,jbothervar)
  
  ######################################################
  # transfer data from vectors to 'DATA' matrix
  ncols <- ivar + bcols
  data <- matrix(1:length(depvar), ncol=ncols, nrow=length(depvar))
  
  dataNames <- c("waterid","staid","fnode","tnode","frac","iftran","target",
                 "demtarea","demiarea","depvar","hydseq","meanq","calsites")
  
  for (i in 1:ivar) {
    data[,i] <- eval(parse(text=datalst[i]))  # transfer required 12 variables to data
  }
  
  
  # transfer source variables
  dataNames <- c(dataNames,srcvar)
  betaNames <- srcvar
  
  iend <- ivar+length(srcvar)
  j<-0
  for (i in (ivar+1):iend) {
    j <- j+1
    data[,i] <- eval(parse(text=srcvar[j]))
  }
  
  # transfer delivery variables
  if(max(jdlvvar) != 0) {
    dataNames <- c(dataNames,dlvvar)
    betaNames <- c(betaNames,dlvvar)
    ibeg <- ivar+length(srcvar)+1
    iend <- ivar+length(srcvar)+length(dlvvar)
    j<-0
    for (i in ibeg:iend) {
      j <- j+1
      if(if_mean_adjust_delivery_vars == "yes"){
        data[,i] <- eval(parse(text=dlvvar[j])) - mean(eval(parse(text=dlvvar[j])))
      }
      else {
        data[,i] <- eval(parse(text=dlvvar[j]))
      }
    }
  } # end length check
  
  # transfer reach decay variables
  if(max(jdecvar) != 0) {
    dataNames <- c(dataNames,decvar)
    betaNames <- c(betaNames,decvar)
    ibeg <- ivar+length(srcvar)+length(dlvvar)+1
    iend <- ivar+length(srcvar)+length(dlvvar)+length(decvar)
    j<-0
    for (i in ibeg:iend) {
      j <- j+1
      data[,i] <- eval(parse(text=decvar[j]))
    }
  } # end length check
  
  # transfer reservoir decay variables
  if(max(jresvar) != 0) {
    dataNames <- c(dataNames,resvar)
    betaNames <- c(betaNames,resvar)
    ibeg <- ivar+length(srcvar)+length(dlvvar)+length(decvar)+1
    iend <- ivar+length(srcvar)+length(dlvvar)+length(decvar)+length(resvar)
    j<-0
    for (i in ibeg:iend) {
      j <- j+1
      data[,i] <- eval(parse(text=resvar[j]))
    }
  } # end length check
  
  # transfer other variables
  if(max(jothervar) != 0) {
    dataNames <- c(dataNames,othervar)
    betaNames <- c(betaNames,othervar)
    ibeg <- ivar+length(srcvar)+length(dlvvar)+length(decvar)+length(resvar)+1
    iend <- ivar+length(srcvar)+length(dlvvar)+length(decvar)+length(resvar)+length(othervar)
    j<-0
    for (i in ibeg:iend) {
      j <- j+1
      data[,i] <- eval(parse(text=othervar[j]))
    }
  } # end length check
  
  
  beta <- matrix(1:length(depvar), ncol=bcols, nrow=length(depvar))
  for (i in 1:bcols) {
    beta[,i] <- beta0[i]
  }
  
  DataMatrix.list <- named.list(dataNames,betaNames,data,beta,data.index.list)
  
  
  return(DataMatrix.list)
  
}#end function


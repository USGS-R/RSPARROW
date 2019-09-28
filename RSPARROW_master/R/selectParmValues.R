#'@title selectParmValues
#'@description Creates the 'SelParmValues' object with the user-selected parameter attributes 
#'            and performs consistency checks on the initial, minimum, and maximum values of the parameters.  \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'Executes Routines: \\itemize\{\\item errorOccurred.R
#'             \\item named.list.R\} \\cr
#'@param df betavalues - list of parameters from parameters.csv
#'@param if_estimate yes/no indicating whether or not estimation is run
#'@param if_estimate_simulation character string setting from sparrow_control.R indicating 
#'       whether estimation should be run in simulation mode only.
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@return `SelParmValues` selected parameters from parameters.csv using condition 
#'            `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'            parmMin>=0) | parmType!="SOURCE")`



selectParmValues <- function(df,if_estimate,if_estimate_simulation,batch_mode){
  
  
  pselect <- ifelse(df$parmConstant==1 | (df$parmMax > 0 | (df$parmType=="DELIVF" & df$parmMax>=0)) & 
                      (df$parmMin<df$parmMax) & 
                      ((df$parmType=="SOURCE" & df$parmMin>=0) | df$parmType!="SOURCE")
                    ,1,0)   # identify selected variables
  srcselect <- ifelse(df$parmType == "SOURCE" & pselect == 1,1,0)
  dlvselect <- ifelse(df$parmType == "DELIVF" & pselect == 1,1,0)
  decselect <- ifelse(df$parmType == "STRM" & pselect == 1,1,0)
  resselect <- ifelse(df$parmType == "RESV" & pselect == 1,1,0)
  otherselect <- ifelse(df$parmType == "OTHER" & pselect == 1,1,0)
  bcols <- sum(pselect)
  
  # transfer parameters for selected variables
  beta0 <- df$parmInit[pselect == 1]
  betamin <- df$parmMin[pselect == 1]
  betamax <- df$parmMax[pselect == 1]
  betatype <- df$parmType[pselect == 1]
  betaconstant <- df$parmConstant[pselect == 1]
  bsrcconstant <- df$parmConstant[df$parmType == "SOURCE" & pselect == 1]
  bCorrGroup <- df$parmCorrGroup[pselect == 1]
  sparrowNames <- df$sparrowNames[pselect == 1]
  
  srcvar <- df$sparrowNames[srcselect == 1]
  dlvvar <- df$sparrowNames[dlvselect == 1]
  decvar <- df$sparrowNames[decselect == 1]
  resvar <- df$sparrowNames[resselect == 1]
  othervar <- df$sparrowNames[otherselect == 1]
  
  SelParmValues <- named.list(sparrowNames,bcols,beta0,betamin,betamax,betatype,pselect,
                              betaconstant,bsrcconstant,bCorrGroup,
                              srcvar,dlvvar,decvar,resvar,othervar)
  
  #checks on parameter values
  exit <- function() {
    .Internal(.invokeRestart(list(NULL, NULL), NULL))
  }
  
  npar <- bcols-(sum(betaconstant)) 
  if (length(betamin)!=bcols){
    #wrong length lower
    if (if_estimate=="yes" | if_estimate_simulation=="yes"){
      message("INVALID NUMBER OF parmMin VALUES FOUND IN PARAMETERS FILE.\nNUMBER OF parmMin VALUES MUST EQUAL NUMBER OF PARAMETERS SELECTED\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
      if (batch_mode=="yes"){#if batch output message to log
        cat("INVALID NUMBER OF parmMin VALUES FOUND IN PARAMETERS FILE.\nNUMBER OF parmMin VALUES MUST EQUAL NUMBER OF PARAMETERS SELECTED\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
      }
      errorOccurred("selectParmValues.R",batch_mode)
    }
  }
  if (length(betamax)!=bcols){
    #wrong length upper
    if (if_estimate=="yes" | if_estimate_simulation=="yes"){
      message("INVALID NUMBER OF parmMax VALUES FOUND IN PARAMETERS FILE.\nNUMBER OF parmMax VALUES MUST EQUAL NUMBER OF PARAMETERS SELECTED\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
      if (batch_mode=="yes"){#if batch output message to log
        cat("INVALID NUMBER OF parmMax VALUES FOUND IN PARAMETERS FILE.\nNUMBER OF parmMax VALUES MUST EQUAL NUMBER OF PARAMETERS SELECTED\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
      }
      errorOccurred("selectParmValues.R",batch_mode)
    }
  }
  if (any(beta0<betamin)){
    #bad start too small
    if (if_estimate=="yes" | if_estimate_simulation=="yes"){
      message("INVALID parmInit VALUES FOUND IN PARAMETERS FILE.\nparmInit MUST SATISFY parmInit>=parmMin \nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
      if (batch_mode=="yes"){#if batch output message to log
        cat("INVALID parmInit VALUES FOUND IN PARAMETERS FILE.\nparmInit MUST SATISFY parmInit>=parmMin\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
      }
      errorOccurred("selectParmValues.R",batch_mode)
    }
  }
  if (any(beta0>betamax)){
    #bad start too big
    if (if_estimate=="yes" | if_estimate_simulation=="yes"){
      message("INVALID parmInit VALUES FOUND IN PARAMETERS FILE.\nparmInit MUST SATISFY parmInit<=parmMax \nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
      if (batch_mode=="yes"){#if batch output message to log
        cat("INVALID parmInit VALUES FOUND IN PARAMETERS FILE.\nparmInit MUST SATISFY parmInit<=parmMax\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
      }
      errorOccurred("selectParmValues.R",batch_mode)
    }
  }
  
  
  
  
  if (any(betamin>betamax)){
    #min>max
    if (if_estimate=="yes" | if_estimate_simulation=="yes"){
      message("INVALID parmMin/parmMax VALUES FOUND IN PARAMETERS FILE.\nparmMin MUST BE LESS THAN parmMax\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
      if (batch_mode=="yes"){#if batch output message to log
        cat("INVALID parmMin/parmMax VALUES FOUND IN PARAMETERS FILE.\nparmMin MUST BE LESS THAN parmMax\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
      }
      errorOccurred("selectParmValues.R",batch_mode)
    }
  }
  if (all(beta0==0)){
    #parmInit == 0
    if (if_estimate=="yes" | if_estimate_simulation=="yes"){
      message("INVALID parmInit VALUES FOUND IN PARAMETERS FILE.\nparmInit MUST NOT EQUAL ZERO FOR SELECTED PARAMETERS\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
      if (batch_mode=="yes"){#if batch output message to log
        cat("INVALID parmInit VALUES FOUND IN PARAMETERS FILE.\nparmInit MUST NOT EQUAL ZERO FOR SELECTED PARAMETERS\nEDIT PARAMETERS FILE TO RUN ESTIMATION\nRUN EXECUTION TERMINATED.")
      }
      errorOccurred("selectParmValues.R",batch_mode)
    }
  }
  
  
  return(SelParmValues)
  
}#end function


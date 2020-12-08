#'@title checkingMissingVars
#'@description check for missing variables in the variable lists specified by `types` \\cr \\cr
#'Executed By: \\itemize\{\\item checkAnyMissingSubdataVars.R
#'             \\item checkMissingData1Vars.R
#'             \\item checkMissingSubdataVars.R\} \\cr
#'Executes Routines: getVarList.R \\cr
#'@param checkData data.frame in which to check for missing values
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param betavalues data.frame of model parameters from parameters.csv
#'@param types character vector of names of variable lists to check for missing values
#'@param allMissing TRUE/FALSE indicating whether to flag 'any missing values' or 'all missing 
#'       values'
#'@param returnData TRUE/FALSE indicated whether the data checked for missing values should be 
#'       returned as part of the `missing` list output
#'@return `missing` list of number of missing variables `k`, names of variables with missing 
#'            values `datalstMissingdata` and the type of variable list with missing values



checkingMissingVars<-function(checkData, data_names, betavalues,  types , allMissing, returnData){
  
  data<-checkData
  missing<-list()
  #check missing in (getVarList()$varList)
  if (length(grep("datalstCheck",types))==1){
    datalstCheck <- as.character(getVarList()$varList)
    datalstMissingdata <- rep(0,length(datalstCheck))
    k<-0
    for (i in 1:length(datalstCheck)) { 
      if((datalstCheck[i] %in% colnames(data))) {   # check for existence of variable in data1
        
        if (allMissing){
          dname <- paste0("if(all(is.na(data$",datalstCheck[i],")) | all(data$",datalstCheck[i]," == 0))
                     { k<-k+1; datalstMissingdata[k] <- datalstCheck[i] }")  
        }else{
          dname <- paste0("if(sum(is.na(data$",datalstCheck[i],"))>0)
                   { k<-k+1; datalstMissingdata[k] <- datalstCheck[i] }")  
        }
        eval(parse(text=dname))  # tag as all values missing
        
      } else {  # check for all missing values
        
        dname <- paste0("data$",datalstCheck[i],"<-NA") 
        eval(parse(text=dname))  # place missing variable in data1 object
        k<-k+1
        datalstMissingdata[k] <- datalstCheck[i]  # list missing variable
      }
    }
    missing$k<-k
    missing$datalstMissingdata<-datalstMissingdata
    missing$datalstCheck<-datalstCheck
  }# end datalstCheck
  
  
  # Check user-selected parameters in SUBDATA for all NAs
  if (length(grep("xlnames",types))==1){
    xlnames <- betavalues$sparrowNames[betavalues$parmMax != 0]
    for (i in 1:length(xlnames)) { 
      if (allMissing){
        dname <- paste0("if(all(is.na(data$",xlnames[i],")) | 
                     all(data$",xlnames[i]," == 0))
                     { k<-k+1; datalstMissingdata[k] <- xlnames[i] }")   
      }else{
        dname <- paste0("if(sum(is.na(data$",xlnames[i],"))>0)
                   { k<-k+1; datalstMissingdata[k] <- xlnames[i] }")  
      }
      eval(parse(text=dname))  # tag as all values missing
    }
    missing$k<-k
    missing$datalstMissingdata<-datalstMissingdata
    missing$xlnames<-xlnames
  }# end parameters
  
  
  # Check sparrowNames from varnames in SUBDATA for all NAs
  if (length(grep("vrnames",types))==1){
    vrnames <- data_names$sparrowNames
    for (i in 1:length(vrnames)) { 
      dname <- paste0("if(all(is.na(data$",vrnames[i],")) | 
                     all(data$",vrnames[i]," == 0))
                     { k<-k+1; datalstMissingdata[k] <- vrnames[i] }")   
      eval(parse(text=dname))  # tag as all values missing
    }
    missing$k<-k
    missing$datalstMissingdata<-datalstMissingdata
    missing$vrnames<-vrnames
  }#end varnames
  
  if(returnData){
    missing$data<-data
  }
  
  return(missing)
  
}#end function

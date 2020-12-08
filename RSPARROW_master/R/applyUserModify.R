#'@title applyUserModify
#'@description reads `userModifyData.R` control file as text, unpacks all variables in the 
#'            data1 file and applies user modifications, creates the `subdata` object for use in all model 
#'            execution statements \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'Executes Routines: \\itemize\{\\item named.list.R
#'             \\item replaceNAs.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param betavalues data.frame of model parameters from parameters.csv
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param subdata data.frame input data (subdata)
#'@param class_landuse character vector of class_landuses from sparrow_control.R
#'@param lon_limit User specified geographic units minimum/maximum longitude limits for 
#'       mapping of Residuals and prediction maps
#'@return `subdata`  data.frame with all user designated modifications from userModifyData.R



applyUserModify<-function(file.output.list,
                          #modifySubdata arguments
                          betavalues,data_names,subdata,class_landuse,lon_limit){
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  pathToUserMod<-paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,"userModifyData.R")
  
  #read userModifyData file as text
  userMod <- readLines(pathToUserMod)
  
  #header text
  top<-"modifySubdata <- function(betavalues,data_names,subdata,class_landuse,lon_limit,
                                  file.output.list) {
  
unPackList(list=list(datalstreq=data_names$sparrowNames,
                     betavalues=betavalues$sparrowNames,
                    file.output.list = file.output.list),
          list(subdata = subdata,
               subdata = subdata,
              NA))

#datalstreq <- data_names$sparrowNames
#for (i in 1:length(datalstreq)) {
#  dname <- paste('subdata$',datalstreq[i],sep='')
#  x1name <- paste(datalstreq[i],sep='')
#  if((x1name %in% names(subdata)) == TRUE) {
#  assign(x1name,eval(parse(text=dname)))
#  }
#}

# Assign parameter variables to global variables in function
#  Checks for existence and transfers variables from 'subdata'

#for (i in 1:length(betavalues$sparrowNames)) {
#dname <- paste('subdata$',betavalues$sparrowNames[i],sep='') 
#x1name <- paste(betavalues$sparrowNames[i],sep='')
#if((x1name %in% names(subdata)) == TRUE) {
#assign(betavalues$sparrowNames[i],eval(parse(text=dname)))
#}
#}
  "
  
  #footer text
  bottom<-  "
    
#check for missing landuse class
missingLanduseClass<-class_landuse[which(!class_landuse %in% data_names$sparrowNames)]
  if (length(na.omit(missingLanduseClass))!=0){
  for (i in 1:length(missingLanduseClass)){
  cat('\n FATAL ERROR : MISSING class_landuse : ',missingLanduseClass[i],'\n ',sep='')
  cat('\n \n')
  }
  }
  
  # substitute 0.0 for NAs for user-selected parameters
  # set NAs for explanatory variables associated with the selected parameters
  eval(parse(text=paste('replaceNAs(named.list(',paste(paste('\"',betavalues$sparrowNames[betavalues$parmMax != 0],'\"',sep=''),collapse=','),'))',sep='')))
  
  # Transfer global variables to SUBDATA
  
  # Refresh variables in 'subdata' (this allows subsequent use of subdata values)
  #  (accounts for any modification to these variables to replace NAs or
  #   following calculations in the data modifications section)
  datalstreq <- data_names$sparrowNames
  for (i in 1:length(datalstreq)) {
  dname <- paste('subdata$',datalstreq[i],' <- ',datalstreq[i],sep='') 
  eval(parse(text=dname)) 
  }
  
  # Ensure that variables associated with user-selected parameters are reassigned to SUBDATA 
  for (i in 1:length(betavalues$sparrowNames)) {
  if(betavalues$parmMax[i]>0){
  dname <- paste('subdata$',betavalues$sparrowNames[i],' <- ',betavalues$sparrowNames[i],sep='') 
  eval(parse(text=dname)) 
  }
  }
  
  return(subdata) 
  }"
  
  
  
  #add header and footers
  userMod<-paste(top,"\n", paste(userMod,collapse="\n"),"\n", bottom)
  
  #evaluate modifySubdata as text
  eval(parse(text = userMod))
  
  #create subdata
  subdata<-modifySubdata(betavalues,data_names,subdata,class_landuse,lon_limit,
                         file.output.list)
  
  #return subdata
  return(subdata)
  
}

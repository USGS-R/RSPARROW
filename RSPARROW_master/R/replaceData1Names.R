#'@title replaceData1Names
#'@description replaces data1UserNames with sparrowNames in the data1 object from the 
#'            dataDictionary.csv control file \\cr \\cr
#'Executed By: dataInputPrep.R \\cr
#'Executes Routines: getVarList.R \\cr
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param data1 input data (data1)
#'@return `data1` input data (data1) with data1UserNames replaced with sparrowNames



replaceData1Names <- function(data_names,data1) {
  
  
  datalstreq <- data_names$sparrowNames
  datalstin <- data_names$data1UserNames 
  datalstunits <- data_names$varunits
  
  # replace variable names in the DATA1 object (datalstin) with the required names (datalstreq)
  # assists with cases where DATA1 is missing a required or user-specified variable 
  #  (a warning will be printed for this variable with all NAs inserted)
  data1_names <- names(data1) 
  for (i in 1:length(datalstreq)) {  
    ick <- 0
    for (j in 1:length(data1_names)) {
      if(!is.na(datalstin[i])) {
        if(datalstin[i] == data1_names[j]) {ick<-ick+1}   # compare required name in DATA1 file with actual DATA1 names
      }
    }
    if(ick == 0) {
      dname <- paste0("data1$",datalstreq[i],"<-NA") 
      eval(parse(text=dname))  # place missing variable in data1 object
    }
  }
  
  data1_names <- names(data1)     # reset data1 (DATA1) names
  for (i in 1:length(data1_names)) {
    for (j in 1:length(datalstin)) {
      if(!is.na(datalstin[j])) {
        if (data1_names[i] == datalstin[j]) {
          data1_names[i] <- datalstreq[j]
        } 
      }
    }
  }
  colnames(data1) <- data1_names
  data1$station_id <- as.character(data1$station_id)
  data1$station_name <- as.character(data1$station_name)
  data1$rchname <- as.character(data1$rchname)
  
  # If 'calsites' missing or has all NAs/0s, then set calibration site index to default setting:  '1' for 'depvar>0'
  
  if(("calsites" %in% names(data1)) == TRUE) {    # index present
    if(all(is.na(data1$calsites) == TRUE) | all(data1$calsites == 0) == TRUE) { # all values of index equal NAs or 0
      data1$calsites <- ifelse(data1$depvar > 0,1,0)
    }
  } else {   # index missing; set to default
    data1$calsites <- ifelse(data1$depvar > 0,1,0)
  }
  
  
  #if names in required/fixed list make tolower()
  names(data1)[which(tolower(names(data1)) %in% as.character(getVarList()$varList))]<-tolower(names(data1)[which(tolower(names(data1)) %in% as.character(getVarList()$varList))])
  
  #save waterid as original name in both data1 and varnames
  origWaterid<-data_names[which(data_names$sparrowNames=="waterid"),]
  origWaterid$sparrowNames<-"waterid_for_RSPARROW_mapping"
  origWaterid$varType<-"OPEN"
  origWaterid<-origWaterid[,match(names(data_names),names(origWaterid))]
  data_names<-rbind(data_names,origWaterid)
  assign("data_names",data_names,envir = .GlobalEnv)
  
  
  origWaterid<-data.frame(waterid_for_RSPARROW_mapping = data1$waterid)
  data1<-cbind(data1,origWaterid)
  
  
  
  return(data1)
  
}#end function


#'@title startEndmodifySubdata
#'@description if userModifyData.R is not run, this function creates the subdata object and 
#'            stops execution if any missing class_landuse found \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param class_landuse character vector of class_landuses from sparrow_control.R
#'@param data1 input data (data1)
#'@return `subdata` data.frame input data (subdata)



startEndmodifySubdata<-function(data_names,class_landuse, data1){
  
  
  #check for missing landuse class
  missingLanduseClass<-class_landuse[which(!class_landuse %in% data_names$sparrowNames)]
  if (length(na.omit(missingLanduseClass))!=0){
    for (i in 1:length(missingLanduseClass)){
      cat("\n FATAL ERROR : MISSING class_landuse : ",missingLanduseClass[i],"\n ",sep="")
      cat("\n \n")
    }
  }
  
  
  
  return(data1)
  
}#end function

#'@title createSubdataSorted
#'@description Creates a subset of `data1`, called `subdata`, sorted by `hydseq` , based on 
#'            the application of the control setting 'filter_data1_conditions'. The 'subdata' object is used in 
#'            model estimation, prediction, mapping, and other functions.  \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'@param filter_data1_conditions User specified additional DATA1 variables (and conditions) to 
#'       be used to filter reaches from sparrow_control
#'@param data1 input data (data1)
#'@return `subdata`  data.frame used in model execution



createSubdataSorted <- function(filter_data1_conditions,data1) {
  
  
  if(is.na(filter_data1_conditions)) {
    data1$fnode[is.na(data1$fnode)] <- 0
    data1$tnode[is.na(data1$tnode)] <- 0
    dname <- paste0("subdata <- data1[(data1$fnode > 0 & data1$tnode > 0), ]") 
    eval(parse(text=dname))   # create subdata            
  } else {
    data1$fnode[is.na(data1$fnode)] <- 0
    data1$tnode[is.na(data1$tnode)] <- 0
    dname1 <- "subdata <- data1[(data1$fnode > 0 & data1$tnode > 0 "
    for (i in 1:length(filter_data1_conditions)) {
      dname1 <- paste0(dname1,"& ",filter_data1_conditions[i]," ") 
    }
    dname <- paste0(dname1,"), ]") 
    eval(parse(text=dname))   # create subdata 
  }
  
  # Sort SUBDATA by HYDSEQ
  subdata <- subdata[with(subdata,order(subdata$hydseq)), ]        # removed secondary sort by waterid (1-8-2017)
  
  
  
  return(subdata)
  
}#end function

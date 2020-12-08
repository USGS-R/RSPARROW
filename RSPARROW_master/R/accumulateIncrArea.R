#'@title accumulateIncrArea
#'@description Accumulates incremental area for `accum_elements` variables, expressed in areal 
#'            units. Function can only be run after replaceData1Names.R has been executed. Uses reach 
#'            navigation data. \\cr \\cr
#'Executed By: \\itemize\{\\item calcClassLandusePercent.R
#'             \\item createVerifyReachAttr.R
#'             \\item hydseq.R\} \\cr
#'@param indata data.frame containing reach navigation variables and variables to be selected 
#'       for incremental area accumulation
#'@param accum_elements character vector indicating variables to be selected for incremental 
#'       area accumulation
#'@param accum_names character vector of final names for accumlated data
#'@return `AreaRch`  data.frame containing waterid and `accum_elements`



accumulateIncrArea<-function(indata,accum_elements,accum_names){
  
  #get data
  data<-indata
  
  #replace any NAs with 0
  for (c in c("termflag","fnode","tnode",accum_elements)){
    eval(parse(text = paste0("data$",c,"<-ifelse(is.na(data$",c,"),0,data$",c,")")))
  }
  
  data <- data[which(data$fnode > 0 & data$tnode > 0 & data$termflag != 3), ]
  
  #order by hydrologic sequence
  data<-data[order(data$hydseq),]
  
  #get relevant variables
  maxArc<-max(c(data$fnode,data$tnode))
  nr<-length(data$waterid)
  iup<-data$fnode
  idown<-data$tnode
  frac<-data$frac
  reach<-data$waterid
  
  #loop through accum_elements
  for (e in accum_elements){
    area<-eval(parse(text = paste0("data$",e)))
    
    carea<-rep(0,maxArc)
    arearch<-rep(0,nr)
    
    for (i in 1:nr){
      carea[idown[i]]<-carea[idown[i]] + (frac[i] * carea[iup[i]] + area[i])
      arearch[i] <- frac[i] * carea[iup[i]] + area[i]
    }
    
    if (e==accum_elements[1]){
      AreaRch<-data.frame(temp = arearch)
      names(AreaRch)<-accum_names[which(accum_elements==e)]
    }else{
      AreaRch<-as.data.frame(cbind(AreaRch,data.frame(temp = arearch)))
      names(AreaRch)[length(AreaRch)]<-accum_names[which(accum_elements==e)]                       
    }
    
  }
  
  #add waterid for merging
  AreaRch$waterid<-reach
  
  return(AreaRch)
  
}#end function

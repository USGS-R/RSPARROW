#'@title createVerifyReachAttr
#'@description Calculates REQUIRED system reach attributes for reaches with `fnode>0`, 
#'            `tnode>0` and `termflag!=3`from `calculate_reach_attribute_list` control setting and verifies 
#'            `demtarea` if `if_verify_demtarea<-"yes"` \\cr \\cr
#'Executed By: dataInputPrep.R \\cr
#'Executes Routines: \\itemize\{\\item accumulateIncrArea.R
#'             \\item calcHeadflag.R
#'             \\item calcTermflag.R
#'             \\item errorOccurred.R
#'             \\item hydseq.R
#'             \\item unPackList.R
#'             \\item verifyDemtarea.R\} \\cr
#'@param if_verify_demtarea specify whether or not to verify demtarea
#'@param calculate_reach_attribute_list list of attributes to calculate
#'@param data1 input data (data1)
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, 
#'                          polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, CRStext, 
#'                          convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, 
#'                          if_verify_demtarea_maps
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@return `data1`  data.frame with calculated reach attributes replaced



createVerifyReachAttr <- function(if_verify_demtarea,calculate_reach_attribute_list,data1,
                                  file.output.list,mapping.input.list,
                                  batch_mode) {
  
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  # Select reaches to be included in the analysis (exclude coastal shorelines)
  # NAs removed first or will create NA records in 'sub1'
  for (c in c("termflag","fnode","tnode","demiarea","demtarea")){
    eval(parse(text = paste0("data1$",c,"<-ifelse(is.na(data1$",c,"),0,data1$",c,")")))
  }
  
  sub1 <- data1[(data1$fnode > 0 & data1$tnode > 0 & data1$termflag != 3), ]
  
  if(!is.na(calculate_reach_attribute_list) == TRUE ){  
    
    if (nrow(sub1)==0){
      cat("\n \n")
      message(paste0("HYDSEQ VARIABLE CANNOT BE CALCULATED\n DATASET WITH FNODE>0, TNODE>0 and TERMFLAG!=0 IS EMPTY\nRUN EXECUTION TERMINATED."))
      if (batch_mode=="yes"){#if batch output message to log
        cat("HYDSEQ VARIABLE CANNOT BE CALCULATED\n DATASET WITH FNODE>0, TNODE>0 and TERMFLAG!=0 IS EMPTY\nRUN EXECUTION TERMINATED.",sep="")
      }
      #stop execution
      errorOccurred("createVerifyReachAttr.R",batch_mode)
    }else{
      
      nreach <- length(sub1$waterid)
      
      
      #calculate reach attributes if on the list
      if (length(grep("hydseq",calculate_reach_attribute_list))!=0){ 
        
        #calculate hydseq variable, also headflag and demtarea if called for
        hydseq_data <- hydseq(sub1,calculate_reach_attribute_list)
        
        waterid <- hydseq_data$waterid
        hydseq <- hydseq_data$hydseq
        headflag <- hydseq_data$headflag
        demtarea <- hydseq_data$demtarea
        
        #calculate termflag if called for
        #if (length(grep("termflag",calculate_reach_attribute_list))!=0){
        #   termflag_new<-calcTermflag(sub1)
        #   termflag_new<-termflag_new[match(sub1$waterid,termflag_new$waterid),]
        #   termflag<-termflag_new$termflag
        # }else{
        #   termflag<-data1$termflag
        # }
        
      }else if (length(grep("demtarea",calculate_reach_attribute_list))!=0){
        waterid<-sub1$waterid
        hydseq<-sub1$hydseq
        
        #calculate headflag if called for
        if (length(grep("headflag",calculate_reach_attribute_list))!=0){
          headflag_new<-calcHeadflag(sub1)
          headflag_new<-headflag_new[match(sub1$waterid,headflag_new$waterid),]
          headflag<-headflag_new$headflag
        }else{
          headflag<-data1$headflag
        }
        
        #calculate termflag if called for
        # if (length(grep("termflag",calculate_reach_attribute_list))!=0){
        #    termflag_new<-calcTermflag(sub1)
        #    termflag_new<-termflag_new[match(sub1$waterid,termflag_new$waterid),]
        ##    termflag<-termflag_new$termflag
        #  }else{
        #    termflag<-data1$termflag
        #  }
        
        #calculate demtarea  
        demtarea_new<-accumulateIncrArea(sub1,c("demiarea"),c("demtarea"))
        demtarea_new<-demtarea_new[match(sub1$waterid,demtarea_new$waterid),]
        demtarea<-demtarea_new$demtarea
        
      }else if (length(grep("headflag",calculate_reach_attribute_list))!=0){
        waterid<-sub1$waterid
        hydseq<-sub1$hydseq
        demtarea<-sub1$demtarea
        
        #calculate headflag
        headflag_new<-calcHeadflag(sub1)
        headflag_new<-headflag_new[match(sub1$waterid,headflag_new$waterid),]
        headflag<-headflag_new$headflag
        
        #calculate termflag if called for
        #   if (length(grep("termflag",calculate_reach_attribute_list))!=0){
        #      termflag_new<-calcTermflag(sub1)
        #      termflag_new<-termflag_new[match(sub1$waterid,termflag_new$waterid),]
        #      termflag<-termflag_new$termflag
        #    }else{
        #      termflag<-data1$termflag
        #    }
        
      }else if (length(grep("termflag",calculate_reach_attribute_list))!=0){
        waterid<-sub1$waterid
        hydseq<-sub1$hydseq
        demtarea<-sub1$demtarea
        
        #calculate termflag
        # termflag_new<-calcTermflag(sub1)
        #  termflag_new<-termflag_new[match(sub1$waterid,termflag_new$waterid),]
        #  termflag<-termflag_new$termflag
      }
      
      #verifyDemtarea
      compareData<-data.frame(waterid=waterid,
                              hydseq=hydseq,
                              headflag=headflag,
                              demtarea=demtarea)
      
      verifyDemtarea(if_verify_demtarea,sub1,compareData,
                     #for checkDrainageErrors
                     file.output.list,mapping.input.list,
                     #for both
                     batch_mode)
      
      #replace reach attributes   
      # remove attributes from data1
      drops <- calculate_reach_attribute_list
      data1 <- data1[ , !(names(data1) %in% drops)]
      
      # add attributes to data1
      for (i in 1:length(calculate_reach_attribute_list)) {
        name1 <- paste0("hs_data <- data.frame(waterid,",calculate_reach_attribute_list[i],")")
        eval(parse(text=name1))
        hs_data <- hs_data[hs_data$waterid != 0, ] # eliminate 0 cases where vector dimension max > no. reaches
        data1 <- merge(data1,hs_data,by="waterid",all.y=TRUE,all.x=TRUE)
      }
      
      
      
    }#if no error
    
  }else{#if no calc_reach_attr
    if (if_verify_demtarea=="yes"){
      waterid <- sub1$waterid
      hydseq <- sub1$hydseq
      headflag <- sub1$headflag
      
      #calculate demtarea  
      demtarea_new<-accumulateIncrArea(sub1,c("demiarea"),c("demtarea"))
      demtarea_new<-demtarea_new[match(sub1$waterid,demtarea_new$waterid),]
      demtarea<-demtarea_new$demtarea
      
      #verifyDemtarea
      compareData<-data.frame(waterid=waterid,
                              hydseq=hydseq,
                              headflag=headflag,
                              demtarea=demtarea)
      
      verifyDemtarea(if_verify_demtarea,sub1,compareData,
                     #for checkDrainageErrors
                     file.output.list,mapping.input.list,
                     #for both
                     batch_mode)
      
    }#end if_verify_demtarea
  }#end if no calc reach attr
  
  return(data1)
  
}#end function





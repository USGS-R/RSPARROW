#'@title verifyDemtarea
#'@description Performs a consistency check on values for the total drainage area system 
#'            variable, 'demtarea', based on the 'if_verify_demtarea' control setting (section 2 of the control 
#'            script). The check compares values of the newly-created 'demtarea' (using the 'hydseq.R' 
#'            function) with user-supplied values of 'demtarea', and outputs the results to CSV and PDF files. \\cr \\cr
#'Executed By: createVerifyReachAttr.R \\cr
#'Executes Routines: \\itemize\{\\item checkDrainageareaErrors.R
#'             \\item unPackList.R\} \\cr
#'@param if_verify_demtarea specify whether or not to verify demtarea
#'@param data1 input data (data1)
#'@param compareData data.frame of reach verification attributes to compare with the users 
#'       data1 file
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



verifyDemtarea<-function(if_verify_demtarea,data1,compareData,
                         #for checkDrainageErrors
                         file.output.list,mapping.input.list,
                         #for both
                         batch_mode){
  
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  if (if_verify_demtarea=="yes"){
    
    # Select reaches to be included in the analysis (exclude coastal shorelines)
    # NAs removed first or will create NA records in 'sub1'
    for (c in c("termflag","fnode","tnode","demiarea","demtarea")){
      eval(parse(text = paste0("data1$",c,"<-ifelse(is.na(data1$",c,"),0,data1$",c,")")))
    }
    
    sub1 <- data1[(data1$fnode > 0 & data1$tnode > 0 & data1$termflag != 3), ]
    
    #get compareData as vectors
    waterid<-compareData$waterid
    hydseq<-compareData$hydseq
    demtarea<-compareData$demtarea
    headflag<-compareData$headflag
    
    #verifyDemtarea
    
    demtarea_new <- demtarea
    hydseq_new <- hydseq
    headflag_new <- headflag
    name1 <- paste0("hs_data <- data.frame(waterid,demtarea_new,hydseq_new,headflag_new)")
    eval(parse(text=name1))
    hs_data <- hs_data[hs_data$waterid != 0, ] # eliminate 0 cases where vector dimension max > no. reaches
    sub1 <- merge(sub1,hs_data,by="waterid",all.y=TRUE,all.x=TRUE)
    sub1.plot <- qplot(sub1$demtarea,sub1$demtarea_new,log="xy",
                       xlab="Pre-calculated Total Drainage Area",ylab="Newly-calculated Total Drainage Area",
                       geom=c("point"),main="Comparison of Total Drainage Areas")
    
    
    sub1$AreaRatio_NewOld <- sub1$demtarea_new / sub1$demtarea
    sub1$Headflag_NewOld <- as.character(ifelse(sub1$headflag == sub1$headflag_new,"  ","DIFFER"))
    sub1$AreaRatio_NewOld[is.na(sub1$AreaRatio_NewOld)] <- 0     # NAs removed first or will create NA records in 'sub1'
    DAreaFailCheckObj <- sub1[(sub1$AreaRatio_NewOld < 0.99 | sub1$AreaRatio_NewOld > 1.01), ]
    if(nrow(DAreaFailCheckObj) > 0) {
      DAreaFailCheckMessage <- 
        paste0("Number of reaches with different (>1%) total drainage areas (see records in DAreaFailCheckObj): ",
              nrow(DAreaFailCheckObj))
      
    }else{
      DAreaFailCheckMessage<-" "
    }
    
    #output results
    if (DAreaFailCheckMessage!=" "){
      message("Reporting checks of total drainage area...")
      message(DAreaFailCheckMessage)
      cat("\n\n")
      if (batch_mode=="yes"){
        cat(DAreaFailCheckMessage)
        cat("\n\n") 
      }
    } else {
      message("No errors found in checks of total drainage area. Area differences are <1%.")
    }
    if(DAreaFailCheckMessage!=" ") {    # Map mis-matched reaches and diagnostics
      if(nrow(DAreaFailCheckObj) > 0) {
        
        message("Writing results from drainage area comparisons (CSV, HTML maps) in estimate directory...")
        
        checkDrainageareaErrors(file.output.list,mapping.input.list,
                                #sub1.plot,
                                DAreaFailCheckObj,data1, 
                                batch_mode)
      }
    }
    
  }#end if_verify 
  
  
  
  
}#end function

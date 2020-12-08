#'@title checkDrainageareaErrors
#'@description Executes drainage area checks for newly computed areas, based on the 
#'            if_verify_demtarea control setting (section 2 of the control script). If any differences are found 
#'            between the user's original data for Total Drainage Area vs. Total Drainage Area calculated by 
#'            RSPARROW, a plot of user's original data for Total Drainage Area vs. Total Drainage Area calculated 
#'            by RSPARROW is output. For the control setting if_verify_demtarea_maps<-"yes", maps are output 
#'            of `demtarea` and `hydseq` for unmatched areas as a ratio of RSPARROW calculated:original. A CSV 
#'            file is output of all differences found to ~/estimate/(run_id)_diagnostic_darea_mismatches.csv. \\cr \\cr
#'Executed By: verifyDemtarea.R \\cr
#'Executes Routines: \\itemize\{\\item checkBinaryMaps.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, 
#'                          polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, CRStext, 
#'                          convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, 
#'                          if_verify_demtarea_maps
#'@param DAreaFailCheckObj data.frame of all rows of subdata in which the user's original data 
#'       for Total Drainage Area vs. Total Drainage Area calculated by RSPARROW differ
#'@param data1 input data (data1)
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



checkDrainageareaErrors <- function(file.output.list,mapping.input.list,
                                    #sub1.plot,
                                    DAreaFailCheckObj,data1,
                                    batch_mode) {
  
  # Setup variable lists 
  # create global variable from list names (mapping.input.list)
  unPackList(lists = list(file.output.list = file.output.list,
                          mapping.input.list = mapping.input.list),
             parentObj = list(NA, NA)) 
  
  filename <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_diagnostic_darea_mismatches.pdf")
  if (length(na.omit(DAreaFailCheckObj$demtarea))!=0){
    
    if (if_verify_demtarea_maps=="yes"){
      #get geoLines
      existGeoLines<-checkBinaryMaps(LineShapeGeo,path_gis,batch_mode)
      
      #get lineShape
      existlineShape<-checkBinaryMaps(lineShapeName,path_gis,batch_mode)
      
      commonvar <- lineWaterid
      
      ##############################################################
      # Loop through variable list
      
      #if (!is.na(LineShapeGeo) & !is.na(lineShapeName)) {  # map if shape files available
      if (existGeoLines & existlineShape){# map if shape files available
        
        map.vars.list <- c("demtarea","hydseq","hydseq_new","AreaRatio_NewOld")
        title_name <- c("Pre-calculated DEMTAREA","HYDSEQ","new HYDSEQ for unmatched areas","AreaRatio_New:Old")
        
        
          path_checkDrainageareaErrorsChild<-file_path_as_absolute(paste0(path_master,"checkDrainageareaErrorsChild.Rmd"))
          
          reportPath<-paste0(path_master,"checkDrainageareaErrors.Rmd")
            
          filename<-gsub("pdf","html",filename)


            
            rmarkdown::render(paste0(path_master,"checkDrainageareaErrors.Rmd"),
              params = list(
                file.output.list = file.output.list,
                mapping.input.list = mapping.input.list,
                DAreaFailCheckObj = DAreaFailCheckObj, 
                data1 = data1,
                existGeoLines = existGeoLines,
                commonvar = commonvar,
                map.vars.list = map.vars.list,
                GeoLines = GeoLines,
                lineShape = lineShape,
                title_name = title_name,
                filename = filename,
                path_checkDrainageareaErrorsChild = path_checkDrainageareaErrorsChild
              ),
              output_file = filename, quiet = TRUE
            )
            
        #}#else plotly
      } # execute if shape files exist
    }
    
    
    #popup pdf of plot
    shell.exec(filename)
  }#end if all missing original demtarea
  
  # Output mis-matched reach data
  waterid <- DAreaFailCheckObj$waterid
  fnode_pre <- DAreaFailCheckObj$fnode
  tnode_pre <- DAreaFailCheckObj$tnode
  frac_pre <- DAreaFailCheckObj$frac
  demtarea_pre <- DAreaFailCheckObj$demtarea
  demtarea_post <- DAreaFailCheckObj$demtarea_new
  hydseq_new <- DAreaFailCheckObj$hydseq_new
  headflag_new <- DAreaFailCheckObj$headflag_new
  headflag_check <- DAreaFailCheckObj$Headflag_NewOld
  AreaRatio_NewOld <- DAreaFailCheckObj$AreaRatio_NewOld
  
  origWaterid<-data1[,which(names(data1) %in% c("waterid","waterid_for_RSPARROW_mapping"))]
  origWaterid<-origWaterid[which(origWaterid$waterid %in% waterid),]
  origWaterid<-origWaterid[order(match(origWaterid$waterid,waterid)),]
  origWaterid<-origWaterid$waterid_for_RSPARROW_mapping
  
  pout <- data.frame(waterid,origWaterid,fnode_pre,tnode_pre,frac_pre,demtarea_pre,demtarea_post,hydseq_new,
                     AreaRatio_NewOld,headflag_new,headflag_check)
  fileout <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_diagnostic_darea_mismatches.csv")
  write.table(pout,file=fileout,row.names=F,append=F,quote=F,
              dec=csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE)
  
  
}#end function

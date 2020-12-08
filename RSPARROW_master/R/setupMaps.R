#'@title setupMaps
#'@description A preprocessor to read shape files and store objects \\cr \\cr
#'Executed By: executeRSPARROW.R \\cr
#'Executes Routines: \\itemize\{\\item batchGeoLines.R
#'             \\item batchlineShape.R
#'             \\item batchpolyShape.R
#'             \\item outputSettings.R
#'             \\item unPackList.R\} \\cr
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
#'@param RSPARROW_errorOption yes/no control setting indicating where the RPSARROW_errorOption 
#'                            should be applied 



setupMaps <- function(file.output.list,mapping.input.list,batch_mode,RSPARROW_errorOption) {
  
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  # Setup variable lists 
  # create global variable from list names (mapping.input.list)
  for(i in 1:length(mapping.input.list)){
    tempobj=mapping.input.list[[i]]
    eval(parse(text=paste(names(mapping.input.list)[[i]],"= tempobj")))
  }
  if (length(na.omit(convertShapeToBinary.list))!=0){
    
    save(list = c(as.character(outputSettings(file.output.list,FALSE)$setting),
                  ls()[which(regexpr("path_",ls())>0)],"path_gis","RSPARROW_errorOption",
                  ls()[which(regexpr("file_",ls())>0)],"mapping.input.list"),
         file=paste0(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"batch.RData"))
    
    # Setup GEOLINES 
    if("LineShapeGeo" %in% convertShapeToBinary.list & !is.na(LineShapeGeo)) {

        message("Creating GeoLines binary file...")
        system(paste0(Sys.which("Rscript.exe")," ",file.path(paste0(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"batchGeoLines.R"))), wait = TRUE, invisible = TRUE)
      
    } 
    
    # STREAMS
    # Read shape files and reproject as lat-lon
    if("lineShapeName" %in% convertShapeToBinary.list & !is.na(lineShapeName)) {

        message("Creating lineShape binary file...")
        system(paste0(Sys.which("Rscript.exe")," ",file.path(paste0(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"batchlineShape.R"))), wait = TRUE, invisible = TRUE)

    }
    # CATCHMENTS
    # Read shape files and reproject as lat-lon
    if("polyShapeName" %in% convertShapeToBinary.list) {

        message("Creating polyShape binary file...")
        system(paste0(Sys.which("Rscript.exe")," ",file.path(paste0(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"batchpolyShape.R"))), wait = TRUE, invisible = TRUE)
        

    }  
    
    
    
    if (output_map_type=="both" & !is.na(master_map_list)[1]){
      if (!file.exists(paste0(path_gis,.Platform$file.sep,"lineShape"))){
        message("WARNING: lineShape BINARY FILE CREATED FROM lineShapeName IS NOT FOUND. \nSTREAM MAPS WILL NOT BE GENERATED\n ")
      }
      if (!file.exists(paste0(path_gis,.Platform$file.sep,"polyShape"))){
        message("WARNING: polyShape BINARY FILE CREATED FROM polyShapeName IS NOT FOUND. \nCATCHMENT MAPS WILL NOT BE GENERATED\n ")
      }
    }else if (output_map_type=="stream" & !file.exists(paste0(path_gis,.Platform$file.sep,"lineShape")) & !is.na(master_map_list)[1]){
      message("WARNING: lineShape BINARY FILE CREATED FROM lineShapeName IS NOT FOUND. \nSTREAM MAPS WILL NOT BE GENERATED\n ")
    }else if (output_map_type=="catchment" & !file.exists(paste0(path_gis,.Platform$file.sep,"polyShape")) & !is.na(master_map_list)[1]){
      message("WARNING: polyShape BINARY FILE CREATED FROM polyShapeName IS NOT FOUND. \nCATCHMENT MAPS WILL NOT BE GENERATED\n ")
    }
    
  }#if convertShapeToBinary.list !=NA    
  
  
}#end function

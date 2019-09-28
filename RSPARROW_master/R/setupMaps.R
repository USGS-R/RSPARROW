#'@title setupMaps
#'@description A preprocessor to read shape files and store objects \\cr \\cr
#'Executed By: executeRSPARROW.R \\cr
#'Executes Routines: \\itemize\{\\item batchGeoLines.R
#'             \\item batchlineShape.R
#'             \\item batchpolyShape.R
#'             \\item outputSettings.R
#'             \\item unPackList.R\} \\cr
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@param RSPARROW_errorOption 



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
         file=paste(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"batch.RData",sep=""))
    
    # Setup GEOLINES 
    if("LineShapeGeo" %in% convertShapeToBinary.list & !is.na(LineShapeGeo)) {
      if (file.exists(paste(path_gis,.Platform$file.sep,"GeoLines",sep=""))){
        message("WARNING: PREVIOUSLY GENERATED GeoLines BINARY FILE FOUND.
'LineShapeGeo' FROM converShapeToBinary.list NOT USED
TO GENERATE A NEW GeoLines FILE THE EXISTING FILE MUST BE DELETED FROM THE GIS DIRECTORY\n")
      }else{
        message("Creating GeoLines binary file...")
        system(paste(Sys.which("Rscript.exe")," ",file.path(paste(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"batchGeoLines.R",sep="")),sep=""), wait = TRUE, invisible = TRUE)
      }
    } 
    
    # STREAMS
    # Read shape files and reproject as lat-lon
    if("lineShapeName" %in% convertShapeToBinary.list & !is.na(lineShapeName)) {
      if (file.exists(paste(path_gis,.Platform$file.sep,"lineShape",sep=""))){
        message("WARNING: PREVIOUSLY GENERATED lineShape BINARY FILE FOUND.
'lineShapeName' FROM converShapeToBinary.list NOT USED
TO GENERATE A NEW lineShape FILE THE EXISTING FILE MUST BE DELETED FROM THE GIS DIRECTORY\n")
      }else{
        message("Creating lineShape binary file...")
        system(paste(Sys.which("Rscript.exe")," ",file.path(paste(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"batchlineShape.R",sep="")),sep=""), wait = TRUE, invisible = TRUE)
        
      }
    }
    # CATCHMENTS
    # Read shape files and reproject as lat-lon
    if("polyShapeName" %in% convertShapeToBinary.list) {
      if (file.exists(paste(path_gis,.Platform$file.sep,"polyShape",sep=""))){
        message("WARNING: PREVIOUSLY GENERATED polyShape BINARY FILE FOUND.
'polyShapeName' FROM converShapeToBinary.list NOT USED
TO GENERATE A NEW polyShape FILE THE EXISTING FILE MUST BE DELETED FROM THE GIS DIRECTORY\n")
      }else{
        message("Creating polyShape binary file...")
        system(paste(Sys.which("Rscript.exe")," ",file.path(paste(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"batchpolyShape.R",sep="")),sep=""), wait = TRUE, invisible = TRUE)
        
      }
    }  
    
    
    
    if (output_map_type=="both" & !is.na(master_map_list)[1]){
      if (!file.exists(paste(path_gis,.Platform$file.sep,"lineShape",sep=""))){
        message("WARNING: lineShape BINARY FILE CREATED FROM lineShapeName IS NOT FOUND. \nSTREAM MAPS WILL NOT BE GENERATED\n ")
      }
      if (!file.exists(paste(path_gis,.Platform$file.sep,"polyShape",sep=""))){
        message("WARNING: polyShape BINARY FILE CREATED FROM polyShapeName IS NOT FOUND. \nCATCHMENT MAPS WILL NOT BE GENERATED\n ")
      }
    }else if (output_map_type=="stream" & !file.exists(paste(path_gis,.Platform$file.sep,"lineShape",sep="")) & !is.na(master_map_list)[1]){
      message("WARNING: lineShape BINARY FILE CREATED FROM lineShapeName IS NOT FOUND. \nSTREAM MAPS WILL NOT BE GENERATED\n ")
    }else if (output_map_type=="catchment" & !file.exists(paste(path_gis,.Platform$file.sep,"polyShape",sep="")) & !is.na(master_map_list)[1]){
      message("WARNING: polyShape BINARY FILE CREATED FROM polyShapeName IS NOT FOUND. \nCATCHMENT MAPS WILL NOT BE GENERATED\n ")
    }
    
  }#if convertShapeToBinary.list !=NA    
  
  
}#end function

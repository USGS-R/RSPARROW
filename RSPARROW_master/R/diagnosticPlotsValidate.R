#'@title diagnosticPlotsValidate
#'@description Creates diagnostic plots and maps for validation sites output to 
#'            ~/estimate/(run_id)_validation_plots.pdf, and saves residual maps as shape files. \\cr \\cr
#'Executed By: estimate.R \\cr
#'Executes Routines: \\itemize\{\\item checkBinaryMaps.R
#'             \\item diagnosticMaps.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param class.input.list list of control settings related to classification variables
#'@param vsitedata.demtarea.class Total drainage area classification variable for validation 
#'                                sites.
#'@param vsitedata sitedata for validation. Calculated by `subdata[(subdata$vdepvar > 0
#'                 & subdata$vcalsites==1), ]`
#'@param vsitedata.landuse Land use for incremental basins for diagnostics for validation 
#'                         sites.
#'@param estimate.list list output from `estimate.R`
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, 
#'                          polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, CRStext, 
#'                          convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, 
#'                          if_verify_demtarea_maps
#'@param add_vars additional variables specified by the setting `add_vars` to be included in 
#'       prediction, yield, and residuals csv and shape files
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



diagnosticPlotsValidate <- function(file.output.list,class.input.list,vsitedata.demtarea.class,
                                    vsitedata,vsitedata.landuse,estimate.list,mapping.input.list,add_vars,
                                    batch_mode) {
  
  
  
  #########################
  # Create Global Variables
  #########################
  
  
  # create global variable from list names (mapping.input.list)
  # create global variable from list names (Mdiagnostics.list)
  unPackList(lists = list(mapping.input.list = mapping.input.list,
                          vMdiagnostics.list = estimate.list$vMdiagnostics.list,
                          file.output.list = file.output.list,
                          class.input.list = class.input.list),
             parentObj = list(NA,
                              NA,
                              NA,
                              NA))
  
  filename <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_validation_plots.html")
  reportPath<-paste0(path_master,"diagnosticPlotsNLLS.Rmd")
  
  
  
  path_diagnosticMapAttrChild <- file_path_as_absolute(paste0(path_master,"diagnosticMapAttrChild.Rmd"))
  path_diagnosticCorrChild <- file_path_as_absolute(paste0(path_master,"diagnosticCorrChild.Rmd"))
  path_diagnosticClassvarChild <- file_path_as_absolute(paste0(path_master,"diagnosticClassvarChild.Rmd"))
  path_diagnosticClassLandChild <- file_path_as_absolute(paste0(path_master,"diagnosticClassLandChild.Rmd"))
  path_diagnosticContiguousChild<- file_path_as_absolute(paste0(path_master,"diagnosticContiguousChild.Rmd"))
  path_diagnosticDiagMapChild<-file_path_as_absolute(paste0(path_master,"diagnosticDiagMapChild.Rmd"))
  
  #edit title of report
  reportTitle<-paste0(run_id,"_validation_plots")
  #read Rmd file as text
  x <- readLines(reportPath)
  #find where title is designated
  editthis<-x[which(regexpr("title:",gsub(" ","",x))>0)]
  #replace with current reportTitle
  y <- gsub( editthis, paste0("title: '",reportTitle,"'"), x )
  #overwrite the file
  cat(y, file=reportPath, sep="\n") 
  
  rmarkdown::render(paste0(path_master,"diagnosticPlotsNLLS.Rmd"),
    params = list(
      validation = TRUE,
      file.output.list = file.output.list,
      path_diagnosticMapAttrChild = path_diagnosticMapAttrChild,
      path_diagnosticCorrChild = path_diagnosticCorrChild,
      path_diagnosticClassvarChild = path_diagnosticClassvarChild,
      path_diagnosticClassLandChild = path_diagnosticClassLandChild,
      path_diagnosticContiguousChild = path_diagnosticContiguousChild,
      path_diagnosticDiagMapChild = path_diagnosticDiagMapChild,
      class.input.list = class.input.list,
      sitedata.demtarea.class = vsitedata.demtarea.class,
      sitedata = vsitedata,
      sitedata.landuse = vsitedata.landuse,
      estimate.list = estimate.list,
      mapping.input.list = mapping.input.list,
      Csites.weights.list = NA,
      Cor.ExplanVars.list = NA,
      data_names = data_names,
      add_vars = add_vars,
      batch_mode = batch_mode
    ),
    output_file = filename, quiet = TRUE
  )
  
 
    #output residuals shapefile
    if (outputESRImaps[3]=="yes"){
      Obsyield <- Obs / vsitedata$demtarea
      predictYield <- ppredict / vsitedata$demtarea
      origWaterid<-vsitedata$waterid_for_RSPARROW_mapping
      
      dd <- data.frame(vsitedata,origWaterid,Obs,ppredict,Obsyield,predictYield,pResids,pratio.obs.pred,xlat,xlon)
      
      keeps <- c("waterid","origWaterid","demtarea","rchname","station_id","station_name","staid",classvar[1],"Obs",
                 "ppredict","Obsyield","predictYield","pResids","pratio.obs.pred","xlat","xlon")
      
      validationResidShape <- dd[keeps]
      
      if (length(na.omit(add_vars))!=0){
        add_data<-data.frame(vsitedata[,which(names(vsitedata) %in% add_vars)])
        if (length(add_vars)==1){
          names(add_data)<-add_vars
        }
        validationResidShape<-cbind(validationResidShape,add_data)
      }
      
      validationResidShape <-SpatialPointsDataFrame(validationResidShape[,c("xlon","xlat")],validationResidShape[,which(!names(validationResidShape) %in% c("xlat","xlon"))],proj4string=CRS(CRStext))
      
      if (!dir.exists(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep))){
        dir.create(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep),showWarnings = FALSE)
      }
      if (!dir.exists(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep))){
        dir.create(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep),showWarnings = FALSE)
      }
      
      maptools::writeSpatialShape(validationResidShape,paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep,"validationResidShape"))
      cat(showWKT(proj4string(validationResidShape)),file=paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep,"validationResidShape.prj")) 
      
    }
    

  
  
}#end function

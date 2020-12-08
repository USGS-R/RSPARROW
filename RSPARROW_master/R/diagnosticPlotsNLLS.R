#'@title diagnosticPlotsNLLS
#'@description Creates diagnostic plots and maps output to 
#'            ~/estimate/(run_id)_diagnostic_plots.pdf, and saves residual maps as shape files. \\cr \\cr
#'Executed By: estimate.R \\cr
#'Executes Routines: \\itemize\{\\item checkBinaryMaps.R
#'             \\item diagnosticMaps.R
#'             \\item mapSiteAttributes.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param class.input.list list of control settings related to classification variables
#'@param sitedata.demtarea.class Total drainage area classification variable for calibration 
#'                               sites.
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0
#'                & subdata$calsites==1), ]`. The object contains the dataDictionary 
#'                ‘sparrowNames’ variables, with records sorted in hydrological 
#'                (upstream to downstream) order (see the documentation Chapter 
#'                sub-section 5.1.2 for details)
#'@param sitedata.landuse Land use for incremental basins for diagnostics.
#'@param estimate.list list output from `estimate.R`
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, 
#'                          polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, CRStext, 
#'                          convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, 
#'                          if_verify_demtarea_maps
#'@param Csites.weights.list regression weights as proportional to incremental area size
#'@param Cor.ExplanVars.list list output from `correlationMatrix.R`
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param add_vars additional variables specified by the setting `add_vars` to be included in 
#'       prediction, yield, and residuals csv and shape files
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode


diagnosticPlotsNLLS<- function(file.output.list,class.input.list,sitedata.demtarea.class,
                               sitedata,sitedata.landuse,estimate.list,mapping.input.list,Csites.weights.list,
                               Cor.ExplanVars.list,
                               data_names,add_vars,batch_mode) {
  
  
  
  #########################
  # Create Global Variables
  #########################
  
  
  
  # create global variable from list names (Mdiagnostics.list)
  unPackList(lists = list(mapping.input.list = mapping.input.list,
                          Mdiagnostics.list = estimate.list$Mdiagnostics.list,
                          file.output.list = file.output.list,
                          class.input.list = class.input.list),
             parentObj = list(NA,
                              NA,
                              NA,
                              NA))
  
  
  filename <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_diagnostic_plots.html")
  reportPath<-paste0(path_master,"diagnosticPlotsNLLS.Rmd")

path_diagnosticMapAttrChild <- file_path_as_absolute(paste0(path_master,"diagnosticMapAttrChild.Rmd"))
path_diagnosticCorrChild <- file_path_as_absolute(paste0(path_master,"diagnosticCorrChild.Rmd"))
path_diagnosticClassvarChild <- file_path_as_absolute(paste0(path_master,"diagnosticClassvarChild.Rmd"))
path_diagnosticClassLandChild <- file_path_as_absolute(paste0(path_master,"diagnosticClassLandChild.Rmd"))
path_diagnosticContiguousChild<- file_path_as_absolute(paste0(path_master,"diagnosticContiguousChild.Rmd"))
path_diagnosticDiagMapChild<-file_path_as_absolute(paste0(path_master,"diagnosticDiagMapChild.Rmd"))

  
rmarkdown::render(paste0(path_master,"diagnosticPlotsNLLS.Rmd"),
    params = list(
      validation = FALSE,
      file.output.list = file.output.list,
      path_diagnosticMapAttrChild = path_diagnosticMapAttrChild,
      path_diagnosticCorrChild = path_diagnosticCorrChild,
      path_diagnosticClassvarChild = path_diagnosticClassvarChild,
      path_diagnosticClassLandChild = path_diagnosticClassLandChild,
      path_diagnosticContiguousChild = path_diagnosticContiguousChild,
      path_diagnosticDiagMapChild = path_diagnosticDiagMapChild,
      class.input.list = class.input.list,
      sitedata.demtarea.class = sitedata.demtarea.class,
      sitedata = sitedata,
      sitedata.landuse = sitedata.landuse,
      estimate.list = estimate.list,
      mapping.input.list = mapping.input.list,
      Csites.weights.list = Csites.weights.list,
      Cor.ExplanVars.list = Cor.ExplanVars.list,
      data_names = data_names,
      add_vars = add_vars,
      batch_mode = batch_mode
    ),
    output_file = filename, quiet = TRUE
  )
  
  #shell.exec(filename)
  
  
  #output siteAttr shapefile
  if (outputESRImaps[4]=="yes"){
    siteAttrshape<-data.frame(waterid = sitedata$waterid,
                              originalWaterid = sitedata$waterid_for_RSPARROW_mapping,
                              xlat,xlon)
    for (s in 1:length(map_siteAttributes.list)){
      if (length(names(sitedata)[which(names(sitedata)==map_siteAttributes.list[s])])!=0){
        siteAttr<-eval(parse(text= paste0("data.frame(",map_siteAttributes.list[s],"=sitedata$",map_siteAttributes.list[s],")")))
        siteAttrshape<-data.frame(siteAttrshape,siteAttr)
        names(siteAttrshape)[length(siteAttrshape)]<-map_siteAttributes.list[s]
      }
    }
    
    
    
    siteAttrshape<-SpatialPointsDataFrame(siteAttrshape[,c("xlon","xlat")],siteAttrshape[,which(!names(siteAttrshape) %in% c("xlat","xlon"))],proj4string=CRS(CRStext))
    
    if (!dir.exists(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep))){
      dir.create(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep),showWarnings = FALSE)
    }
    if (!dir.exists(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep))){
      dir.create(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep),showWarnings = FALSE)
    }
    
    maptools::writeSpatialShape(siteAttrshape,paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep,"siteAttrshape"))
    cat(showWKT(proj4string(siteAttrshape)),file=paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep,"siteAttrshape.prj")) 
  }
  
  #output residuals shapefile
  if (outputESRImaps[3]=="yes"){
    Resids <- estimate.list$sparrowEsts$resid
    Obsyield <- Obs / sitedata$demtarea
    
    predictYield <- predict / sitedata$demtarea
    leverage<-estimate.list$JacobResults$leverage
    boot_resid<-estimate.list$JacobResults$boot_resid
    tiarea<-Csites.weights.list$tiarea
    weight<-Csites.weights.list$weight
    origWaterid<-sitedata$waterid_for_RSPARROW_mapping
    
    dd <- data.frame(sitedata,origWaterid,Obs,predict,Obsyield,predictYield,Resids,standardResids,leverage,boot_resid,weight,tiarea,pResids,ratio.obs.pred,pratio.obs.pred,xlat, xlon)
    keeps <- c("waterid","origWaterid","demtarea","rchname","station_id","station_name","staid",classvar[1],"Obs",
               "predict","Obsyield","predictYield","Resids","standardResids","leverage","boot_resid","weight","tiarea","pResids","ratio.obs.pred","pratio.obs.pred","xlat","xlon")
    residShape <- dd[keeps]
    
    if (length(na.omit(add_vars))!=0){
      add_data<-data.frame(sitedata[,which(names(sitedata) %in% add_vars)])
      if (length(add_vars)==1){
        names(add_data)<-add_vars
      }
      residShape<-cbind(residShape,add_data)
    }
    
    residShape <-SpatialPointsDataFrame(residShape[,c("xlon","xlat")],residShape[,which(!names(residShape) %in% c("xlat","xlon"))],proj4string=CRS(CRStext))
    
    if (!dir.exists(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep))){
      dir.create(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep),showWarnings = FALSE)
    }
    if (!dir.exists(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep))){
      dir.create(paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep),showWarnings = FALSE)
    }
    
    maptools::writeSpatialShape(residShape,paste0(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep,"residShape"))
    cat(showWKT(proj4string(residShape)),file=paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"residuals",.Platform$file.sep,"residShape.prj")) 
    
  }
  
  
}#end function

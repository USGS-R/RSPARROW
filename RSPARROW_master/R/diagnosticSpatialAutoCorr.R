#'@title diagnosticSpatialAutoCorr
#'@description Generates diagnostics for spatial dependence (autocorrelation) in the model 
#'            residuals. SiteIDs are determined from hydseq sorted file to ensure consistency in hydrologic 
#'            distance and other ID ordering across programs. Outputs plots to 
#'            ~/estimate/(run_id)_diagnostic_spatialautocor.pdf. Outputs Morans I stats to ~/estimate/(run_id)_diagnostic_spatialautocor.txt. 
#'            Outputs Morans I stats to ~/estimate/summaryCSV/(run_id)_EuclideanMoransI.csv \\cr \\cr
#'Executed By: controlFileTasksModel.R \\cr
#'Executes Routines: \\itemize\{\\item fixDupLatLons.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param classvar character vector of user specified spatially contiguous discrete 
#'       classification variables from sparrow_control.  First element is reach classification variable.
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0
#'                & subdata$calsites==1), ]`. The object contains the dataDictionary 
#'                ‘sparrowNames’ variables, with records sorted in hydrological 
#'                (upstream to downstream) order (see the documentation Chapter 
#'                sub-section 5.1.2 for details)
#'@param numsites number of sites selected for calibration
#'@param estimate.list list output from `estimate.R`
#'@param estimate.input.list named list of sparrow_control settings: ifHess, s_offset, 
#'                           NLLS_weights,if_auto_scaling, and if_mean_adjust_delivery_vars
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, 
#'                          polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, CRStext, 
#'                          convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, 
#'                          if_verify_demtarea_maps
#'@param subdata data.frame input data (subdata)



diagnosticSpatialAutoCorr <- function(file.output.list,classvar,sitedata,numsites,estimate.list,
                                      estimate.input.list,mapping.input.list,subdata) { 
  
  
  
  #  z = (Moran's I statistic - Moran's Expectation) / sqrt(Variance) = Moran's standard deviate
  
  
  #############################################
  
  
  # transfer required variables to global environment from 'estimate.list$Mdiagnostics.list'
  # create global variable from list names
  # transfer required variables to global environment from 'DataMatrix.list$data.index.list'
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA))
  filename <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_diagnostic_spatialautocor.html")
  
  path_masterFormat <- file_path_as_absolute(paste0(path_master,"diagnosticSpatialAutoCorr.R"))
  path_masterFormat<-gsub("diagnosticSpatialAutoCorr.R","",path_masterFormat)
  
  reportPath<-paste0(path_master,"diagnosticSpatialAutoCorr.Rmd")

  
  rmarkdown::render(paste0(path_master,"diagnosticSpatialAutoCorr.Rmd"),
                    params = list(
                      path_masterFormat = path_masterFormat,
                      file.output.list = file.output.list,
                      mapping.input.list = mapping.input.list,
                      classvar = classvar,
                      sitedata = sitedata,
                      numsites = numsites, 
                      estimate.list = estimate.list,
                      estimate.input.list = estimate.input.list,
                      subdata = subdata
                    ),
                    output_file = filename, quiet = TRUE
  )
  
  #sink text file
  options(width = 200, max.print = 999999)
  # define title output function
  outcharfun<-function(char) {
    outchar <- data.frame(char)
    row.names(outchar ) <- c(" ")
    colnames(outchar ) <- c(" ")
    return(outchar)
  }
  # define "space" for printing
  ch <- character(1)
  space <- data.frame(ch)
  row.names(space) <- ch
  colnames(space) <- c(" ")
  
  #load data from RMD
  load(paste0(path_master,"tempDiagSpat.RData"))
  unlist(paste0(path_master,"tempDiagSpat.RData"))
  unPackList(lists = list(saveList = saveList),
             parentObj = list(NA))
  
  filename <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_diagnostic_spatialautocor.txt")
  sink(file=filename,split="FALSE",append=FALSE)
  
  print(outcharfun("MORAN'S I EUCLIDEAN AND HYDROLOGIC DISTANCE WEIGHTED RESULTS"))
  print(dd)
  
  print(outcharfun("RIVER BASIN RESULTS"))
  print(outcharfun(" Euclidean (E) and hydrologic (H) distance weighting (reported for most downstream site in river basins with >= 5 sites)"))
  print(space)
  print(sites_sigmoran)
  print(space)
  
  print(outcharfun("FULL DOMAIN RESULTS (Hydrologic distance weighting within river basins)"))
  print(moranOut)
  
  print(outcharfun(xtext))
  print(space)
  
  print(outcharfun("REGIONAL AND FULL DOMAIN RESULTS"))
  print(outcharfun(" Euclidean distance weighting (regional results reported for contiguous spatial class variable)"))
  print(space)
  print(class_sigmoran)
  
  sink(type="message")
  sink()
  
  
  
  

}#end function





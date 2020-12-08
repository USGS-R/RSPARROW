#'@title diagnosticSensitivity
#'@description Calculates the parameter sensitivities (change in load predictions for a a 1% 
#'            unit change in the explanatory variables). Outputs plots to 
#'            ~/estimate/(run_id)_diagnostic_sensitivity.pdf. Outputs `sensitivities.list` as binary file to ~/estimate/(run_id)_sensitivities.list. \\cr \\cr
#'Executed By: estimate.R \\cr
#'Executes Routines: \\itemize\{\\item named.list.R
#'             \\item predictSensitivity.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param classvar character vector of user specified spatially contiguous discrete 
#'       classification variables from sparrow_control.  First element is reach classification variable.
#'@param estimate.list list output from `estimate.R`
#'@param DataMatrix.list named list of 'data' and 'beta' matrices and 'data.index.list' 
#'                       for optimization
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'       parmMin>=0) | parmType!="SOURCE")`
#'@param reach_decay_specification the SAS IML reach decay function code from sparrow_control
#'@param reservoir_decay_specification the SAS IML reservoir decay function code from 
#'       sparrow_control
#'@param subdata data.frame input data (subdata)
#'@param sitedata.demtarea.class Total drainage area classification variable for calibration 
#'                               sites.
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, 
#'                          polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, CRStext, 
#'                          convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, 
#'                          if_verify_demtarea_maps



diagnosticSensitivity <- function(file.output.list,classvar,estimate.list,DataMatrix.list,SelParmValues,
                                  reach_decay_specification,reservoir_decay_specification,
                                  subdata,sitedata.demtarea.class,mapping.input.list) {
  
  
  ####################################################################
  # create global variables

  
  unPackList(lists = list(file.output.list = file.output.list),
               parentObj = list(NA))
  


  filename <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_diagnostic_sensitivity.html")
  reportPath<-paste0(path_master,"diagnosticSensitivity.Rmd")
  
  path_diagnosticSensParamChild <- file_path_as_absolute(paste0(path_master,"diagnosticSensParamChild.Rmd"))
  
  rmarkdown::render(paste0(path_master,"diagnosticSensitivity.Rmd"),
    params = list(
      file.output.list = file.output.list,
      path_diagnosticSensParamChild = path_diagnosticSensParamChild,
      classvar = classvar,
      estimate.list = estimate.list,
      DataMatrix.list = DataMatrix.list,
      SelParmValues = SelParmValues,
      reach_decay_specification = reach_decay_specification,
      reservoir_decay_specification = reservoir_decay_specification,
      subdata = subdata,
      sitedata.demtarea.class = sitedata.demtarea.class,
      mapping.input.list = mapping.input.list
    ),
    output_file = filename, quiet = TRUE
  )
  ####################################################################
  
  objfile <- paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_sensitivities.list") 
  load(objfile)
  assign("sensitivities.list",sensitivities.list,envir = .GlobalEnv)
  
  
 

  
}#end function


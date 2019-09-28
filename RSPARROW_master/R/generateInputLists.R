#'@title generateInputLists
#'@description creates lists of control settings for group input into functions \\cr \\cr
#'Executed By: executeRSPARROW.R \\cr
#'Executes Routines: \\itemize\{\\item named.list.R
#'             \\item unPackList.R\} \\cr
#'@param settings named list of all control settings



generateInputLists<-function(settings){
  unPackList(lists = list(settings = settings),
             parentObj = list(NA))   
  
  
  
  estimate.input.list <- named.list(ifHess, s_offset, NLLS_weights,if_mean_adjust_delivery_vars,
                                    yieldFactor,ConcFactor,confInterval,
                                    loadUnits,yieldUnits,ConcUnits,MoranDistanceWeightFunc,
                                    incr_delivery_specification,reach_decay_specification,
                                    reservoir_decay_specification) 
  mapping.input.list <- named.list(lat_limit,lon_limit,master_map_list,
                                   lineShapeName,lineWaterid,polyShapeName,polyWaterid,LineShapeGeo,
                                   LineShapeGeo,CRStext,convertShapeToBinary.list,
                                   map_siteAttributes.list,
                                   if_verify_demtarea_maps,output_map_type,  
                                   outputERSImaps,
                                   
                                   #diagnosticPlots
                                   loadUnits,
                                   yieldUnits,
                                   diagnosticPlotPointSize,
                                   diagnosticPlotPointStyle,
                                   
                                   #prediction map settings
                                   predictionTitleSize,
                                   predictionLegendSize,
                                   predictionLegendBackground,
                                   predictionMapColors,
                                   predictionClassRounding,
                                   predictionMapBackground,
                                   lineWidth,
                                   
                                   #residual maps settings
                                   residual_map_breakpoints,
                                   ratio_map_breakpoints,
                                   residualTitleSize,
                                   residualLegendSize,
                                   residualColors,
                                   residualPointStyle,
                                   residualPointSize_breakpoints,
                                   residualPointSize_factor,
                                   residualMapBackground,
                                   
                                   #siteAttribute maps settings
                                   siteAttrTitleSize,
                                   siteAttrLegendSize,
                                   siteAttrColors,
                                   siteAttrClassRounding,
                                   siteAttr_mapPointStyle,
                                   siteAttr_mapPointSize,
                                   siteAttrMapBackground,
                                   
                                   #scenarios
                                   scenarioMapColors) 
  
  file.output.list <- named.list(path_master,
                                 path_main,
                                 path_user,
                                 path_data,
                                 path_gis,
                                 path_results,
                                 results_directoryName,
                                 data_directoryName,
                                 gis_directoryName,
                                 run_id,
                                 csv_decimalSeparator,
                                 csv_columnSeparator)
  
  class.input.list<-named.list(classvar,class_landuse,class_landuse_percent)
  
  min.sites.list<-named.list(minimum_headwater_site_area,
                             minimum_reaches_separating_sites,
                             minimum_site_incremental_area)
  
  scenario.input.list<-named.list(select_scenarioReachAreas,
                                  select_targetReachWatersheds,
                                  scenario_name,
                                  scenario_map_list,
                                  scenarioMapColors,
                                  scenario_sources,
                                  scenario_factors,
                                  landuseConversion)
  
  assign("estimate.input.list",estimate.input.list,envir = .GlobalEnv)
  assign("mapping.input.list",mapping.input.list,envir = .GlobalEnv)
  assign("file.output.list",file.output.list,envir = .GlobalEnv)
  assign("class.input.list",class.input.list,envir = .GlobalEnv)
  assign("min.sites.list",min.sites.list,envir = .GlobalEnv)
  assign("scenario.input.list",scenario.input.list,envir = .GlobalEnv)
  
}

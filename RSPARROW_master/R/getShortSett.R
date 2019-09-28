#'@title getShortSett
#'@description list of all control settings that should have a length of 1 \\cr \\cr
#'Executed By: \\itemize\{\\item executeRSPARROW.R
#'             \\item outputSettings.R
#'             \\item runRsparrow.R
#'             \\item testSettings.R\} \\cr
#'@return `shortSettings` list of all control settings that should have a length of 1



getShortSett<-function(){
  shortSettings<-c("input_data_fileName",
                   "minimum_headwater_site_area",
                   "minimum_reaches_separating_sites",
                   "minimum_site_incremental_area",
                   "reach_decay_specification",
                   "reservoir_decay_specification",
                   "s_offset",
                   "NLLS_weights",
                   "pvalidate",
                   "biters",
                   "iseed",
                   "ConcFactor",
                   "run_id",
                   "output_map_type",
                   "scenario_name",
                   "modelComparison_name",
                   "results_directoryName",
                   "data_directoryName",
                   "gis_directoryName",
                   "csv_decimalSeparator",
                   "csv_columnSeparator",
                   "copy_PriorModelFiles",
                   
                   "predictionTitleSize",
                   "predictionLegendSize",
                   "predictionLegendBackground",
                   "predictionClassRounding",
                   "predictionMapBackground",
                   "lineWidth",
                   "residualTitleSize",
                   "residualLegendSize",
                   "residualPointSize_factor",
                   "residualMapBackground",
                   "siteAttrTitleSize",
                   "siteAttrLegendSize",
                   "siteAttrClassRounding",
                   "siteAttr_mapPointStyle",
                   "siteAttr_mapPointSize",
                   "siteAttrMapBackground",
                   "loadUnits",
                   "yieldUnits",
                   "diagnosticPlotPointSize",
                   "diagnosticPlotPointStyle",
                   
                   "confInterval",
                   "MoranDistanceWeightFunc")
  return(shortSettings)
}

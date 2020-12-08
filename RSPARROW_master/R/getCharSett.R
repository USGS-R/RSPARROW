#'@title getCharSett
#'@description list of all character type control settings \\cr \\cr
#'Executed By: \\itemize\{\\item executeRSPARROW.R
#'             \\item outputSettings.R
#'             \\item runRsparrow.R
#'             \\item testSettings.R\} \\cr
#'@return `charSettings` list of all character type control settings



getCharSett<-function(){
  charSettings<-c("path_master",
                  "path_main",
                  "path_user",
                  "path_data",
                  "path_gis",
                  "path_results",
                  "filter_data1_conditions",
                  "reach_decay_specification",
                  "reservoir_decay_specification",
                  "classvar",
                  "class_landuse",
                  "master_map_list",
                  "lineShapeName",
                  "lineWaterid",
                  "polyShapeName",
                  "polyWaterid",
                  "LineShapeGeo",
                  "CRStext",
                  "scenario_sources",
                  "scenario_map_list",
                  "run_id",
                  "add_vars",
                  "output_map_type",
                  "scenario_name",
                  "input_data_fileName",
                  "modelComparison_name",
                  "compare_models",
                  "results_directoryName",
                  "data_directoryName",
                  "gis_directoryName",
                  "csv_decimalSeparator",
                  "csv_columnSeparator",
                  "copy_PriorModelFiles",
                  
                  "outputESRImaps",
                  "predictionLegendBackground",
                  "predictionMapColors",
                  "predictionMapBackground",
                  "residualColors",
                  "residualMapBackground",
                  "siteAttrColors",
                  "siteAttrMapBackground",
                  "scenarioMapColors",
                  "loadUnits",
                  "yieldUnits",
                  "landuseConversion",
                  
                  "incr_delivery_specification",
                  
                  "MoranDistanceWeightFunc")
  return(charSettings)
}

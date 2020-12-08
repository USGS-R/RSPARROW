#'@title getOptionSett
#'@description list of all control settings with specified optional values \\cr \\cr
#'Executed By: \\itemize\{\\item executeRSPARROW.R
#'             \\item outputSettings.R
#'             \\item runRsparrow.R
#'             \\item testSettings.R\} \\cr
#'@return `optionSettings` list of all control settings with specified optional values



getOptionSett<-function(){
  optionSettings<-c("calculate_reach_attribute_list = c('hydseq','headflag','termflag','demtarea')",
                    "NLLS_weights = c('default','lnload','user')",
                    "select_scenarioReachAreas = c('all reaches','none','selected reaches')",
                    "convertShapeToBinary.list = c('lineShapeName','polyShapeName','LineShapeGeo')",
                    "output_map_type = c('stream','catchment','both')",
                    "outputESRImaps = c('yes','no')")
  return(optionSettings)
}

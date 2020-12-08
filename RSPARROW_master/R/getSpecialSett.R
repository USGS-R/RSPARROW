#'@title getSpecialSett
#'@description list of settings that require additional testing and the test that should be run \\cr \\cr
#'Executed By: \\itemize\{\\item testCosmetic.R
#'             \\item testSettings.R\} \\cr
#'Executes Routines: areColors.R \\cr
#'@return `specialSettings` list of settings that require additional testing and the test that 
#'            should be run



getSpecialSett<-function(){
  specialSettings<-list()
  specialSettings$name<-c("outputESRImaps",
                          
                          "predictionMapColors",
                          "predictionMapColors",
                          "predictionLegendBackground",
                          "predictionMapBackground",
                          "predictionClassRounding",
                          
                          "residual_map_breakpoints",
                          "ratio_map_breakpoints",
                          "residualColors",
                          "residualColors",
                          "residualPointStyle",
                          "residualPointSize_breakpoints",
                          "residualMapBackground",
                          "residualPointStyle",
                          
                          "siteAttrColors",
                          "siteAttrColors",
                          "siteAttrMapBackground",
                          "siteAttrClassRounding",
                          "siteAttr_mapPointStyle",
                          
                          "scenarioMapColors",
                          "scenarioMapColors"
  )
  
  specialSettings$test<-c("length(outputESRImaps)==4",
                          
                          "length(predictionMapColors)>1",
                          "areColors(predictionMapColors)",
                          "areColors(predictionLegendBackground)",
                          "areColors(predictionMapBackground)",
                          "suppressWarnings(round(as.numeric(as.character(predictionClassRounding))))==predictionClassRounding",
                          
                          "length(residual_map_breakpoints)==7",
                          "length(ratio_map_breakpoints)==7",
                          "length(residualColors)==8",
                          "areColors(residualColors)",
                          "length(residualPointStyle)==8",
                          "length(residualPointSize_breakpoints)==8",
                          "areColors(residualMapBackground)",
                          "length(unique(round(residualPointStyle)==residualPointStyle))==1 & unique(round(residualPointStyle)==residualPointStyle)",
                          
                          "length(siteAttrColors)>1",
                          "areColors(siteAttrColors)",
                          "areColors(siteAttrMapBackground)",
                          
                          "suppressWarnings(round(as.numeric(as.character(siteAttrClassRounding))))==siteAttrClassRounding",
                          "suppressWarnings(round(as.numeric(as.character(siteAttr_mapPointStyle))))==siteAttr_mapPointStyle",
                          
                          # "round(siteAttrClassRounding)==siteAttrClassRounding",
                          #  "round(siteAttr_mapPointStyle)==siteAttr_mapPointStyle",
                          
                          "length(scenarioMapColors)>1",
                          "areColors(scenarioMapColors)"
  )
  
  specialSettings$fail<-c("length(outputESRImaps)==4",
                          
                          "length(predictionMapColors)>1",
                          "predictionMapColors is vector of valid color strings",
                          "predictionLegendBackground is a valid color string",
                          "predictionMapBackground is a valid color string",
                          "predictionClassRounding is integer",
                          
                          "length(residual_map_breakpoints)==7",
                          "length(ratio_map_breakpoints)==7",
                          "length(residualColors)==8",
                          "residualColors is vector of valid color strings",
                          "length(residualPointStyle)==8",
                          "length(residualPointSize_breakpoints)==8",
                          "residualMapBackground is a valid color string",
                          "residualPointStyle is vector of integers",
                          
                          "length(siteAttrColors)>1",
                          "siteAttrColors is vector of valid color strings",
                          "siteAttrMapBackground is a valid color string",
                          "siteAttrClassRounding is integer",
                          "siteAttr_mapPointStyle is integer",
                          
                          "length(scenarioMapColors)>1",
                          "scenarioMapColors is vector of valid color strings"
  )
  
  return(specialSettings)
}

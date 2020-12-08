#'@title setMapDefaults
#'@description Sets default values for any missing required mapping settings and assigns 
#'            default value to global environment. \\cr \\cr
#'Executed By: executeRSPARROW.R \\cr
#'Executes Routines: unPackList.R \\cr
#'@param settings named list of all control settings



setMapDefaults<-function(settings){
  
  unPackList(lists = list(settings = settings),
             parentObj = list(NA))  
  
  #must be given in alphabetical order like ls()
  defaults<-list(diagnosticPlotPointSize = 0.6,
                 diagnosticPlotPointStyle = 16,
                 lineWidth = 0.5,
                 outputESRImaps = c("no","no","no","no"),
                 predictionClassRounding = 1,
                 predictionLegendBackground = "grey",
                 predictionLegendSize = 0.5,
                 predictionMapBackground = "white",
                 predictionMapColors = c("blue","dark green","gold","red","dark red"),
                 predictionTitleSize = 16,
                 ratio_map_breakpoints = c(0.3,0.5,0.8,1,1.25,2,3.3),
                 residual_map_breakpoints = c(-2.5,-0.75,-0.25,0,0.25,0.75,2.5),
                 residualColors = c("red","red","gold","gold","dark green","dark green","blue","blue"),
                 residualLegendSize = 1,
                 residualMapBackground = "grey",
                 residualPointSize_breakpoints = c(0.75,0.5,0.4,0.25,0.25,0.4,0.5,0.75),
                 residualPointSize_factor = 1,
                 residualPointStyle = c(2,2,1,1,1,1,6,6),
                 residualTitleSize = 16,
                 scenarioMapColors = c("light blue","blue","dark green","gold","red","dark red"),
                 siteAttr_mapPointSize = 2,
                 siteAttr_mapPointStyle = 16,
                 siteAttrClassRounding = 2,
                 siteAttrColors = c("blue","green4","yellow","orange","red","darkred"),
                 siteAttrLegendSize = 0.5,
                 siteAttrMapBackground = "grey",
                 siteAttrTitleSize = 1
                 
  )
  

  listSettings<-names(settings)[which(names(settings) %in% names(defaults))]
  
  for (s in listSettings){
    setting<-get(s)
    if (length(setting)==1){
      if (is.na(setting)){
        assign(s,defaults[s][[1]],envir = .GlobalEnv)
        
        value<-defaults[s][[1]]
        if (length(value)>1){
          value<-paste0("c(",paste(value, collapse = ","),")")
        }
        warn<-paste0("MISSING VALUE FOR REQUIRED SETTING : ", s, "\n DEFAULT VALUE :  ",value, " USED \n \n")
        message(warn)
      }
    }
  }
}

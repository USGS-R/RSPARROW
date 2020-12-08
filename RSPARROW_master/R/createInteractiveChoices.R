#'@title createInteractiveChoices
#'@description Generates list of prediction metrics for user to select to plot in RShiny based 
#'            on modeled parameters \\cr \\cr
#'Executed By: shinyMap2.R \\cr
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'       parmMin>=0) | parmType!="SOURCE")`
#'@param existPredict logical TRUE/FALSE whether predict.list is available in shiny environment
#'@param subdata data.frame input data (subdata)
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param map_uncertainties Vector of user selected uncertainty parameters to map, if 
#'       uncertainty analysis was not run NA
#'@return `Choices`  data.frame of parameter metrics by type ("Load Predictions", "Yield 



createInteractiveChoices<-function(SelParmValues,existPredict,subdata, data_names, map_uncertainties){
  if (existPredict){
    sources<-as.character(SelParmValues$srcvar)
    
    
    #load metrics
    choices<-c("pload_total",
               paste0("pload_",sources),
               "mpload_total",
               paste0("mpload_",sources),
               "pload_nd_total",
               paste0("pload_nd_",sources),
               "pload_inc",
               paste0("pload_inc_",sources),
               "deliv_frac",
               "pload_inc_deliv",
               paste0("pload_inc_",sources,"_deliv"),
               paste0("share_total_",sources),
               paste0("share_inc_",sources))
    
    definitions<-c("Total load (fully decayed)",
                   rep("Source load (fully decayed)",length(sources)),
                   "Monitoring-adjusted total load (fully decayed)",
                   rep("Monitoring-adjusted source load (fully decayed)",length(sources)),
                   "Total load delivered to streams (no stream decay)",
                   rep("Source load delivered to streams (no stream decay)",length(sources)),
                   "Total incremental load delivered to streams",
                   rep("Source incremental load delivered to streams",length(sources)),
                   "Fraction of total load delivered to terminal reach",
                   "Total incremental load delivered to terminal reach",
                   rep("Source incremental load delivered to terminal reach",length(sources)),
                   rep("Source shares for total load (percent)",length(sources)),
                   rep("Source share for incremental load (percent)",length(sources)))
    
    choices<-data.frame(category = rep("Load Predictions",length(choices)),
                        variable = choices,
                        definition = definitions)
    Choices<-choices
    
    #yield metrics
    choices<-c("concentration",
               "yield_total",
               paste0("yield_",sources),
               "myield_total",
               paste0("myield_",sources),
               "yield_inc",
               paste0("yield_inc_",sources),
               "yield_inc_deliv",
               paste0("yield_inc_",sources,"_deliv"))
    definitions<-c("Concentration based on decayed total load and discharge",
                   "Total yield (fully decayed)",
                   rep("Source yield (fully decayed)",length(sources)),
                   "Monitoring-adjusted total yield (fully decayed)",
                   rep("Monitoring-adjusted source yield (fully decayed)",length(sources)),
                   "Total incremental yield delivered to streams",
                   rep("Source incremental yield delivered to streams",length(sources)),
                   "Total incremental yield delivered to terminal reach",
                   rep("Source incremental yield delivered to terminal reach",length(sources)))
    
    
    choices<-data.frame(category = rep("Yield Predictions",length(choices)),
                        variable = choices,
                        definition = definitions)
    Choices<-rbind(Choices,choices)
    
    if (!is.na(map_uncertainties)){
      #Uncertainty metrics
      choices<-c("se_pload_total",
                 "ci_pload_total",
                 "se_mpload_total",
                 "ci_mpload_total",
                 "model.error.var",
                 "sample.error.var.boots")
      definitions<-c("Standard error of the total load (percent of mean)",
                     "95% prediction interval of the total load (percent of mean)",
                     "",
                     "",
                     "",
                     "")
      
      choices<-data.frame(category = rep("Prediction Uncertainties",length(choices)),
                          variable = choices,
                          definition = definitions)
      Choices<-rbind(Choices,choices)
    }
    #data dictionary variables
    for (n in names(subdata)[which(regexpr("waterid",names(subdata))<0)]){
      if (class(subdata[,n])=="numeric"){
        def<-data_names[which(data_names$sparrowNames==n),]$explanation
        if (length(def)==0){
          def<-""
        }
        choices<-data.frame(category = c("Data Dictionary Variable"),
                            variable = n,
                            definition = def)
        Choices<-rbind(Choices,choices)
      }
    }
    
  }else{#only data dictionary variables
    Choices<-data.frame(matrix(0, nrow= 0, ncol = 3))
    names(Choices)<-c("category","variable","definition")
    for (n in names(subdata)[which(regexpr("waterid",names(subdata))<0)]){
      if (class(subdata[,n])=="numeric"){
        def<-data_names[which(data_names$sparrowNames==n),]$explanation
        if (length(def)==0){
          def<-""
        }
        choices<-data.frame(category = c("Data Dictionary Variable"),
                            variable = n,
                            definition = def)
        Choices<-rbind(Choices,choices)
      }
    }
    
  }
  return(Choices) 
  
}

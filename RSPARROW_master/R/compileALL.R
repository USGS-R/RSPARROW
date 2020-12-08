#'@title compileALL
#'@description Compiles all user input from the shiny session in the namespace of the map 
#'            being generated into a single list object. \\cr \\cr
#'Executed By: \\itemize\{\\item goShinyPlot.R
#'             \\item shinyMap2.R\} \\cr
#'Executes Routines: compileInput.R \\cr
#'@param input top level interactive user input in Shiny app
#'@param output shiny list object for session output
#'@param session active shiny session
#'@param path_results path to results model subdirectory
#'@param choices data.frame output of function createInteractiveChoices.R
#'@return `out`  list object of all user input within the namespace of the type of map being 



compileALL<-function(input, output, session, path_results, choices){
  
  top<-list(batch = input$batch,
            mapType = input$mapType,
            enablePlotly = input$enablePlotly)
  
  nsList<-character(0)
  if (input$batch=="Batch"){
    nsList<-c(nsList,"nsBatch")
  }
  if (input$mapType  %in% c("Stream","Catchment")){
    nsList<-c(nsList,"nsStreamCatch")
    if (input$batch=="Batch"){
      for (c in as.character(unique(choices$category))){
        if (c!="Prediction Uncertainties"){
          nsName<-paste0("ns",tolower(str_split(c," ")[[1]][1]),"Drop")
          
        }else{
          nsName<-"nsuncertaintyDrop"
        }
        nsList<-c(nsList,nsName)
      }
    }
  }else if (input$mapType=="Site Attributes"){
    nsList<-c(nsList, "nsSiteAttr","nsattrDrop")
  }else if (input$mapType=="Source Change Scenarios"){
    if (input$batch=="Batch"){
      choicesScen<-choices[which(!choices$category %in% c("Data Dictionary Variable","Prediction Uncertainties") & regexpr("Monitoring-adjusted",choices$definition)<0),]
      ratioChoices<-data.frame(category = c("Relative Change in Load","Relative Change in Load"),
                               variable = c("ratio_total","ratio_inc","percent_total","percent_inc"),
                               definition = c("Ratio of the changed total load to the baseline (unchanged) total load",
                                              "Ratio of the changed incremental load to the baseline (unchanged) incremental load"))
      choices$category<-ifelse(choices$category=="Load Predictions","Load Predictions for Changed Sources",
                               ifelse(choices$category=="Yield Predictions","Yield Predictions for Changed Sources",choices$category))
      choicesScen<-rbind(choicesScen,ratioChoices)
      for (c in as.character(unique(choicesScen$category))){
        nsName<-paste0("nsScen",tolower(str_split(c," ")[[1]][1]),"Drop")
        nsList<-c(nsList,nsName)
      }
    }
    nsList<-c(nsList, "nsScenarios")
  }
  
  compiledInput<-top
  for (n in nsList){
    compiledInput<-append(compiledInput,callModule(compileInput,n))
    
  }
  
  
  
  
  out<-list(compiledInput = compiledInput)
  return(out)
}

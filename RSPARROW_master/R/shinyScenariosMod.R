#'@title shinyScenariosMod
#'@description shiny module for source change scenario mapping \\cr \\cr
#'Executed By: shinyMap2.R \\cr
#'Executes Routines: \\itemize\{\\item handsOnMod.R
#'             \\item testCosmetic.R
#'             \\item validCosmetic.R\} \\cr
#'@param input top level interactive user input in Shiny app
#'@param output shiny list object for session output
#'@param session active shiny session
#'@param scenarioRtables named list of rhandsontables used as initial values for shiny app
#'@param path_results path to results model subdirectory



shinyScenariosMod<-function(input, output, session, scenarioRtables,
                            path_results,scenario.input.list, mapping.input.list){
  
  #button for flaging targets
  observeEvent(input$openTarget, {
    filein <- paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,"flag_TargetReachWatersheds.csv")
    shell(filein, wait = TRUE, invisible=FALSE)
    
  })
  
  
  observe({
    
    if (input$domain=="selected reaches"){
      if (input$allSrc=="yes"){
        
        callModule(handsOnMod, "nsSourceRed", DF = as.data.frame(scenarioRtables$sourceRed))
        
        callModule(handsOnMod, "nsAllSources", DF = as.data.frame(scenarioRtables$allSourcesDF))
      }else{
        
        
        callModule(handsOnMod, "nsAllSourcesNO", DF = as.data.frame(scenarioRtables$allSourcesDFno))
        
      }
    }else{#all reaches
      
      callModule(handsOnMod, "nsSourceRedALL", DF = as.data.frame(scenarioRtables$sourceRed))
    }
    
    testRow<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticScen), "Source Change Scenarios",
                          scenario.input.list, mapping.input.list)$rowNums
    callModule(validCosmetic,"nsCosmetic", DF = as.data.frame(scenarioRtables$cosmeticScen),rowNum = testRow)
    
    

    
    
  }) 
  
  
  
  
  
  
  
  
}

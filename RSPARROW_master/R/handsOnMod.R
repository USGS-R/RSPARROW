#'@title handsOnMod
#'@description shiny module to create rshandsontables \\cr \\cr
#'Executed By: \\itemize\{\\item shinyMap2.R
#'             \\item shinyScenariosMod.R\} \\cr
#'Executes Routines: allowRemoveRow.R \\cr
#'@param input top level interactive user input in Shiny app
#'@param output shiny list object for session output
#'@param session active shiny session
#'@param DF rhandsontable for cosmetic mapping settings in the shiny app



handsOnMod<-function(input, output, session, DF){
  
  #set reactive values
  values <- reactiveValues()
  
  ## Handsontable
  #create initial or get current
  observe({
    if (!is.null(input$hot)) {
      values[["previous"]] <- isolate(values[["DF"]])
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]])){
        DF <- DF
      }else{
        DF <- values[["DF"]]
      }
    }
    values[["DF"]] <- DF
    
  }) 
  #render hottable as rhandsontable
  output$hot <- renderRHandsontable({
    DF <- values[["DF"]]
    if (!is.null(DF)){
      
      #dont allow the first row to be removed
      allowRemove<-function(DF){
        allow<-ifelse(nrow(DF)==1,FALSE,TRUE)
        return(allow)
      }
      
      #special format for landuseConversion (default values)
      if (length(names(DF)[(names(DF)=="LanduseConversion")]!=0)){
        rhandsontable(DF, rowHeaders = NULL, height = 200, 
                      manualColumnResize=TRUE) %>%
          hot_col("LanduseConversion",default = "None") %>%
          hot_col("ChangeCoefficient",default = "no") %>%
          allowRemoveRow(allowRemove= allowRemove(DF)) 
        
      }else{
        rhandsontable(DF, rowHeaders = NULL, height = 200, 
                      manualColumnResize=TRUE) %>%
          allowRemoveRow(allowRemove= allowRemove(DF)) 
      }
    }
  })
  
  #return(DF)
}

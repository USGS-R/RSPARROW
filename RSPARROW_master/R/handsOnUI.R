#'@title handsOnUI
#'@description shiny ui to setup place for rhandsontable \\cr \\cr
#'Executed By: \\itemize\{\\item shinyScenarios.R
#'             \\item shinySiteAttr.R
#'             \\item streamCatch.R\} \\cr
#'@param id Shiny namespace designation
#'@param input top level interactive user input in Shiny app



handsOnUI<-function(id, input){
  #set namespace
  ns<-NS(id)
  
  fluidPage(
    
    fluidRow( 
      #make rhandsontable
      rHandsontableOutput(ns("hot"))
      
    ))
}

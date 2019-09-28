#'@title shapeFunc
#'@description Shiny ui function applies Shiny conditional panel to show option to output to 
#'            ERSI shapefile if Batch mode is selected \\cr \\cr
#'Executed By: shinyMap2.R \\cr
#'@param id Shiny namespace designation
#'@param input top level interactive user input in Shiny app



shapeFunc<-function(id, input){
  ns <- NS(id)
  conditionalPanel(
    condition = "input.batch == 'Batch'",
    
    #horizontal line
    h5(HTML('<hr style="color: #000000;background-color: #000000; height: 2px"/>')),
    
    selectInput(ns("shapeFile"),"Output to ESRI Shape File",c("yes","no"))
  )
}

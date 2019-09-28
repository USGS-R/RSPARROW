#'@title shinySiteAttr
#'@description Shiny ui function generates user selections for Site Attribute Mapping Uses 
#'            subroutines: dropFunc. \\cr \\cr
#'Executed By: shinyMap2.R \\cr
#'Executes Routines: \\itemize\{\\item dropFunc.R
#'             \\item handsOnUI.R\} \\cr
#'@param id Shiny namespace designation
#'@param input top level interactive user input in Shiny app
#'@param choices data.frame output of function createInteractiveChoices.R



shinySiteAttr<-function(id, input, choices){
  #set namespace
  ns <- NS(id)
  
  #start UI
  conditionalPanel(
    condition = "input.mapType == 'Site Attributes'",
    
    conditionalPanel(
      condition = "input.batch=='Interactive'",
      selectInput(ns("var"), "Site Attribute", 
                  c("",as.character(choices[which(choices$category=="Data Dictionary Variable"),]$variable))),
      textOutput(ns("definition"))),
    
    conditionalPanel(
      condition = "input.batch == 'Batch'",
      h5(HTML("<font color = 'black'><strong>Select Mapping Variables</strong></font>")),  
      dropFunc("nsattrDrop","Data Dictionary Variable",choices)),
    
    #horizontal line
    h5(HTML('<hr style="color: #000000;background-color: #000000; height: 2px"/>')),
    #cosmetic mapping controls
    h4("Mapping Settings"),
    handsOnUI(ns("nsCosmetic"),input)
    
  )
}


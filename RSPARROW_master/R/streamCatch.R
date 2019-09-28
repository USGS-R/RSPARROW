#'@title streamCatch
#'@description Shiny ui function generates user selections for Stream and Catchment Mapping 
#'            Uses subroutines: dropFunc. \\cr \\cr
#'Executed By: shinyMap2.R \\cr
#'Executes Routines: \\itemize\{\\item dropFunc.R
#'             \\item handsOnUI.R\} \\cr
#'@param id Shiny namespace designation
#'@param input top level interactive user input in Shiny app
#'@param choices data.frame output of function createInteractiveChoices.R
#'@param map_uncertainties Vector of user selected uncertainty parameters to map, if 
#'       uncertainty analysis was not run NA



streamCatch<-function(id, input, choices, map_uncertainties){
  #set namespace
  ns <- NS(id)
  
  #begin UI
  conditionalPanel(
    condition = "input.mapType == 'Stream' || input.mapType == 'Catchment'",
    
    conditionalPanel(
      condition = "input.batch == 'Batch'",
      h5(HTML("<font color = 'black'><strong>Select Mapping Variables</strong></font>")),  
      
      #batch mapping variables
      lapply(1:length(as.character(unique(choices$category))), function(c) {
        category<-as.character(unique(choices$category))[c]
        if (category!="Prediction Uncertainties"){
          nsName<-paste0("ns",tolower(str_split(category," ")[[1]][1]),"Drop")
        }else{
          nsName<-"nsuncertaintyDrop"
        }
        dropFunc(nsName,category,choices)
      })
    ),
    
    
    #interactive mapping variable
    conditionalPanel(
      condition = "input.batch != 'Batch'",
      
      
      selectInput(ns("mapCategory"), "Mapping Variable Type", c("",as.character(unique(choices$category)))),
      selectInput(ns("var"), "Mapping Variable", c("",as.character(choices$variable))),
      textOutput(ns("definition"))
    ),
    
    #horizontal line
    h5(HTML('<hr style="color: #000000;background-color: #000000; height: 2px"/>')),
    
    #cosmetic mapping controls
    h4("Mapping Settings"),
    handsOnUI(ns("nsCosmetic"),input)
    
    
  )
  
}

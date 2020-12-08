#'@title shinySiteAttr
#'@description Shiny ui function generates user selections for Site Attribute Mapping Uses 
#'            subroutines: dropFunc. \\cr \\cr
#'Executed By: shinyMap2.R \\cr
#'Executes Routines: \\itemize\{\\item dropFunc.R
#'             \\item handsOnUI.R\} \\cr
#'@param id Shiny namespace designation
#'@param input top level interactive user input in Shiny app
#'@param choices data.frame output of function createInteractiveChoices.R
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0
#'                & subdata$calsites==1), ]`. The object contains the dataDictionary 
#'                ‘sparrowNames’ variables, with records sorted in hydrological 
#'                (upstream to downstream) order (see the documentation Chapter 
#'                sub-section 5.1.2 for details)
#'@param add_plotlyVars character vector indicating user selected variables to add to plot 
#'                      hover text


shinySiteAttr<-function(id, input, choices,sitedata,add_plotlyVars){
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
    
    conditionalPanel(
      condition = "input.enablePlotly != 'static'",
      dropdownButton(circle = FALSE,
                     label = "Add Hover Variable(s)",
                     inputId = ns("dropdown"),
     # dropFunc("nsPlotlyDrop","",choices))
      checkboxGroupInput(ns("plotlyDrop"), "Add Hover Variable(s)", 
                         names(sitedata),
                         selected = names(sitedata)[which(names(sitedata) %in% add_plotlyVars)],
                         inline=FALSE))
      ),
    
    #horizontal line
    h5(HTML('<hr style="color: #000000;background-color: #000000; height: 2px"/>')),
    #cosmetic mapping controls
    h4("Mapping Settings"),
    handsOnUI(ns("nsCosmetic"),input)
    
  )
}


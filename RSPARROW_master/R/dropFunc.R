#'@title dropFunc
#'@description Shiny ui function creates drop down button with SelectAll and ClearAll Buttons 
#'            and a multi-select list of variables in the designated category. Only applied in Batch mode. \\cr \\cr
#'Executed By: \\itemize\{\\item shinyScenarios.R
#'             \\item shinySiteAttr.R
#'             \\item streamCatch.R\} \\cr
#'@param id Shiny namespace designation
#'@param category variable type from `unique(choices$category)` in the Rshiny app
#'@param choices data.frame output of function createInteractiveChoices.R



dropFunc<-function(id,category, choices){
  #set namespace
  ns <- NS(id)
  
  #create dropdown button
  lab<-category
  if (category!="Prediction Uncertainties"){
    name<-paste0(tolower(str_split(category," ")[[1]][1]),"Check")
  }else{
    name<-"uncertaintyCheck"
  }
  dropdownButton(circle = FALSE,
                 label = lab,
                 inputId = ns("dropdown"),
                 #make select all and clear all buttons
                 fluidRow(column(width=6,(actionButton(ns("selectAll"),"Select All",style="color: #000000; background-color: 	#DCDCDC; border-color: #A9A9A9"))),
                          column(width=6,(actionButton(ns("clearAll"),"Clear All",style="color: #000000; background-color: 	#DCDCDC; border-color: #A9A9A9")))),
                 checkboxGroupInput(ns(name), "", 
                                    as.character(choices[which(choices$category==category),]$variable),
                                    inline=FALSE)
  )
}

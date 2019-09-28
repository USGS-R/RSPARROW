#'@title selectAll
#'@description Shiny server function updates variable drop down list selecting all variable 
#'            choices in selected namespace given by 'category' \\cr \\cr
#'Executed By: shinyMap2.R \\cr
#'@param input top level interactive user input in Shiny app
#'@param output shiny list object for session output
#'@param session active shiny session
#'@param category variable type from `unique(choices$category)` in the Rshiny app
#'@param choices data.frame output of function createInteractiveChoices.R



selectAll<-function(input, output, session, category, choices){
  
  #short name for uncertainty checkbox
  if (category!="Prediction Uncertainties"){
    name<-paste0(tolower(str_split(category," ")[[1]][1]),"Check")
  }else{
    name<-"uncertaintyCheck"
  }
  #select all
  observeEvent(input$selectAll,{
    updateCheckboxGroupInput(session,name,selected = as.character(choices[which(choices$category==category),]$variable))
  })
  #clear all
  observeEvent(input$clearAll,{
    updateCheckboxGroupInput(session,name,choices = as.character(choices[which(choices$category==category),]$variable))
  })
  
}


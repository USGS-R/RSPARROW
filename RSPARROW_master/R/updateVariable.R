#'@title updateVariable
#'@description Shiny server function updates variable drop down list according to the user's 
#'            selection of mapping variable type and outputs the definition of the variable in Shiny namespace 
#'            = 'nsStreamCatch' \\cr \\cr
#'Executed By: shinyMap2.R \\cr
#'@param input top level interactive user input in Shiny app
#'@param output shiny list object for session output
#'@param session active shiny session
#'@param choices data.frame output of function createInteractiveChoices.R
#'@param mapType character string indicating type of map (prediction, siteAttr, scenario)



updateVariable<-function(input,output, session,choices, mapType){
  if (mapType=="Site Attributes"){
    category<-"Data Dictionary Variable"
  }else if (mapType=="Source Change Scenarios"){
    
    if (input$mapCategory!=""){
      category<-ifelse(input$mapCategory=="Load Predictions for Changed Sources","Load Predictions",
                       ifelse(input$mapCategory=="Yield Predictions for Changed Sources","Yield Predictions","Relative Change in Load"))
      
    }else{
      category<-input$mapCategory
    }
  }else{
    category<-input$mapCategory
    
  }
  
  
  updateSelectInput(session, "var", 
                    choices = as.character(choices[which(choices$category==category),]$variable))
  output$definition <- renderText({
    as.character(choices[which(choices$variable==input$var),]$definition)
  })
}

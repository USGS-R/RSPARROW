#'@title validSetting
#'@description  \\cr \\cr
#'Executed By:  \\cr
#'@param input top level interactive user input in Shiny app
#'@param output shiny list object for session output
#'@param session active shiny session
#'@param inputIdstr character string naming user inputs in the shiny app
#'@param labelstr character string giving the labels associated user inputs in the shiny app



validSetting<-function(input, output, session, inputIdstr, labelstr){     
  valid<-FALSE
  #update red labels
  observe({
    valid<-eval(parse(text = paste0("input$",inputIdstr)))
    valid<-ifelse(valid!="",TRUE,FALSE)
    
    if (length(valid==TRUE)!=0){  
      if (valid==TRUE){
        updateSelectInput(session, inputIdstr,
                          label = labelstr
        )
      }
    }
  })
  
}

#'@title compileInput
#'@description Shiny server function that updates compiles input from namespace indicated in 
#'            callModule function call \\cr \\cr
#'Executed By: \\itemize\{\\item compileALL.R
#'             \\item testCosmetic.R\} \\cr
#'@param input top level interactive user input in Shiny app
#'@param output shiny list object for session output
#'@param session active shiny session



compileInput<-function(input, output, session){
  
  invalue<-list()
  for (n in names(input)){
    if (!n %in% c("selectAll","clearAll","dropdown")){
      eval(parse(text = paste0("invalue$`",n,"`<-input$`",n,"`")))
    }}
  
  
  return(invalue)
} 

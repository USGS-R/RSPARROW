#'@title validCosmetic
#'@description highlights invalid user inputs in the map settings rhandsontables in the shiny 
#'            app \\cr \\cr
#'Executed By: \\itemize\{\\item shinyMap2.R
#'             \\item shinyScenariosMod.R\} \\cr
#'@param input top level interactive user input in Shiny app
#'@param output shiny list object for session output
#'@param session active shiny session
#'@param DF rhandsontable for cosmetic mapping settings in the shiny app
#'@param rowNum rownumbers of invalid settings in the rhandsontables in the shiny app



validCosmetic<-function(input, output, session, DF, rowNum){
  
  
  values <- reactiveValues()
  
  ## Handsontable
  observe({
    if (!is.null(input$hot)) {
      values[["previous"]] <- isolate(values[["DF"]])
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        DF <- DF
      else
        DF <- values[["DF"]]
    }
    values[["DF"]] <- DF
  })
  
  
  output$hot <- renderRHandsontable({
    DF <- values[["DF"]]
    if (!is.null(DF)){
      
      text_renderer <- "
  function (instance, td, row, col, prop, value, cellProperties) {
Handsontable.renderers.TextRenderer.apply(this, arguments);

td.style.color = 'black';


  }"
      text_renderer2<-paste0(  "function (instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);
                             
                             if (value == '') {
                             td.style.background = 'red';
                             }else if (",paste("row==",rowNum-1,collapse=" || "),"){
                             td.style.background = 'yellow';
                             }else{
                             td.style.background = 'white';
                             }
    }"
      )
      
      rhandsontable(DF, rowHeaders = NULL, height = 200, manualColumnResize=TRUE) %>%
        hot_col(col = c(1),  renderer = text_renderer,readOnly = TRUE)   %>%    
        hot_col(col = c(2), renderer = text_renderer2) %>%
        hot_context_menu(allowRowEdit = FALSE,allowColEdit = FALSE)
      
    }
    
  })
  
  return(DF)
}

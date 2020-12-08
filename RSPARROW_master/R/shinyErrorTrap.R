#'@title shinyErrorTrap
#'@description tests for invalid user inputs in the shiny app and pops up a custom error 
#'            message, shiny map execution is halted \\cr \\cr
#'Executed By: goShinyPlot.R \\cr
#'@param input top level interactive user input in Shiny app
#'@param path_results path to results model subdirectory
#'@param badSettings data.frame of row and column number is rhandsontables with invalid 
#'       entries in the shiny app
#'@param errMsg character string custom message indicating invalid entries in the shiny app
#'@return `errMsg` character string custom message indicating invalid entries in the shiny app



shinyErrorTrap<-function(input, path_results, badSettings, errMsg){
  
  errMsg2<-NA
  
  
  # Modal error message
  modalError <- function() {
    modalDialog(
      title = errMsg,
      if (!is.na(errMsg2)){
        renderUI({errMsg2}) 
      }
      ,
      footer = tagList(
        modalButton("OK")
      )
    )
  }
  
  #check setting errors
  if (input$mapType==""){
    errMsg<-"Please select a Map Type"
  }
  
  #streamCatch 
  if (input$batch!="Batch"){
    if (input$mapType %in% c("Stream","Catchment")){
      
      if (input$mapCategory==""){
        errMsg<-"Please select a Mapping Variable Type"
      }
    }else if (input$mapType=="Source Change Scenarios"){#scenarios
      scenDir <- paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,.Platform$file.sep)
      
      if (input$scenarioName==""){
        errMsg<-"Please enter a scenario name"
      }else if (dir.exists(scenDir)){
        if (length(input$overwriteScenario)!=0){
          if (input$overwriteScenario==FALSE){
            errMsg<-paste0(input$scenarioName, "already exists.  Please enter a new scenario name or select 'Overwrite'")
          }
        }else{
          errMsg<-paste0(input$scenarioName, "already exists.  Please enter a new scenario name or select 'Overwrite'")
        }
      }#end if directory
      if (is.na(errMsg)){
        if (input$target==""){
          
          errMsg<-"Please Select Target Reach Watersheds"
          errMsg2<- shinyUI(
            fluidPage(
              h6(HTML("<b>\"default\"</b> = run scenario for watersheds above the original outlet reaches (i.e., based on the user-defined terminal reaches for the network)")), 
              h6(HTML("<b>\"waterid1\"</b> or <b>\"waterid1, waterid2, ...\"</b> = run scenario for watersheds above a single or mulitple outlet reach(es), based on the 'waterid' system variable")),
              h6(HTML("<b>\"import\"</b> = run scenario for watersheds above flagged outlet reaches, imported from ~/scenarios/flag_TargetReachWatersheds.csv (with flag = 1 ) "))
            ))
        }else if (!tolower(input$target) %in% c("default","import")){
          if (class(try(suppressWarnings(eval(parse(text= paste0("c(",input$target,")"))))))!="numeric"){
            errMsg<-"Please Select Valid Target Reach Watersheds"
            errMsg2<- shinyUI(
              fluidPage(
                h6(HTML("<b>\"default\"</b> = run scenario for watersheds above the original outlet reaches (i.e., based on the user-defined terminal reaches for the network)")), 
                h6(HTML("<b>\"waterid1\"</b> or <b>\"waterid1, waterid2, ...\"</b> = run scenario for watersheds above a single or mulitple outlet reach(es), based on the 'waterid' system variable")),
                h6(HTML("<b>\"import\"</b> = run scenario for watersheds above flagged outlet reaches, imported from ~/scenarios/flag_TargetReachWatersheds.csv (with flag = 1 ) "))
              ))
          }
        }else if (tolower(input$target)=="import"){#test for flags
          #read flag file
          filein <- paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,"flag_TargetReachWatersheds.csv")
          select_targetReachWatersheds <- fread(filein,header=TRUE,stringsAsFactors=FALSE,
                                                dec = csv_decimalSeparator,sep=csv_columnSeparator)
          
          #extract only flagged waterids
          select_targetReachWatersheds <-select_targetReachWatersheds[which(select_targetReachWatersheds$flag==1),]$waterid
          if (length(select_targetReachWatersheds)==0){
            errMsg<-paste0("No target reach watershed flagged.  \nEdit ~/scenarios/flag_TargetReachWatersheds.csv.\nPlease flag target reach watersheds with flag = 1 or change selection of 'Import' in Select Target Reach Watersheds to options shown below.")
            errMsg2<- shinyUI(
              fluidPage(
                h6(HTML("<b>\"default\"</b> = run scenario for watersheds above the original outlet reaches (i.e., based on the user-defined terminal reaches for the network)")), 
                h6(HTML("<b>\"waterid1\"</b> or <b>\"waterid1, waterid2, ...\"</b> = run scenario for watersheds above a single or mulitple outlet reach(es), based on the 'waterid' system variable")),
                h6(HTML("<b>\"import\"</b> = run scenario for watersheds above flagged outlet reaches, imported from ~/scenarios/flag_TargetReachWatersheds.csv (with flag = 1 ) "))
              ))
          }
        }
      }#end if errMsg
    }
    
  }else{#batch
    allMetrics<-as.character(unlist(input[which(regexpr("Check",names(input))>0 & names(input)!="outCheck")]))
    if (length(allMetrics)==0){
      errMsg<-"Please select at least 1 Mapping Variable"
    }
  }#end batch
  
  #hot table settings
  if (nrow(badSettings)!=0){
    errMsg<-as.character(badSettings$Test[1])
  }
  
  #show model error
  if (!is.na(errMsg)){
    showModal(modalError())
  }#if errMsg
  
  #return errMsg
  return(errMsg)
}

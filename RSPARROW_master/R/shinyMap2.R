#'@title shinyMap2
#'@description Modular Shiny app that allows the user to interactively generate Stream, 
#'            Catchment, and Site Attribute maps, as well as execute Source Change Scenarios Uses libraries shiny, 
#'            sp, data.table, maptools, rgdal, shinyWidgets, stringr, and rhandsontable Uses subroutines: 
#'            setup routines : createInteractiveChoices, createInteractiveScenarioChoices, createRTables, UIs : 
#'            streamCatch, shinySiteAttr, shinyScenarios MODS : compileALL, selectAll, updateVariable, 
#'            shinyScenariosMod, goShinyPlot \\cr \\cr
#'Executed By: \\itemize\{\\item runShiny.R
#'             \\item startModelRun.R\} \\cr
#'Executes Routines: \\itemize\{\\item compileALL.R
#'             \\item createInteractiveChoices.R
#'             \\item createRTables.R
#'             \\item goShinyPlot.R
#'             \\item handsOnMod.R
#'             \\item selectAll.R
#'             \\item shapeFunc.R
#'             \\item shinyScenarios.R
#'             \\item shinyScenariosMod.R
#'             \\item shinySiteAttr.R
#'             \\item streamCatch.R
#'             \\item testCosmetic.R
#'             \\item testRedTbl.R
#'             \\item unPackList.R
#'             \\item updateVariable.R
#'             \\item validCosmetic.R\} \\cr
#'@param map_uncertainties Vector of user selected uncertainty parameters to map, if 
#'       uncertainty analysis was not run NA
#'@param BootUncertainties Uncertainty values if available, if uncertainty analysis was not 
#'       run NA
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param subdata data.frame input data (subdata)
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'       parmMin>=0) | parmType!="SOURCE")`
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0), ]`
#'@param ConcFactor the concentration conversion factor, computed as Concentration = load / 
#'       discharge * ConcFactor
#'@param dlvdsgn design matrix imported from design_matrix.csv
#'@param reach_decay_specification the SAS IML reach decay function code from sparrow_control
#'@param reservoir_decay_specification the SAS IML reservoir decay function code from 
#'       sparrow_control
#'@param add_vars additional variables specified by the setting `add_vars` to be included in 
#'       prediction, yield, and residuals csv and shape files
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@param RSPARROW_errorOption 
#'@return `outshinyInput`  the Shiny Input list with hottables as dataframes and cosmetic 
#'            mapping settings as list objects



shinyMap2<-function(
  #stream/catchment
  file.output.list, map_uncertainties,BootUncertainties,
  data_names,mapping.input.list,
  #predict.list,
  subdata,SelParmValues,
  #site attr
  sitedata,
  #scenarios
  estimate.list,
  ConcFactor,DataMatrix.list,dlvdsgn,
  reach_decay_specification,reservoir_decay_specification,
  scenario.input.list,
  #scenarios out
  add_vars,
  #batchError
  batch_mode,
  RSPARROW_errorOption){
  
  
  suppressWarnings(suppressMessages(library(shiny)))
  suppressWarnings(suppressMessages(library(sp)))
  suppressWarnings(suppressMessages(library(data.table)))
  suppressWarnings(suppressMessages(library(maptools)))
  suppressWarnings(suppressMessages(library(rgdal)))
  suppressWarnings(suppressMessages(library(shinyWidgets)))
  suppressWarnings(suppressMessages(library(stringr)))
  suppressWarnings(suppressMessages(library(rhandsontable)))
  
  unPackList(lists = list(file.output.list = file.output.list,
                          scenario.input.list = scenario.input.list),
             parentObj = list(NA,NA)) 
  
  #load predicitons if available
  if (file.exists(paste(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list",sep=""))){
    load(paste(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list",sep=""))
  }
  
  #estimation objects
  if (file.exists(paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_JacobResults",sep=""))){
    if (!exists("JacobResults")){
      load(paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_JacobResults",sep=""))
    }
  }
  
  
  
  
  
  #set up variable choices
  choices<-createInteractiveChoices(SelParmValues,exists("predict.list"),subdata, data_names, map_uncertainties)
  
  #map type choices
  if (exists("predict.list") & exists("JacobResults")){
    mapTypeChoices<-c("","Stream","Catchment","Site Attributes","Source Change Scenarios")
    selectSources<-as.character(JacobResults$Parmnames[which(JacobResults$btype=="SOURCE")])
    
    
  }else{
    mapTypeChoices<-c("","Stream","Catchment","Site Attributes")
    selectSources<-""
  }
  
  
  scenarioRtables<-createRTables(selectSources,data_names,mapping.input.list)
  
  
  
  #setup shiny ui
  shinyApp(  ui=shinyUI(
    
    fluidPage(tags$head(
      tags$style("h5{color: red}")),
      titlePanel(
        h1(paste("Rshiny Interactive Map : ",run_id,sep=""),h5(div(HTML("DO NOT CLICK ON ITEMS ABOVE THIS POINT!"))))),
      
      sidebarLayout(
        sidebarPanel(width=6,
                     h4("SPARROW Interactive Mapping                     "),
                     br(),
                     
                     #top level user input
                     selectInput("batch","Output Mode",c("Interactive","Batch")),
                     selectInput("mapType","Map Type",mapTypeChoices),
                     
                     
                     
                     #Stream and Catchment arguments
                     streamCatch("nsStreamCatch", input, choices, map_uncertainties),
                     
                     #site Attribute arguments
                     shinySiteAttr("nsSiteAttr",input,choices),
                     
                     #scenarios arguments
                     shinyScenarios("nsScenarios",input,choices),
                     
                     #output shape file ifBatch
                     shapeFunc("nsBatch",input),
                     
                     # actionButton("showInput","Show Input"),
                     conditionalPanel(
                       condition = "input.batch=='Interactive'",
                       fluidRow(
                         actionButton("goPlot","Generate Plot"),
                         actionButton("savePDF", "SaveAs PDF"))       
                       
                     ),
                     
                     conditionalPanel(
                       condition = "input.batch=='Batch'",
                       actionButton("batchPlot","Save Plot(s)")      
                     )
        ),
        mainPanel(width = 6,
                  # verbatimTextOutput("txtOut"),
                  plotOutput("plotOne", width=900,height=900)
        )
      )))#end ui function
    ,
    
    ################################################################
    ###############################################################
    ###############################################################
    
    server=shinyServer(function(input, output,session) {
      #update red labels
      observe({
        if (input$mapType!=""){
          updateSelectInput(session, "mapType",
                            label = "Map Type"
          )
        }
        
      })  
      
      
      
      #select all and clear all buttons in drop downs 
      observe({        
        if (input$batch=="Batch"){
          if (input$mapType %in% c("Stream","Catchment")){
            lapply(1:length(as.character(unique(choices$category))), function(c) {
              category<-as.character(unique(choices$category))[c]
              if (category!="Prediction Uncertainties"){
                nsName<-paste0("ns",tolower(str_split(category," ")[[1]][1]),"Drop")
              }else{
                nsName<-"nsuncertaintyDrop"
              }
              callModule(selectAll,nsName, category = category, choices = choices)
            })
          }else{
            choicesScen<-choices[which(!choices$category %in% c("Data Dictionary Variable","Prediction Uncertainties") & regexpr("Monitoring-adjusted",choices$definition)<0),]
            ratioChoices<-data.frame(category = c("Relative Change in Load","Relative Change in Load"),
                                     variable = c("ratio_total","ratio_inc"),
                                     definition = c("Ratio of the changed total load to the baseline (unchanged) total load",
                                                    "Ratio of the changed incremental load to the baseline (unchanged) incremental load"))
            choices$category<-ifelse(choices$category=="Load Predictions","Load Predictions for Changed Sources",
                                     ifelse(choices$category=="Yield Predictions","Yield Predictions for Changed Sources",choices$category))
            choicesScen<-rbind(choicesScen,ratioChoices)
            
            lapply(1:length(as.character(unique(choicesScen$category))), function(c) {
              category<-as.character(unique(choicesScen$category))[c]
              nsName<-paste0("nsScen",tolower(str_split(category," ")[[1]][1]),"Drop")
              callModule(selectAll,nsName, category = category, choices = choicesScen)
            }) 
          }
          if (input$mapType %in% c("Stream","Catchment","Site Attributes")){
            callModule(selectAll,"nsattrDrop", category = "Data Dictionary Variable", choices = choices)
          }
          
        }
      })
      
      #update variable lists according to variable type selection in interactive mode
      observe({
        if (input$batch=="Interactive" & input$mapType %in% c("Stream","Catchment")){     
          callModule(updateVariable,"nsStreamCatch", choices= choices, mapType = input$mapType)
          
        }else if (input$batch=="Interactive" & input$mapType == "Site Attributes"){
          callModule(updateVariable,"nsSiteAttr", choices= choices, mapType = input$mapType)
        }else{
          choicesScen<-choices[which(!choices$category %in% c("Data Dictionary Variable","Prediction Uncertainties") & regexpr("Monitoring-adjusted",choices$definition)<0),]
          ratioChoices<-data.frame(category = c("Relative Change in Load","Relative Change in Load"),
                                   variable = c("ratio_total","ratio_inc"),
                                   definition = c("Ratio of the changed total load to the baseline (unchanged) total load",
                                                  "Ratio of the changed incremental load to the baseline (unchanged) incremental load"))
          choices$category<-ifelse(choices$category=="Load Predictions","Load Predictions for Changed Sources",
                                   ifelse(choices$category=="Yield Predictions","Yield Predictions for Changed Sources",choices$category))
          choicesScen<-rbind(choicesScen,ratioChoices)
          callModule(updateVariable,"nsScenarios", choices= choicesScen, mapType = input$mapType)
        }
      })
      
      
      #rTables 
      
      
      observe({
        if (input$mapType %in% c("Source Change Scenarios")){
          callModule(shinyScenariosMod,"nsScenarios",scenarioRtables,path_results)
          
        }else if (input$mapType %in% c("Stream","Catchment")){
          testRow<-testCosmetic(input, output, session, 
                                DF = as.data.frame(scenarioRtables$cosmeticPred),mapType = input$mapType)$rowNums
          callModule(validCosmetic,"nsStreamCatch-nsCosmetic", 
                     DF = as.data.frame(scenarioRtables$cosmeticPred),rowNum = testRow)
          
        }else if (input$mapType == "Site Attributes"){
          testRow<-testCosmetic(input, output, session, 
                                DF = as.data.frame(scenarioRtables$cosmeticSite),mapType = input$mapType)$rowNums
          callModule(validCosmetic,"nsSiteAttr-nsCosmetic", 
                     DF = as.data.frame(scenarioRtables$cosmeticSite),rowNum = testRow)
          
        }
      })
      #interactive plot
      output$plotOne  <- renderPlot({
        p<-eventReactive(input$goPlot, {
          
          #test bad Settings
          badSettings<-as.data.frame(matrix(0,ncol=4,nrow=0))
          names(badSettings)<-c("Setting","CurrentValue","Type","Test")
          errMsg<-NA
          if (input$mapType %in% c("Stream","Catchment")){
            badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticPred),mapType =input$mapType)$badSettings
          }else if (input$mapType == "Site Attributes"){
            badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticSite),mapType =input$mapType)$badSettings
          }else{
            errMsg1<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$sourceRed))$errMsg
            errMsg2<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDF))$errMsg
            errMsg3<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDFno))$errMsg
            
            
            errMsg<-na.omit(c(errMsg1,errMsg2, errMsg3))
            if (length(errMsg)==0){
              errMsg<-NA
            }else{
              errMsg<-errMsg[1]
            }
            
            badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticScen), "Source Change Scenarios")$badSettings
            
          }
          
          #run plot
          goShinyPlot(input, output, session, choices,"goPlot", badSettings,errMsg,
                      file.output.list,map_uncertainties,BootUncertainties,
                      data_names,mapping.input.list,
                      #predict.list,
                      subdata,SelParmValues,
                      #site attr
                      sitedata,estimate.list,#Mdiagnostics.list,
                      #scenarios
                      JacobResults,
                      ConcFactor,DataMatrix.list,dlvdsgn,
                      reach_decay_specification,reservoir_decay_specification,
                      scenario.input.list,
                      #scenarios out
                      add_vars,
                      #batchError
                      batch_mode,
                      RSPARROW_errorOption)
        })()
        
      })#end renderplot
      
      #pdf output
      observeEvent(input$savePDF, {
        #test bad Settings
        badSettings<-as.data.frame(matrix(0,ncol=4,nrow=0))
        names(badSettings)<-c("Setting","CurrentValue","Type","Test")
        errMsg<-NA
        if (input$mapType %in% c("Stream","Catchment")){
          badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticPred),mapType =input$mapType)$badSettings
        }else if (input$mapType == "Site Attributes"){
          badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticSite),mapType =input$mapType)$badSettings
        }else{
          errMsg1<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$sourceRed))$errMsg
          errMsg2<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDF))$errMsg
          errMsg3<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDFno))$errMsg
          
          
          errMsg<-na.omit(c(errMsg1,errMsg2, errMsg3))
          if (length(errMsg)==0){
            errMsg<-NA
          }else{
            errMsg<-errMsg[1]
          }
          
          badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticScen), "Source Change Scenarios")$badSettings
          
        }
        
        goShinyPlot(input, output, session, choices,"savePDF",badSettings, errMsg,
                    file.output.list, map_uncertainties,BootUncertainties,
                    data_names,mapping.input.list,
                    #predict.list,
                    subdata,SelParmValues,
                    #site attr
                    sitedata,estimate.list,#Mdiagnostics.list,
                    #scenarios
                    JacobResults,
                    ConcFactor,DataMatrix.list,dlvdsgn,
                    reach_decay_specification,reservoir_decay_specification,
                    scenario.input.list,
                    #scenarios out
                    add_vars,
                    #batchError
                    batch_mode,
                    RSPARROW_errorOption)
      })#end pdf output
      
      #batchplot
      observeEvent(input$batchPlot, {
        #test bad Settings
        badSettings<-as.data.frame(matrix(0,ncol=4,nrow=0))
        names(badSettings)<-c("Setting","CurrentValue","Type","Test")
        errMsg<-NA
        if (input$mapType %in% c("Stream","Catchment")){
          badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticPred),mapType =input$mapType)$badSettings
        }else if (input$mapType == "Site Attributes"){
          badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticSite),mapType =input$mapType)$badSettings
        }else{
          errMsg1<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$sourceRed))$errMsg
          errMsg2<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDF))$errMsg
          errMsg3<-testRedTbl(input, output, session, DF = as.data.frame(scenarioRtables$allSourcesDFno))$errMsg
          
          
          errMsg<-na.omit(c(errMsg1,errMsg2, errMsg3))
          if (length(errMsg)==0){
            errMsg<-NA
          }else{
            errMsg<-errMsg[1]
          }
          
          badSettings<-testCosmetic(input, output, session, DF = as.data.frame(scenarioRtables$cosmeticScen), "Source Change Scenarios")$badSettings
          
        }
        
        goShinyPlot(input, output, session, choices,"batchPlot",badSettings,errMsg,
                    file.output.list, map_uncertainties,BootUncertainties,
                    data_names,mapping.input.list,
                    #predict.list,
                    subdata,SelParmValues,
                    #site attr
                    sitedata,estimate.list,#Mdiagnostics.list,
                    #scenarios
                    JacobResults,
                    ConcFactor,DataMatrix.list,dlvdsgn,
                    reach_decay_specification,reservoir_decay_specification,
                    scenario.input.list,
                    #scenarios out
                    add_vars,
                    #batchError
                    batch_mode,
                    RSPARROW_errorOption)
      })#end batch plot
      session$onSessionEnded(function() {
        stopApp()
      }) 
    })#end server function
  )#end shinyApp function
}#end ShinyMap2    





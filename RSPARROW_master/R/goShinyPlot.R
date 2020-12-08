#'@title goShinyPlot
#'@description function to execute plots in shiny \\cr \\cr
#'Executed By: shinyMap2.R \\cr
#'Executes Routines: \\itemize\{\\item interactiveBatchRun.R
#'             \\item compileALL.R
#'             \\item convertHotTables.R
#'             \\item mapSiteAttributes.R
#'             \\item predictMaps.R
#'             \\item predictScenarios.R
#'             \\item shinyErrorTrap.R
#'             \\item sourceRedFunc.R
#'             \\item unPackList.R\} \\cr
#'@param input top level interactive user input in Shiny app
#'@param output shiny list object for session output
#'@param session active shiny session
#'@param choices data.frame output of function createInteractiveChoices.R
#'@param button character string indicating which button was clicked by the user in the shiny 
#'       app
#'@param badSettings data.frame of row and column number is rhandsontables with invalid 
#'       entries in the shiny app
#'@param errMsg character string custom message indicating invalid entries in the shiny app
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param map_uncertainties Vector of user selected uncertainty parameters to map, if 
#'       uncertainty analysis was not run NA
#'@param BootUncertainties Uncertainty values if available, if uncertainty analysis was not 
#'       run NA
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, 
#'                          polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, CRStext, 
#'                          convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, 
#'                          if_verify_demtarea_maps
#'@param subdata data.frame input data (subdata)
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'       parmMin>=0) | parmType!="SOURCE")`
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0
#'                & subdata$calsites==1), ]`. The object contains the dataDictionary 
#'                ‘sparrowNames’ variables, with records sorted in hydrological 
#'                (upstream to downstream) order (see the documentation Chapter 
#'                sub-section 5.1.2 for details)
#'@param estimate.list list output from `estimate.R`
#'@param JacobResults list output of Jacobian first-order partial derivatives of the model 
#'       residuals `estimateNLLSmetrics.R` contained in the estimate.list object.  For more details see 
#'       documentation Section 5.2.4.5.
#'@param ConcFactor the concentration conversion factor, computed as Concentration = load / 
#'       discharge * ConcFactor
#'@param DataMatrix.list named list of 'data' and 'beta' matrices and 'data.index.list' 
#'                       for optimization
#'@param dlvdsgn design matrix imported from design_matrix.csv
#'@param reach_decay_specification the SAS IML reach decay function code from sparrow_control
#'@param reservoir_decay_specification the SAS IML reservoir decay function code from 
#'       sparrow_control
#'@param scenario.input.list list of control settings related to source change scenarios
#'@param add_vars additional variables specified by the setting `add_vars` to be included in 
#'       prediction, yield, and residuals csv and shape files
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@param RSPARROW_errorOption yes/no control setting indicating where the RPSARROW_errorOption 
#'                            should be applied



goShinyPlot<-function(input, output, session, choices, button, badSettings,errMsg,
                      file.output.list, map_uncertainties,BootUncertainties,
                      data_names,mapping.input.list,
                      #predict.list,
                      subdata,SelParmValues,
                      #site attr
                      sitedata,estimate.list,estimate.input.list,#Mdiagnostics.list,
                      #scenarios
                      JacobResults,
                      ConcFactor,DataMatrix.list,dlvdsgn,
                      reach_decay_specification,reservoir_decay_specification,
                      scenario.input.list,if_predict,
                      #scenarios out
                      add_vars,
                      #batchError
                      batch_mode,
                      RSPARROW_errorOption){
  
  #unpack list objects making contents available by name 
  unPackList(lists = list(file.output.list = file.output.list,
                          scenario.input.list = scenario.input.list, 
                          mapping.input.list = mapping.input.list),
             parentObj = list(NA,NA, NA)) 
  
  #compile all user input and convert hottables to dataframes
  compileALL<-compileALL(input, output, session, path_results, choices)
  compiledInput<-compileALL$compiledInput
  compiledInput<-convertHotTables(compiledInput)
  compiledInput$button<-button
  
  #check for setting errors
  errMsg<-shinyErrorTrap(compiledInput,path_results, badSettings,errMsg)
  if (is.na(errMsg)){
    
    #load predicitons if available
    if (file.exists(paste0(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list"))){
      load(paste0(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list"))
    }
    
    #estimation objects
    if (file.exists(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_JacobResults"))){
      if (!exists("JacobResults")){
        load(paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_JacobResults"))
      }
    }
    
    #setup output file paths for batch and pdf output
    if (button=="savePDF" | input$batch=="Batch"){
      if (!dir.exists(paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep))){
        dir.create(paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep))
      }
      if (input$mapType=="Stream"){
        if (!dir.exists(paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"Stream",.Platform$file.sep))){
          dir.create(paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"Stream",.Platform$file.sep))
        }
        filename<- paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"Stream",.Platform$file.sep,run_id,"_",compiledInput$var,".pdf")
        batchFilename<-paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"Stream",.Platform$file.sep,"batch_",format(Sys.time(),"%Y-%m-%d_%H.%M.%S"),".RData")
        
      }else if (input$mapType=="Catchment"){
        if (!dir.exists(paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"Catchment",.Platform$file.sep))){
          dir.create(paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"Catchment",.Platform$file.sep))
        }
        filename<- paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"Catchment",.Platform$file.sep,run_id,"_",compiledInput$var,".pdf")
        batchFilename<-paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"Catchment",.Platform$file.sep,"batch_",format(Sys.time(),"%Y-%m-%d_%H.%M.%S"),".RData")
        
      }else if (input$mapType=="Site Attributes"){
        if (!dir.exists(paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"SiteAttributes",.Platform$file.sep))){
          dir.create(paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"SiteAttributes",.Platform$file.sep))
        }
        filename<- paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"SiteAttributes",.Platform$file.sep,run_id,"_SiteAttributes_",compiledInput$var,".pdf")
        batchFilename<-paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"SiteAttributes",.Platform$file.sep,"batch_",format(Sys.time(),"%Y-%m-%d_%H.%M.%S"),".RData")
        
      }else{#add check for if scenario exists ask user if proceed
        if (!dir.exists(paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,compiledInput$scenarioName,.Platform$file.sep,
                              as.character(compiledInput$outType),.Platform$file.sep))){
          dir.create(paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,compiledInput$scenarioName,.Platform$file.sep,
                           as.character(compiledInput$outType),.Platform$file.sep))
        }
        filename<- paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,compiledInput$scenarioName,.Platform$file.sep,
                         as.character(compiledInput$outType),.Platform$file.sep,
                         compiledInput$scenarioName,"_",run_id,"_",compiledInput$var,".pdf")

        batchFilename<-paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,compiledInput$scenarioName,.Platform$file.sep,"batch_",format(Sys.time(),"%Y-%m-%d_%H.%M.%S"),".RData")
        
      }
      
      #if (input$batch!="Batch" & input$enablePlotly=="no"){
      #  pdf(filename)
      #}
    }
    
    # Modal processing message
    dataModal <- function() {
      modalDialog(
        title = "Please Wait Processing Map Request...",
        
        footer = tagList(
          modalButton("OK")
        )
      )
    }
    
    
    
    #set mapping input variable
    if (input$batch!="Batch"){
      
      if (button!="savePDF"){
        
        
        if (input$mapType=="Stream" | input$mapType=="Catchment"){
          showModal(dataModal())
          
          mapScenarios<-FALSE
          scenarioFlag<-NA

          #p<<-predictMaps(compiledInput,NA,output_map_type,TRUE,
          p<-predictMaps(compiledInput,NA,output_map_type,TRUE,
                          file.output.list,
                          data_names,mapping.input.list,
                          subdata,
                          #scenarios
                          mapScenarios,
                          scenario_map_list,
                          predictScenarios.list,
                          scenarioFlag,
                          batch_mode)
          assign("p",p,envir = .GlobalEnv)

          return(p)

        }else if (input$mapType=="Site Attributes"){
          showModal(dataModal())
          
          p<-mapSiteAttributes(#Rshiny
            compiledInput,NA, path_gis, sitedata, LineShapeGeo,data_names,TRUE,
            #regular
            mapColumn,mapdata,GeoLines,mapping.input.list,
            strTitle,unitAttr,batch_mode)
          assign("p",p,envir = .GlobalEnv)
          return(p) 
          
        }else if (input$mapType=="Source Change Scenarios"){
          showModal(dataModal())
          #     compiledInput<-convertHotTables(compiledInput)
          #get source reduction functions
          compiledInput<-sourceRedFunc(compiledInput)
          
          #delete previously generated scenario output with same scenario name
          unlink(list.files(paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,compiledInput$scenarioName,.Platform$file.sep),full.names = TRUE),recursive = TRUE)
          p<- predictScenarios(#Rshiny
            compiledInput,NA, tolower(as.character(compiledInput$outType)),TRUE,
            #regular
            estimate.input.list,estimate.list,
            predict.list,scenario.input.list,
            data_names,JacobResults,if_predict,
            #bootcorrection,
            DataMatrix.list,SelParmValues,subdata,
            #predictStreamMapScenarios
            file.output.list,
            #scenarios out
            add_vars,
            mapping.input.list,
            batch_mode,
            RSPARROW_errorOption)
          assign("p",p,envir = .GlobalEnv)
          return(p)
          
        }
      }else{
        showModal(modalDialog(
          title = "",
          "Please wait for plot save action to complete",
          footer = tagList(
            modalButton("OK")
          )
        ))
        if (class(p)[1]=="gg" & input$enablePlotly=="static"){
          pdf(filename)
        }else if (class(p)[1]=="plotly" | class(p)[1]=="leaflet") {
          filename<-gsub(".pdf",".html",filename)
        }
        
        if ((class(p)[1]=="gg" & input$enablePlotly=="static") | 
            (class(p)[1]=="plotly" & input$enablePlotly=="plotly") | 
            (class(p)[1]=="leaflet" & input$enablePlotly=="leaflet")){
          if (class(p)[1]=="gg" & input$enablePlotly=="static"){
            #replayPlot(p)
            print(p)
          }else if (class(p)[1]=="plotly" | class(p)[1]=="leaflet") {
            reportPath<-paste0(path_master,"shinySavePlot.Rmd")

            #ptm <- proc.time()
            rmarkdown::render(
              reportPath, params = list(
                p = p,
                run_id = run_id
              ),
              output_file = filename, quiet = TRUE
            )
            #saveWidget(p,filename,selfcontained = FALSE)
          }
          
          if (input$enablePlotly=="static"){
            
            dev.off()
            
            
          }
          suppressWarnings(remove(p))
          showModal(modalDialog(
            title = "",
            "Plot save action complete",
            footer = tagList(
              modalButton("OK")
            )
          ))
        }else{
          if (input$mapType=="Stream" | input$mapType=="Catchment"){
            showModal(dataModal())
            mapScenarios<-FALSE
            scenarioFlag<-NA
            p<-predictMaps(compiledInput,NA,output_map_type,TRUE,
                            file.output.list,
                            data_names,mapping.input.list,
                            subdata,
                            #scenarios
                            mapScenarios,
                            scenario_map_list,
                            predictScenarios.list,
                            scenarioFlag,
                            batch_mode)
            
            #return(p)
            
          }else if (input$mapType=="Site Attributes"){
            showModal(dataModal())
            p<-mapSiteAttributes(#Rshiny
              compiledInput,NA, path_gis, sitedata, LineShapeGeo,data_names,TRUE,
              #regular
              mapColumn,mapdata,GeoLines,mapping.input.list,
              strTitle,unitAttr,batch_mode)
          }else if (input$mapType=="Source Change Scenarios"){
            showModal(dataModal())
            #     compiledInput<-convertHotTables(compiledInput)
            #get source reduction functions
            compiledInput<-sourceRedFunc(compiledInput)
            
            #delete previously generated scenario output with same scenario name
            unlink(list.files(paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,compiledInput$scenarioName,.Platform$file.sep),full.names = TRUE),recursive = TRUE)
            p<- predictScenarios(#Rshiny
              compiledInput,NA, tolower(as.character(compiledInput$outType)),TRUE,
              #regular
              estimate.input.list,estimate.list,
              predict.list,scenario.input.list,
              data_names,JacobResults,if_predict,
              #bootcorrection,
              DataMatrix.list,SelParmValues,subdata,
              #predictStreamMapScenarios
              file.output.list,
              #scenarios out
              add_vars,
              mapping.input.list,
              batch_mode,
              RSPARROW_errorOption)
            # return(p)
            
          }
          
          if (class(p)[1]=="gg" & input$enablePlotly=="static"){
            pdf(filename)
          }else if (class(p)[1]=="plotly" | class(p)[1]=="leaflet") {
            filename<-gsub(".pdf",".html",filename)
          }
          if (class(p)[1]=="gg" & input$enablePlotly=="static"){
            #replayPlot(p)
           print(p)
            #ggsave(p,height = 7, width = 7,filename = filename, units = "in")
          }else if (class(p)[1]=="plotly" | class(p)[1]=="leaflet") {
            reportPath<-paste0(path_master,"shinySavePlot.Rmd")

            #ptm <- proc.time()
            rmarkdown::render(
              reportPath, params = list(
                p = p,
                run_id = run_id
              ),
              output_file = filename, quiet = TRUE
            )
            #saveWidget(p,filename,selfcontained = FALSE)
          }
          
          if (input$enablePlotly=="static"){
            dev.off()
            
          }    
          suppressWarnings(remove(p))
          showModal(modalDialog(
            title = "",
            "Plot save action complete",
            footer = tagList(
              modalButton("OK")
            )
          ))
          return(p)
        }
        #if (button=="savePDF" & input$enablePlotly=="no"){
        # dev.off()
        
        
        #}
      } 
      
    }else{#end interactive start batch
      showModal(modalDialog(
        title = "",
        "Running batch plot output.  DO NOT CLOSE Rhiny or Rstudio while batch plot output is running!",
        footer = tagList(
          modalButton("OK")
        )
      ))
      
      if (input$mapType=="Source Change Scenarios"){
        compiledInput<-sourceRedFunc(compiledInput)
      }
      
      inputShiny<-compiledInput
      
      #make list of everything needed in batch mode
      if (exists("predict.list")){
        saveList<-c(      #interactiveStream
          "inputShiny","file.output.list","map_uncertainties","BootUncertainties",
          "data_names","mapping.input.list","predict.list","subdata","SelParmValues","LineShapeGeo",
          "lineShapeName","lineWaterid",
          #iinteractiveSiteAttr
          "sitedata", "LineShapeGeo",
          "estimate.list",#"Mdiagnostics.list",
          #interactiveScenarios
          "scenario.input.list","if_predict","JacobResults",
          "ConcFactor","DataMatrix.list","estimate.input.list",
          "reach_decay_specification","reservoir_decay_specification","dlvdsgn",
          #scenarios out
          "add_vars",
          #all
          "batch_mode")
      }else{
        saveList<-c(      #interactiveStream
          "inputShiny","file.output.list","map_uncertainties","BootUncertainties",
          "data_names","mapping.input.list","subdata","SelParmValues","LineShapeGeo",
          "lineShapeName","lineWaterid",
          #iinteractiveSiteAttr
          "sitedata", "LineShapeGeo",
          "estimate.list",#"Mdiagnostics.list",
          #interactiveScenarios
          "scenario.input.list","if_predict","JacobResults",
          "ConcFactor","DataMatrix.list","estimate.input.list",
          "reach_decay_specification","reservoir_decay_specification","dlvdsgn",
          #scenarios out
          "add_vars",
          #all
          "batch_mode")
      }
      save(list = saveList,
           
           file=batchFilename)
      
      #save batchfileName to batch folder in master
      save(list = c("path_main","batchFilename","RSPARROW_errorOption"),
           file=paste0(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"interactiveBatch.RData"))
      
      system(paste0(Sys.which("Rscript.exe")," ",file.path(paste0(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"interactiveBatchRun.R"))), wait = FALSE, invisible = FALSE)
      
      
      
    }  #end batch
    
  }#if no errMsg
  
}

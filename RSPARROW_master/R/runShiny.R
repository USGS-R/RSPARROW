#'@title runShiny
#'@description runs shiny app on previously executed RSPARROW model \\cr \\cr
#'Executed By:  \\cr
#'Executes Routines: \\itemize\{\\item shinyMap2.R
#'             \\item unPackList.R\} \\cr
#'@param enable_interactiveMaps yes/no control setting indicating whether shiny app should be 
#'       triggered at the end of the run
#'@param RSPARROW_errorOption 



runShiny<-function(file.output.list, enable_interactiveMaps,RSPARROW_errorOption){
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  run_id<-enable_interactiveMaps
  #load shinyArgs
  if (file.exists(paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,run_id,.Platform$file.sep,"maps",.Platform$file.sep,"shinyArgs"))){
    load(paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,run_id,.Platform$file.sep,"maps",.Platform$file.sep,"shinyArgs")) 
    
    
    
    #unpack shinyArgs
    for (n in names(shinyArgs)){
      assign(as.character(n),eval(parse(text = paste0("shinyArgs$",n))))
    }
    
    #trigger shiny
    shiny::runApp(shinyMap2(#stream/catchment
      file.output.list, map_uncertainties,BootUncertainties,
      data_names,mapping.input.list,
      #predict.list,
      subdata,SelParmValues,
      #site attr
      sitedata,
      #scenarios
      estimate.list,
      ConcFactor,DataMatrix.list,
      reach_decay_specification,reservoir_decay_specification,
      scenario.input.list,
      #scenarios out
      add_vars,
      #batchError
      batch_mode,
      RSPARROW_errorOption))
    
  }else{
    message(paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,run_id,.Platform$file.sep,"maps",.Platform$file.sep,"shinyArgs \nFILE NOT FOUND\nRShiny NOT AVAILABLE"))
  }
}

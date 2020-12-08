#'@title runBatchShiny
#'@description runs shiny app on previously executed RSPARROW model in batch mode from user
#'             specified browser \\cr \\cr
#'Executes Routines: shinyBatch.R \\cr
#'@param path_PastResults character string path to previously generated results including run_id subdirectory
#'@param path_shinyBrowser character string path to browser executible, if NA defualt browser will be used
#'@examples
#'path_master<-'./RSPARROW_master'
#'suppressWarnings(remove(list="runRsparrow"))
#'devtools::load_all(path_master,recompile = FALSE)
#'runBatchShiny("~/UserDirectory/results/customRunId") #uses default browser
#'runBatchShiny("~/UserDirectory/results/customRunId",path_shinyBrowser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")


runBatchShiny<-function(path_PastResults, path_shinyBrowser = NA){
  path_PastResults<-paste0(path_PastResults,"/maps/shinyArgs")
  
  if (file.exists(path_PastResults)){
    load(path_PastResults)
    
    shinyArgs$path_shinyBrowser<-path_shinyBrowser
    save(shinyArgs, 
         file = paste0(shinyArgs$file.output.list$path_main,.Platform$file.sep,"batch",.Platform$file.sep,"shinyBatch.RData"))

    system(paste0(Sys.which("Rscript.exe")," ",
                 file.path(paste0(shinyArgs$file.output.list$path_main,.Platform$file.sep,"batch",.Platform$file.sep,"shinyBatch.R"))), 
           wait = FALSE, invisible = FALSE)
    
    
  }else{
    message(paste0(path_PastResults,.Platform$file.sep,results_directoryName,.Platform$file.sep,run_id,.Platform$file.sep,"maps",.Platform$file.sep,"shinyArgs \nFILE NOT FOUND\nRShiny NOT AVAILABLE"))
  }
}
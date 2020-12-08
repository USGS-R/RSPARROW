#find path to this file
cmdArgs = commandArgs(trailingOnly = FALSE)
needle = "--file="
match = grep(needle, cmdArgs)
res<-normalizePath(sub(needle, "", cmdArgs[match]))

#get paths
load(gsub("shinyBatch.R","shinyBatch.RData",res))

#load RSPARROW
runRsparrow<-"no"
devtools::load_all(shinyArgs$file.output.list$path_main,recompile = FALSE)

#source(paste0(shinyArgs$file.output.list$path_master,.Platform$file.sep,"unPackList.R"))

unPackList(lists = list(shinyArgs = shinyArgs),
           parentObj = list(NA)) 
unPackList(lists = list(file.output.list = file.output.list),
           parentObj = list(NA)) 

# source(paste0(path_master,"/executionTree.R"))
# #get list of required functions
# listFuncs<-executionTree(path_master, startRoutine = "goShinyPlot.R", outputType = "data.frame",includeTypes = "all")
# listFuncs<-as.data.frame(listFuncs)
# listFuncs<-listFuncs[,regexpr("line",names(listFuncs))<0]
# listFuncs<-as.matrix(listFuncs)
# listFuncs<-na.omit(unique(listFuncs[!duplicated(listFuncs)]))
# listFuncs<-paste0(path_master,"/",listFuncs)
# listFuncs<-listFuncs[endsWith(listFuncs,".R")]
# 
# #load required functions
# sapply(listFuncs, source)
# reqFuncs<-c("shinyMap2.R", "unPackList.R", "createInteractiveChoices.R", 
#             "createRTables.R", "streamCatch.R", "shinySiteAttr.R", "shinyScenarios.R", 
#             "shapeFunc.R", "selectAll.R", "updateVariable.R", "shinyScenariosMod.R", 
#             "testCosmetic.R", "validCosmetic.R", "testRedTbl.R", "goShinyPlot.R", 
#             "dropFunc.R", "handsOnUI.R", "handsOnMod.R", "compileInput.R", 
#             "convertHotTables.R", "getNumSett.R", "getSpecialSett.R", "compileALL.R", 
#             "shinyErrorTrap.R", "predictMaps.R", "mapSiteAttributes.R", "sourceRedFunc.R", 
#             "predictScenarios.R", "allowRemoveRow.R", "areColors.R", "checkBinaryMaps.R", 
#             "mapBreaks.R", "addMarkerText.R", "replaceNAs.R", "predictScenariosPrep.R", 
#             "predictScenariosOutCSV.R", "outputSettings.R", "hydseqTerm.R", 
#             "getVarList.R", "getCharSett.R", "getShortSett.R", "getYesNoSett.R", 
#             "getOptionSett.R", "named.list.R","errorOccurred.R")
# reqFuncs<-paste0(file.output.list$path_master,reqFuncs)
# reqFuncs<-sapply(reqFuncs, source)
# 
# #load libraries
# suppressWarnings(suppressMessages(library(shiny)))


if (RSPARROW_errorOption=="yes"){
  #errorhandle
  backupOptions<-list(error = options()$error,
                      show.error.locations = options()$show.error.locations,
                      keep.source = options()$keep.source)
  options(error = quote({
    #print custom message to console
    message("\nRSPARROW SYSTEM ERROR OCCURRED");
    #instruct the user to reset their options
    message('To reset user options in R use options(backupOptions)'); 
    #First dump error stack to file; not accessible by the R session.
    dump.frames("errorDump", to.file=TRUE, include.GlobalEnv=TRUE); 
    #sink to file
    sink(file=paste0(path_results,"error.log")); 
    #print custom error message to file
    cat("RSPARROW SYSTEM ERROR OCCURRED\n");
    #instruct the user to reset their options
    cat('To reset user options in R use options(backupOptions)\n \n'); 
    #Dump again to get error message and write it to error log; 
    #accessible by the R session.
    dump.frames(); 
    #Print simple error message to file
    cat(attr(last.dump,"error.message")); 
    cat('\nTraceback:');
    cat('\n'); #line space
    # Print full traceback of function calls. 
    #The '2' omits the outermost two function calls in the traceback.
    traceback(2); 
    shell.exec(paste0(path_results,"error.log"));
    sink() #end sink
  }),
  #show line numbers in traceback (shown as 'from #4')
  #line numbers count from the function call (i.e. `nestedFunc<-function(){` is line 1)
  show.error.locations = TRUE,keep.source = TRUE)
}#end Error 

if (!is.na(path_shinyBrowser)){
  if (file.exists(path_shinyBrowser)){
options(browser = path_shinyBrowser)
  }else{
    message(paste0("INVALID path_shinyBrowser : ",path_shinyBrowser,"/nDefault browser will be used for shiny"))
  }
}

#trigger shiny
shiny::runApp(shinyMap2(
  #stream/catchment
  file.output.list,map_uncertainties,BootUncertainties,
  data_names,mapping.input.list,
  #predict.list,
  subdata,SelParmValues,
  #site attr
  sitedata,
  #scenarios
  estimate.list,estimate.input.list,
  ConcFactor,DataMatrix.list,dlvdsgn,
  reach_decay_specification,reservoir_decay_specification,
  scenario.input.list,if_predict,
  #scenarios out
  add_vars,
  #batchError
  batch_mode,
  RSPARROW_errorOption),launch.browser = TRUE)
stopApp()


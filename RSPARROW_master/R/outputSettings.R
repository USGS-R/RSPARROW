#'@title outputSettings
#'@description outputs archived list of all control settings \\cr \\cr
#'Executed By: \\itemize\{\\item controlFileTasksModel.R
#'             \\item executeRSPARROW.R
#'             \\item predictScenarios.R
#'             \\item setupMaps.R
#'             \\item startModelRun.R\} \\cr
#'Executes Routines: \\itemize\{\\item getCharSett.R
#'             \\item getNumSett.R
#'             \\item getOptionSett.R
#'             \\item getShortSett.R
#'             \\item getYesNoSett.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param save TRUE/FALSE indicating whether control setting values are to be saved to a csv 
#'       file
#'@return `settings` data.frame of all user control file settings and setting values



outputSettings<-function(file.output.list,save){
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  #get all settings
  settings<-c(getCharSett(),getNumSett(),getShortSett(),getYesNoSett())
  
  #format options settings
  optSettings<-getOptionSett()
  optSettings<-unlist(lapply(optSettings,function(x) trimws(strsplit(x,"=")[[1]][1])))
  settings<-c(settings,optSettings)
  
  #make dataframe and add values for settings
  settings<-data.frame(setting = settings)
  settings$value<-NA
  for (s in settings$setting){
    settings[which(settings$setting==s),]$value<-paste(capture.output(dput(get(s))),collapse=", ")
  }
  
  if (save){
    #output to csv
    fwrite(settings, file=paste0(path_results,run_id,"_userSettings.csv"),
           showProgress = FALSE,row.names=FALSE,dec = csv_decimalSeparator,sep=csv_columnSeparator,
           col.names = TRUE,na = "NA")
  }
  
  
  return(settings)
  
}#end function

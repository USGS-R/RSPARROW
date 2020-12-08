#'@title openDesign
#'@description opens the design_matrix.csv control file for edit \\cr \\cr
#'Executed By: executeRSPARROW.R \\cr
#'@param path_user character string path to RSPARROW user directory containing results, data, 
#'       and gis subdirectories
#'@param results_directoryName character string indicating the users results subdirectory name



openDesign<-function(path_user,results_directoryName){
  #test filePath
  if (!file.exists(paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,"design_matrix.csv"))){
    message(cat("NO DESIGN MATRIX FILE FOUND IN RESULTS DIRECTORY.\n",paste(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,sep="")))
  }else{
    shell.exec(paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,"design_matrix.csv"))}
}

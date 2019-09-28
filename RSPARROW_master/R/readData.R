#'@title readData
#'@description Reads the users input data (either .csv or binary format) \\cr \\cr
#'Executed By: \\itemize\{\\item createInitialDataDictionary.R
#'             \\item dataInputPrep.R\} \\cr
#'Executes Routines: unPackList.R \\cr
#'@param input_data_fileName name of users data1 file
#'@return `data1` input data (data1)



readData <- function(file.output.list,input_data_fileName){
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  path<-path_data
  
  ptm <- proc.time()
  
  filedata1 <- paste(path,input_data_fileName,sep="")   
  if (regexpr(".csv",input_data_fileName)>0){
    data1 <- read.csv(filedata1,header=TRUE,stringsAsFactors=FALSE,
                      dec = csv_decimalSeparator,sep=csv_columnSeparator)
    data1BinaryName<-gsub(".csv","",input_data_fileName)
    save(data1, file=paste(path_data,data1BinaryName,sep=""))
  }else{
    load(filedata1)
  }
  
  
  cat("head(data1)\n\n")
  print(head(data1))
  cat("\n\n")
  cat("nrow(data1)\n\n")
  print(nrow(data1))
  cat("\n\n")
  cat("Time elapsed during data import.\n ")
  print(proc.time() - ptm)
  cat("\n\n")
  
  
  return(data1)
  
}#end function


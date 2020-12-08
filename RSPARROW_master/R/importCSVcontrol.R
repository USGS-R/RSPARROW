#'@title importCSVcontrol
#'@description Imports csv control file and checks/corrects the number of columns used by the 
#'            'syncVarNames' function.  \\cr \\cr
#'Executed By: \\itemize\{\\item addVars.R
#'             \\item createInitialParameterControls.R
#'             \\item createMasterDataDictionary.R
#'             \\item read_dataDictionary.R
#'             \\item readDesignMatrix.R
#'             \\item readParameters.R
#'             \\item syncVarNames.R\} \\cr
#'Executes Routines: \\itemize\{\\item errorOccurred.R
#'             \\item unPackList.R\} \\cr
#'@param filein path to input file in csv format
#'@param Ctype column class designation for required columns
#'@param NAMES column names for required columns
#'@param strEndMessage message string to be evaluated
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param exitRun TRUE/FALSE states whether code should terminate
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@return `data` data object imported from csv control file



importCSVcontrol<-function(filein,Ctype,NAMES,strEndMessage,
                           file.output.list,
                           exitRun,batch_mode){
  
  tryIt<-try({ 
    
    
    unPackList(lists = list(file.output.list = file.output.list),
               parentObj = list(NA)) 
    
    fileName<-basename(filein)
    
    #check file for correct number of fields
    numberFields<-max(count.fields(filein,sep=csv_columnSeparator))
    numberFields<-numberFields-length(Ctype)
    if (numberFields>0){
      Ctype<-c(Ctype,rep("NULL",numberFields))
      numberFieldsOut<-list(Ctype=Ctype,numberFields=numberFields)
    }else if (numberFields<0){#not enough columns invalid file
      message(paste0("ERROR: INVALID ",fileName," FILE '",filein,"' CHECK NUMBER OF COLUMNS\n", 
                    fileName," FILE SHOULD HAVE THE FOLLOWING COLUMNS:\n "))
      for (i in NAMES){
        message(i)
      }
      message(eval(parse(text=strEndMessage)))
      if (batch_mode=="yes"){
        cat("ERROR: INVALID ",fileName," FILE '",filein,"' CHECK NUMBER OF COLUMNS\n", 
            fileName," FILE SHOULD HAVE THE FOLLOWING COLUMNS:\n ",sep="")
        for (i in NAMES){
          cat(i)
        }
        cat(eval(parse(text=strEndMessage)))
        
      }#end batch_mode
      
      
      if (exitRun){
        errorOccurred("importCSVcontrol.R",batch_mode)    
      }else{
        data<-"error"
      }
      
    }
    
    
    #read file
    if (regexpr("design_matrix.csv",filein)>0){
      data<-read.csv(filein,header=TRUE,row.names=1,dec = csv_decimalSeparator,sep=csv_columnSeparator)
    }else{
      data<-fread(file = filein,sep = csv_columnSeparator, dec = csv_decimalSeparator, 
                  header = TRUE,colClasses=Ctype)
    }
    
    #remove excess fields
    data<-data[,1:(length(Ctype)-numberFields)]         
    #remove columns/rows with all missing
    if (regexpr("design_matrix.csv",filein)>0){
      data<-as.data.frame(matrix(data[apply(data,1, function(x) any(!is.na(x))),],
                                 ncol=ncol(data),nrow=nrow(data),
                                 dimnames = list(rownames(data),colnames(data))))
    }else{
      data<-data[apply(data,1, function(x) any(!is.na(x))),]
    }
    
    names(data)<-NAMES
    
    
    
  },TRUE)#end try
  
  
  
  return(data)
  
  
  
}

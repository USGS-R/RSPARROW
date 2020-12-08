#'@title addVars
#'@description Tests for sparrowNames found in parameters.csv, but not in dataDictionary.csv 
#'            or design_matrix.csv. Edits dataDictionary.csv and/or design_matrix.csv adding missing 
#'            sparrowNames and opens dataDictionary.csv, design_matrix.csv and userModifyData.R for edit. \\cr \\cr
#'Executed By: executeRSPARROW.R \\cr
#'Executes Routines: \\itemize\{\\item errorOccurred.R
#'             \\item getVarList.R
#'             \\item importCSVcontrol.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



addVars<-function(file.output.list,batch_mode){
  
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  
  #read parameters file
  filebetas<-paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,"parameters.csv")
  Ctype <- c("character","character","character","numeric","numeric","numeric","character","numeric")
  NAMES<- c("sparrowNames","description","parmUnits","parmInit","parmMin","parmMax","parmType","parmCorrGroup")   
  
  #check file for correct number of fields
  #import parameters
  betavalues<-importCSVcontrol(filebetas,Ctype,NAMES,"paste0('\n \nRUN EXECUTION TERMINATED')",
                               file.output.list,TRUE,batch_mode)
  
  #trim whitespaces
  betavalues$sparrowNames<-trimws(betavalues$sparrowNames,which="both")
  #make fixed and required names lowercase
  betavalues$sparrowNames<-ifelse(tolower(betavalues$sparrowNames) %in% as.character(getVarList()$varList),tolower(betavalues$sparrowNames),betavalues$sparrowNames)
  
  
  
  #read dataDictionary
  filein <- paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,"dataDictionary.csv")
  Ctype <- c("character","character","character","character","character")
  NAMES<-c("varType","sparrowNames","data1UserNames","varunits","explanation")
  
  #check file for correct number of fields
  #import dataDictionary
  data_names<-importCSVcontrol(filein,Ctype,NAMES,"paste0('\n \nRUN EXECUTION TERMINATED')",
                               file.output.list,TRUE,batch_mode)
  
  #trim whitespaces
  data_names$sparrowNames<-trimws(data_names$sparrowNames,which="both")
  data_names$data1UserNames<-trimws(data_names$data1UserNames,which="both")  
  #make fixed and required names lowercase
  data_names$sparrowNames<-ifelse(tolower(data_names$sparrowNames) %in% as.character(getVarList()$varList),tolower(data_names$sparrowNames),data_names$sparrowNames)
  
  fileDic<-paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,"dataDictionary.csv")
  
  
  #read designMatrix
  filed <- paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,"design_matrix.csv")
  
  #columns for DELIVF
  NAMES<-betavalues[which(betavalues$parmType=="DELIVF"),]$sparrowNames
  Ctype<-seq(1:length(NAMES))
  
  #check file for correct number of fields
  #import
  dmatrixin<-importCSVcontrol(filed,Ctype,NAMES,"paste0('\n \nRUN EXECUTION TERMINATED')",
                              file.output.list,TRUE,batch_mode)
  
  
  #trim whitespaces
  rownames(dmatrixin)<-trimws(rownames(dmatrixin),which="both")
  names(dmatrixin)<-trimws(names(dmatrixin),which="both")
  #make fixed and required names lowercase
  rownames(dmatrixin)<-ifelse(tolower(rownames(dmatrixin)) %in% as.character(getVarList()$varList),tolower(rownames(dmatrixin)),rownames(dmatrixin))
  names(dmatrixin)<-ifelse(tolower(names(dmatrixin)) %in% as.character(getVarList()$varList),tolower(names(dmatrixin)),names(dmatrixin))
  
  
  fileDesign<-paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,"design_matrix.csv")
  
  
  #test for parameters NOT in dataDictionary
  test_data_names<-as.data.frame(betavalues[which(!betavalues$sparrowNames %in% data_names$sparrowNames),])
  names(test_data_names)<-names(betavalues)
  if (nrow(test_data_names)!=0){#missing from dataDictionary
    for (t in test_data_names$sparrowNames){
      message(paste0("ERROR: ",t," PARAMETER NOT FOUND IN dataDictonary.csv\n ",t, 
                    " HAS BEEN ADDED TO dataDictionary.csv\n USER MUST EDIT 
dataDictionary.csv, userModifyData.R, and design_matrix.R\n 
TO ALLOW FOR NEW PARAMETER\n \nDATA IMPORT MUST BE RE_RUN\n
SET run_dataImport<-'yes' AND load_previousDataImport<-'no'\n \n"))
      new_data_names<-data.frame(varType = test_data_names[which(test_data_names$sparrowNames==t),]$parmType,
                                 sparrowNames=t,
                                 data1UserNames = NA,
                                 varunits = NA,
                                 explanation = test_data_names[which(test_data_names$sparrowNames==t),]$description)
      
      #check against fixed and required list
      if (t %in% getVarList()$reqNames){
        new_data_names$varType<-"REQUIRED"
        explanations<-getVarList()$explanations
        new_data_names$explanation<-as.character(explanations[which(explanations$sparrowNames==t),]$explanation)
      }else if (t %in% getVarList()$fixNames){
        new_data_names$varType<-"FIXED"
        explanations<-getVarList()$explanations
        new_data_names$explanation<-as.character(explanations[which(explanations$sparrowNames==t),]$explanation)
      }
      
      data_names<-rbind(data_names,new_data_names)
    }#end for
    
    #save dataDictionary
    fwrite(data_names,file=fileDic,row.names=F,append=F,quote=T,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    
  }#end if test_data_names
  
  #check against design_matrix if SOURCE or DELIVF 
  test_design<-c(rownames(dmatrixin),names(dmatrixin)[which(names(dmatrixin)!="sparrowNames")])
  test_design<-as.data.frame(betavalues[which(!betavalues$sparrowNames %in% test_design
                                              & betavalues$parmType %in% c("SOURCE","DELIVF")),])
  names(test_design)<-names(betavalues)
  if (nrow(test_design)!=0){#missing from design_matrix
    
    for (t in test_design$sparrowNames){
      if (test_design[which(test_design$sparrowNames==t),]$parmType=="SOURCE"){
        message(paste0("ERROR: ",t," PARAMETER NOT FOUND IN design_matrix.csv as SOURCE\n ",t, 
                      " HAS BEEN ADDED TO design_matrix.csv\n USER MUST EDIT design_matrix.csv, userModifyData.R\n 
TO ALLOW FOR NEW PARAMETER\n \nDATA IMPORT MUST BE RE_RUN\n
SET run_dataImport<-'yes' AND load_previousDataImport<-'no'\n \n"))
        
        new_design_matrix<-as.data.frame(matrix(rep(0,length(dmatrixin)),ncol=length(dmatrixin),nrow=1))
        names(new_design_matrix)<-names(dmatrixin)
        rownames(new_design_matrix)<-t
        
        dmatrixin<-rbind(dmatrixin,new_design_matrix)
      }else{
        message(paste0("ERROR: ",t," PARAMETER NOT FOUND IN design_matrix.csv as DELIVF\n ",t, 
                      " HAS BEEN ADDED TO design_matrix.csv\n USER MUST EDIT design_matrix.csv, userModifyData.R\n 
TO ALLOW FOR NEW PARAMETER\n \nDATA IMPORT MUST BE RE_RUN\n
SET run_dataImport<-'yes' AND load_previousDataImport<-'no'\n \n"))
        
        new_design_matrix<-as.data.frame(matrix(rep(0,nrow(dmatrixin)),ncol=1,nrow=nrow(dmatrixin)))
        names(new_design_matrix)<-t
        
        dmatrixin<-cbind(dmatrixin,new_design_matrix)
      }#end ifelse
    }#end for 
    
    #save design_matrix
    dmatrixin<-cbind(rownames(dmatrixin),dmatrixin)
    names(dmatrixin)[1]<-"sparrowNames"
    
    #order according to parameters
    dmatrixin<-dmatrixin[match(betavalues[which(betavalues$parmType=="SOURCE"),]$sparrowNames,dmatrixin$sparrowNames),]
    dmatrixin<-dmatrixin[,match(c("sparrowNames",betavalues[which(betavalues$parmType=="DELIVF"),]$sparrowNames),names(dmatrixin))]
    
    fwrite(dmatrixin,file=fileDesign,row.names=F,append=F,quote=F,showProgress = FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  }#end if test_design
  
  #if missing parameter variables found open files for edit and terminate run
  if (nrow(test_design)!=0 | nrow(test_data_names)!=0){
    message(paste0("USER MUST EDIT CONTROL FILES WITH MISSING PARAMETER INFORMATION\ndesign_matrix.csv, dataDictionary.csv, and userModifyData.R ARE OPEN FOR EDIT\n RUN EXECUTION TERMINATED"))
    shell.exec(paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,"design_matrix.csv"))
    shell.exec(paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,"dataDictionary.csv"))
    file.edit(paste0(path_user,.Platform$file.sep,results_directoryName,.Platform$file.sep,"userModifyData.R"))
    errorOccurred("addVars.R",batch_mode)
  }
  
  
  
}#end function


#'@title readDesignMatrix
#'@description Reads the land-to-water and source interaction matrix in the 
#'            'design_matrix.csv' file.  \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'Executes Routines: \\itemize\{\\item getVarList.R
#'             \\item importCSVcontrol.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param betavalues data.frame of model parameters from parameters.csv
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@return `dmatrixin` imported object from design_matrix.csv



readDesignMatrix <- function(file.output.list,betavalues,batch_mode){
  
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  filed <- paste0(path_results,run_id,"_design_matrix.csv")
  
  #columns for DELIVF
  NAMES<-betavalues[which(betavalues$parmType=="DELIVF"),]$sparrowNames
  Ctype<-seq(1:length(NAMES))
  
  #read file
  dmatrixin<-importCSVcontrol(filed,Ctype,NAMES,"paste0('\n \nRUN EXECUTION TERMINATED')",
                              file.output.list,TRUE,batch_mode)
  
  #trim whitespaces
  rownames(dmatrixin)<-trimws(rownames(dmatrixin),which="both")
  names(dmatrixin)<-trimws(names(dmatrixin),which="both")
  #make fixed and required names lowercase
  rownames(dmatrixin)<-ifelse(tolower(rownames(dmatrixin)) %in% as.character(getVarList()$varList),tolower(rownames(dmatrixin)),rownames(dmatrixin))
  names(dmatrixin)<-ifelse(tolower(names(dmatrixin)) %in% as.character(getVarList()$varList),tolower(names(dmatrixin)),names(dmatrixin))
  
  #order according to parameters
  dmatrixin<-as.data.frame(matrix(dmatrixin[match(betavalues[which(betavalues$parmType=="SOURCE"),]$sparrowNames,rownames(dmatrixin)),],
                                  ncol=ncol(dmatrixin),nrow=nrow(dmatrixin),dimnames = list(rownames(dmatrixin),colnames(dmatrixin))))
  dmatrixin<-as.data.frame(matrix(dmatrixin[,match(betavalues[which(betavalues$parmType=="DELIVF"),]$sparrowNames,names(dmatrixin))],
                                  ncol=ncol(dmatrixin),nrow=nrow(dmatrixin),dimnames = list(rownames(dmatrixin),colnames(dmatrixin))))
  
  
  return(dmatrixin)
  
}#end function

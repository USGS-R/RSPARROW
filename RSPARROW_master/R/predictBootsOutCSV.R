#'@title predictBootsOutCSV
#'@description Outputs the bootstrap predictions to CSV files.  \\cr \\cr
#'Executed By: controlFileTasksModel.R \\cr
#'Executes Routines: \\itemize\{\\item getVarList.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param estimate.list list output from `estimate.R`
#'@param predictBoots.list contains parametric bootstrap predictions for load and yield. 
#'                         For more details see documentation Section 5.3.2.3
#'@param subdata data.frame input data (subdata)
#'@param add_vars additional variables specified by the setting `add_vars` to be included in 
#'       prediction, yield, and residuals csv and shape files
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file



predictBootsOutCSV <- function(file.output.list,estimate.list,predictBoots.list,
                               subdata,add_vars,data_names) {
  
  #################################################
  
  
  # create global variable from list names (JacobResults)
  # 'oEstimate' containing the estimated mean parameters for all non-constant and constant parameters
  # 'Parmnames' list of variable names 
  # create global variable from list names (predictBoots.list)
  # transfer required variables to global environment from SUBDATA
  unPackList(lists = list(JacobResults = estimate.list$JacobResults,
                          datalstCheck = as.character(getVarList()$varList),
                          predictBoots.list = predictBoots.list,
                          file.output.list = file.output.list),
             parentObj = list(NA,
                              subdata = subdata,
                              NA,
                              NA))
  
  #test if waterid was renumbered, if so add it to add_vars
  origWaterid<-as.character(data_names[which(data_names$sparrowNames=="waterid"),]$data1UserNames)
  if (origWaterid!="waterid"){
    add_vars<-c(origWaterid,add_vars)    
  }else{
    add_vars<-c("waterid_for_RSPARROW_mapping",add_vars)
  }
  
  #get user selected additional variables (add_vars)
  add_vars<-add_vars[which(add_vars!="waterid")]
  add_vars<-add_vars[!duplicated(add_vars)]
  if (length(na.omit(add_vars))!=0){
    for (a in na.omit(add_vars)){
      if (a!=as.character(data_names[which(data_names$sparrowNames=="waterid"),]$data1UserNames) & a!="waterid_for_RSPARROW_mapping"){
        if (a %in% names(subdata)){
          if (a==add_vars[1]){#if a is first add_var
            addSubdataVars<-subdata[,which(names(subdata) %in% c("waterid",a))] 
          }else{
            addSubdataVarstemp<-data.frame(temp = subdata[,which(names(subdata) %in% a)])
            names(addSubdataVarstemp)<-a
            addSubdataVars<-cbind(addSubdataVars,addSubdataVarstemp)
          }
        }#if a in subdata
      }else{# a == data1UserNames where sparrowName = waterid |a=="waterid_for_RSPARROW_mapping"
        if (a=="waterid_for_RSPARROW_mapping"){
          tempName<-"originalWaterid"
          tempCol<-"waterid_for_RSPARROW_mapping"
        }else{
          tempName<-as.character(data_names[which(data_names$sparrowNames=="waterid"),]$data1UserNames)
          tempCol<-"waterid"
        }
        if (a==add_vars[1]){#if a is first add_var
          addSubdataVars<-data.frame(waterid = subdata[,which(names(subdata) %in% tempCol)])
          addSubdataVars<-cbind(addSubdataVars,data.frame(temp = subdata[,which(names(subdata) %in% tempCol)]))
          names(addSubdataVars)[2]<-tempName
        }else{
          addSubdataVarstemp<-data.frame(temp = subdata[,which(names(subdata) %in% tempCol)])
          names(addSubdataVarstemp)[1]<-tempName
          addSubdataVars<-cbind(addSubdataVars,addSubdataVarstemp)
        } 
      }# a == data1UserNames where sparrowName = waterid
      
    }#for a in add_vars
  }  
  
  # Output load predictions
  outvars <- as.data.frame(bootmatrix)
  colnames(outvars) <- boparmlist  
  
  rchname <- subdata$rchname
  rchname <- gsub(",", "", rchname)
  predatts <- data.frame(waterid,rchname,rchtype,headflag,termflag,demtarea,
                         demiarea,meanq,fnode,tnode,hydseq,frac,iftran,staid)
  outvars2 <- merge(predatts,outvars,by="waterid",all.y=TRUE,all.x=TRUE)
  if (length(na.omit(add_vars))!=0){
    outvars2 <-merge(outvars2,addSubdataVars,by="waterid",all.x=TRUE,all.y=TRUE)
    
    #test if origWaterid column is in addsubdataVars, if so reorder columns so that origWaterid is in column 2
    origWaterid<-names(addSubdataVars)[which(names(addSubdataVars) %in% c("originalWaterid",as.character(data_names[which(data_names$sparrowNames=="waterid"),]$data1UserNames)) 
                                             & names(addSubdataVars)!="waterid")]
    if (length(origWaterid)!=0){
      outvars2<-outvars2[,match(c("waterid",origWaterid,names(outvars2)[which(!names(outvars2) %in% c("waterid",origWaterid))]),names(outvars2))]
    }
    
  }# if add_vars
  
  outvars2 <- outvars2[with(outvars2,order(outvars2$hydseq,outvars2$waterid)), ]
  
  fileout <- paste0(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predicts_load_boots.csv")
  fwrite(outvars2,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  # Output yield predictions
  outvars <- as.data.frame(bootyldmatrix)
  colnames(outvars) <- byldoparmlist  
  
  rchname <- subdata$rchname
  rchname <- gsub(",", "", rchname)
  predatts <- data.frame(waterid,rchname,rchtype,headflag,termflag,demtarea,
                         demiarea,meanq,fnode,tnode,hydseq,frac,iftran,staid)
  outvars2 <- merge(predatts,outvars,by="waterid",all.y=TRUE,all.x=TRUE)
  if (length(na.omit(add_vars))!=0){
    outvars2 <-merge(outvars2,addSubdataVars,by="waterid",all.x=TRUE,all.y=TRUE)
    
    #test if origWaterid column is in addsubdataVars, if so reorder columns so that origWaterid is in column 2
    origWaterid<-names(addSubdataVars)[which(names(addSubdataVars) %in% c("originalWaterid",as.character(data_names[which(data_names$sparrowNames=="waterid"),]$data1UserNames)) 
                                             & names(addSubdataVars)!="waterid")]
    if (length(origWaterid)!=0){
      outvars2<-outvars2[,match(c("waterid",origWaterid,names(outvars2)[which(!names(outvars2) %in% c("waterid",origWaterid))]),names(outvars2))]
    }
    
  }# if add_vars
  outvars2 <- outvars2[with(outvars2,order(outvars2$hydseq,outvars2$waterid)), ]
  
  fileout <- paste0(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predicts_yield_boots.csv")
  fwrite(outvars2,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  
}#end function


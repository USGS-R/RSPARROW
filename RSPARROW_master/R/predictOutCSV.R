#'@title predictOutCSV
#'@description Outputs the standard predictions to CSV files.  \\cr \\cr
#'Executed By: controlFileTasksModel.R \\cr
#'Executes Routines: \\itemize\{\\item getVarList.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param estimate.list list output from `estimate.R`
#'@param predict.list archive with all load and yield prediction variables to provide for 
#'                    the efficient access and use of predictions in subsequent execution 
#'                    of the parametric bootstrap predictions and uncertainties, mapping, 
#'                    and scenario evaluations.  For more details see documentation Section 
#'                    5.3.1.5
#'@param subdata data.frame input data (subdata)
#'@param add_vars additional variables specified by the setting `add_vars` to be included in 
#'       prediction, yield, and residuals csv and shape files
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file



predictOutCSV <- function(file.output.list,estimate.list,predict.list,
                          subdata,add_vars,data_names) {
  
  
  
  # create global variable from list names (JacobResults)
  # 'oEstimate' containing the estimated mean parameters for all non-constant and constant parameters
  # 'Parmnames' list of variable names 
  # create global variable from list names
  # transfer required variables to global environment from SUBDATA
  unPackList(lists = list(JacobResults = estimate.list$JacobResults,
                          datalstCheck = as.character(c(getVarList()$varList,"waterid_for_RSPARROW_mapping")),
                          predict.list = predict.list,
                          file.output.list = file.output.list),
             parentObj = list(NA,
                              subdata = subdata,
                              NA,
                              NA))
  #test if waterid was renumbered, if so add it to add_vars
  origWaterid<-as.character(data_names[which(data_names$sparrowNames=="waterid"),]$data1UserNames)
  
  if (unique(unique(waterid_for_RSPARROW_mapping-waterid)!=0) & length(unique(unique(waterid_for_RSPARROW_mapping-waterid)!=0))==1){
    add_vars<-c("waterid_for_RSPARROW_mapping",add_vars)
  }else if (origWaterid!="waterid"){
    add_vars<-c(origWaterid,add_vars)
  }
  
  #get user selected additional variables (add_vars)
  add_vars<-c("waterid",add_vars)
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
          addSubdataVars<-data.frame(waterid = subdata[,which(names(subdata) %in% tempCol)],
                                     temp = subdata[,which(names(subdata) %in% tempCol)])
          names(addSubdataVars)[2]<-tempName
        }else{
          
          addSubdataVarstemp<-data.frame(temp = subdata[,which(names(subdata) %in% tempCol)])
          names(addSubdataVarstemp)[1]<-tempName
          addSubdataVars<-cbind(addSubdataVars,addSubdataVarstemp)
        } 
      }# a == data1UserNames where sparrowName = waterid
    }#for a in add_vars
    names(addSubdataVars)[1]<-"waterid"
  }
  addSubdataVars<-subset(addSubdataVars, select=which(!duplicated(names(addSubdataVars)))) 
  # Output load predictions
  # prep for output to CSV
  outvars <- as.data.frame(predmatrix)
  colnames(outvars) <- oparmlist  
  
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
  }#if add_vars
  outvars2 <- outvars2[with(outvars2,order(outvars2$waterid)), ]  # sort by waterid
  
  fileout <- paste0(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predicts_load.csv")
  fwrite(outvars2,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  # Output the prediction variable names and units to CSV file
  lunitsOut <- data.frame(oparmlist,loadunits,oparmlistExpl)
  colnames(lunitsOut) <- c("Prediction Metric Name","Units","Metric Explanation")
  fileout <- paste0(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predicts_load_units.csv")
  fwrite(lunitsOut,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  # Output yield predictions
  # prep for output to CSV
  outvars <- as.data.frame(yldmatrix)
  colnames(outvars) <- oyieldlist  
  
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
  outvars2 <- outvars2[with(outvars2,order(outvars2$waterid)), ]    # sort by waterid
  
  fileout <- paste0(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predicts_yield.csv")
  fwrite(outvars2,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  # Output the prediction variable names and units to CSV file
  yunitsOut <- data.frame(oyieldlist,yieldunits,oyieldlistExpl)
  colnames(yunitsOut) <- c("Prediction Metric Name","Units","Metric Explanation")
  fileout <- paste0(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predicts_yield_units.csv")
  fwrite(yunitsOut,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  
}#end function


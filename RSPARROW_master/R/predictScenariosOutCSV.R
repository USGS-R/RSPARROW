#'@title predictScenariosOutCSV
#'@description Outputs the scenario load predictions to CSV files.  \\cr \\cr
#'Executed By: predictScenarios.R \\cr
#'Executes Routines: \\itemize\{\\item getVarList.R
#'             \\item named.list.R
#'             \\item unPackList.R\} \\cr
#'@param input top level interactive user input in Shiny app
#'@param Rshiny TRUE/FALSE indicating whether routine is being run from the Shiny app
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param estimate.list list output from `estimate.R`
#'@param predictScenarios.list an archive with key scenario control settings and the load and 
#'                             yield prediction variables that are output from the execution of 
#'                             a source-change scenario evaluation. For more details see 
#'                             documentation Section 5.5.9
#'@param subdata data.frame input data (subdata)
#'@param add_vars additional variables specified by the setting `add_vars` to be included in 
#'       prediction, yield, and residuals csv and shape files
#'@param scenario_name User specified name of source reduction scenario from sparrow_control.  
#'       Used to create directory for source reduction scenario results.
#'@param scenarioFlag binary vector indicating whether a reach is included in the source 
#'       reduction scenario
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param scenarioCoefficients user specified model coefficients for the scenario



predictScenariosOutCSV <- function(#Rshiny 
  input, Rshiny,
  #regular
  file.output.list,estimate.list,predictScenarios.list,subdata,add_vars,
  scenario_name,scenarioFlag,data_names,scenarioCoefficients) {
  
  #################################################
  
  
  if (Rshiny){
    scenario_name<-input$scenarioName
  }
  # define "space" for printing
  ch <- character(1)
  space <- data.frame(ch)
  row.names(space) <- ch
  colnames(space) <- c(" ")
  # create global variable from list names (JacobResults)
  # 'oEstimate' containing the estimated mean parameters for all non-constant and constant parameters
  # 'Parmnames' list of variable names 
  # create global variable from list names
  # transfer required variables to global environment from SUBDATA
  unPackList(lists = list(JacobResults = estimate.list$JacobResults,
                          datalstCheck =  as.character(c(getVarList()$varList,"waterid_for_RSPARROW_mapping")),
                          predictScenarios.list = predictScenarios.list,
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
          addSubdataVars<-data.frame(waterid = subdata[,which(names(subdata) %in% tempCol)])
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
  # Output load predictions with changes from load-reduction scenarios
  # prep for output to CSV
  outvars <- predmatrix
  
  
  
  rchname <- subdata$rchname
  rchname <- gsub(",", "", rchname)
  predatts <- data.frame(waterid,rchname,rchtype,headflag,termflag,demtarea,
                         demiarea,meanq,fnode,tnode,hydseq,frac,iftran,staid,scenarioFlag)
  
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
  outvars2 <- outvars2[with(outvars2,order(outvars2$waterid)), ]  # sort by waterid
  
  fileout <- paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,scenario_name,"_",run_id,"_predicts_load_scenario.csv")
  fwrite(outvars2,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,col.names=TRUE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")
  
  # Output the prediction variable names and units to CSV file
  lunitsOut <- data.frame(oparmlist,loadunits)
  colnames(lunitsOut) <- c("Prediction Metric Name","Units")
  fileout <- paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,scenario_name,"_",run_id,"_predicts_load_scenario_units.csv")
  fwrite(lunitsOut,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  
  # Output load prediction changes (percent) from load-reduction scenarios
  outvars <- predmatrix_chg
  
  rchname <- subdata$rchname
  rchname <- gsub(",", "", rchname)
  predatts <- data.frame(waterid,rchname,rchtype,headflag,termflag,demtarea,
                         demiarea,meanq,fnode,tnode,hydseq,frac,iftran,staid,scenarioFlag)
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
  
  fileout <- paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,scenario_name,"_",run_id,"_predicts_loadchg_scenario.csv")
  fwrite(outvars2,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,col.names=TRUE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")
  
  
  # Output yield predictions with changes from load-reduction scenarios
  # prep for output to CSV
  outvars <- yldmatrix
  
  
  
  rchname <- subdata$rchname
  rchname <- gsub(",", "", rchname)
  predatts <- data.frame(waterid,rchname,rchtype,headflag,termflag,demtarea,
                         demiarea,meanq,fnode,tnode,hydseq,frac,iftran,staid,scenarioFlag)
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
  outvars2 <- outvars2[with(outvars2,order(outvars2$waterid)), ]    # sort by waterid
  
  fileout <- paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,scenario_name,"_",run_id,"_predicts_yield_scenario.csv")
  fwrite(outvars2,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  # Output the prediction variable names and units to CSV file
  yunitsOut <- data.frame(oyieldlist,yieldunits)
  colnames(yunitsOut) <- c("Prediction Metric Name","Units")
  fileout <- paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,scenario_name,"_",run_id,"_predicts_yield_scenario_units.csv")
  fwrite(yunitsOut,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  # Output yield prediction changes (percent) from load-reduction scenarios
  outvars <- yldmatrix_chg
  
  
  
  rchname <- subdata$rchname
  rchname <- gsub(",", "", rchname)
  predatts <- data.frame(waterid,rchname,rchtype,headflag,termflag,demtarea,
                         demiarea,meanq,fnode,tnode,hydseq,frac,iftran,staid,scenarioFlag)
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
  outvars2 <- outvars2[with(outvars2,order(outvars2$waterid)), ]    # sort by waterid
  
  fileout <- paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,scenario_name,"_",run_id,"_predicts_yieldchg_scenario.csv")
  fwrite(outvars2,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  # Output meta data to text file documenting settings for scenario 
  fileout <- paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,scenario_name,"_",run_id,"_scenario_metainfo.txt")
  sink(file=fileout,split="FALSE",append=FALSE)
  if (!Rshiny){
    
    
    outvars2<-named.list(select_scenarioReachAreas,select_targetReachWatersheds,
                         scenario_sources,scenario_factors,landuseConversion)
    print(outvars2)
    
    
  }else{#Rshiny
    
    outvars2 <- data.frame(select_scenarioReachAreas=as.character(input$domain),
                           select_targetReachWatersheds = as.character(input$target))
    if (input$domain=="all reaches"){
      
      print(outvars2)
      print(space)
      print(input$`nsSourceRedALL-hot`)
      
    }else{
      if (input$allSrc=="yes"){
        
        print(outvars2)
        print(space)
        print(input$`nsSourceRed-hot`)
        print(space)
        print(input$`nsAllSources-hot`)
        
      }else{
        
        print(outvars2)
        print(space)
        print(input$`nsAllSourcesNO-hot`)
        
      }
    }
    
    
  }#end Rshiny sink
  print(space)
  print(scenarioCoefficients)
  sink(type = "message")
  sink()
  
  
  if (!Rshiny){
    #save the modifySubdata routine with a record of the source change settings
    filesList<-c("_modifySubdata.R")
    sapply(filesList, function(x) file.copy(paste0(path_results,run_id,x),
                                            paste0(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,scenario_name,"_",run_id,x)))
  }
  
  
  
  
}#end function


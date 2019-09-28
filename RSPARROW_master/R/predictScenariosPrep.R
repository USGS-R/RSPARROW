#'@title predictScenariosPrep
#'@description Sets up source reduction scenario applying change factors to specified areas \\cr \\cr
#'Executed By: predictScenarios.R \\cr
#'Executes Routines: \\itemize\{\\item hydseqTerm.R
#'             \\item named.list.R
#'             \\item unPackList.R\} \\cr
#'@param input top level interactive user input in Shiny app
#'@param allMetrics character string of all load, yield, uncertainty, and data dictionary 
#'       variables to map in shiny batch mode
#'@param output_map_type character string control setting to identify type of map(s) to output 
#'       to PDF file from "stream","catchment", or "both"
#'@param Rshiny TRUE/FALSE indicating whether routine is being run from the Shiny app
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param if_predict yes/no indicating whether or not prediction is run
#'@param data `DataMatrix.list$data` to be used in source reduction scenario setup. For more 
#'       details see documentation Section 5.2.3.1
#'@param srcvar `SelParmValues$srcvar` to be used in source reduction scenario setup. For more 
#'       details see documentation Section 5.2.3.2
#'@param jsrcvar `DataMatrix.list$data.index.list$jsrcvar`  to be used in source reduction 
#'       scenario setup. For more details see documentation Section 5.2.3.1
#'@param dataNames the names of the FIXED and REQUIRED system variables and explanatory 
#'       variables associated with the user-specified model.
#'@param JacobResults list output of Jacobian first-order partial derivatives of the model 
#'       residuals `estimateNLLSmetrics.R` contained in the estimate.list object.  For more details see 
#'       documentation Section 5.2.4.5.
#'@param subdata data.frame input data (subdata)
#'@return `scenarioPrep.list` list of source change scenario setup control settings, data with 
#'            change factors applied, and the scenarioFlag indicating the scenario mapping area



predictScenariosPrep<-function(##Rshiny
  input,allMetrics, output_map_type,Rshiny,
  ##regular
  scenario.input.list,
  data_names,
  if_predict,
  #data
  data,
  srcvar,jsrcvar,dataNames,JacobResults,
  subdata,
  #paths
  file.output.list){
  
  scenarioError<-FALSE
  unPackList(lists = list(file.output.list = file.output.list,
                          scenario.input.list = scenario.input.list),
             parentObj = list(NA,NA)) 
  
  #create scenario_name directory
  options(warn=-1)
  if (Rshiny==FALSE){
    if (!dir.exists(paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,sep=""))){
      dir.create(paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,sep=""))
    }#if directory not found
  }else{#Rshiny TRUE
    if (!dir.exists(paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,sep=""))){
      dir.create(paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,sep=""),showWarnings = FALSE)
    }#if direcotry not found
  }#if Rshiny TRUE
  options(warn=0)  
  
  
  # transfer required variables to global environment from SUBDATA 
  unPackList(lists = list(datalstCheck = data_names$sparrowNames),
             parentObj = list(subdata = subdata))
  
  #get coefficient estimates
  betalst <- JacobResults$oEstimate
  beta1<-t(matrix(betalst, ncol=nrow(subdata), nrow=length(betalst)))
  
  scenarioCoefficients<-data.frame(PARAMETER = JacobResults$Parmnames,
                                   ESTIMATE = JacobResults$oEstimate,
                                   PercentChange = rep(0,length(JacobResults$oEstimate)))
  
  #convert Rshiny metrics to control setting names 
  if (Rshiny==TRUE){
    scenario_name<-as.character(input$scenarioName)
    scenario_sources<-as.character(input$scenario_sources)
    select_targetReachWatersheds<-as.character(input$target)
    select_scenarioReachAreas<-as.character(input$domain)
    scenario_factors<-input$scenario_factors
    ChangeCoef<-as.character(input$ChangeCoef)
    sourceCoef<-as.character(input$sourceCoef)
    
    #clear any (source)_LC variables from subdata
    for (s in scenario_sources){
      suppressWarnings(rm(list = c(paste0("S_",s,"_LC"))))
      suppressWarnings(rm(list = c(paste0("S_",s,"_LC")),envir = .GlobalEnv))
      suppressWarnings(rm(list = c(paste0("S_",s))))
      suppressWarnings(rm(list = c(paste0("S_",s)),envir = .GlobalEnv))
      
    }
    
    
    
    if (select_scenarioReachAreas=="selected reaches"){
      #apply source reduction factors to create 'scenario_factor' with selected reach application of factors
      for (f in 1:length(input$selectFuncs)){
        eval(parse(text = input$selectFuncs[f]))
        if (input$allSrc=="no"){
          eval(parse(text = input$lcFuncs[f]))
        }
      }
    }
    if (select_scenarioReachAreas=="all reaches"| input$allSrc=="yes"){#if all reaches
      landuseConversion<-as.character(input$landuseConversion)
      landuseConversion<-ifelse(landuseConversion=="",NA,landuseConversion)
      landuseConversion<-ifelse(landuseConversion=="None",NA,landuseConversion)
      
    }else{#selected reaches
      landuseConversion<-NA
    }#if all reaches
    
    if (tolower(select_targetReachWatersheds)=="default" | select_targetReachWatersheds==""){
      select_targetReachWatersheds<-NA
    }else if (tolower(select_targetReachWatersheds)=="import"){
      #read flag file
      filein <- paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,"flag_TargetReachWatersheds.csv",sep="")
      select_targetReachWatersheds <- fread(filein,header=TRUE,stringsAsFactors=FALSE,
                                            dec = csv_decimalSeparator,sep=csv_columnSeparator)
      
      #save flag file to subdirectory
      fileout <- paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,scenario_name,"_",run_id,"_flag_TargetReachWatersheds.csv",sep="")
      fwrite(select_targetReachWatersheds,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,col.names=TRUE,
             dec = csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")      
      
      #extract only flagged waterids
      select_targetReachWatersheds <-select_targetReachWatersheds[which(select_targetReachWatersheds$flag==1),]$waterid
      if (length(select_targetReachWatersheds)==0){
        message(paste0("WARNING : No target reach watershed flagged in ",filein,".  \nEdit ~/scenarios/flag_TargetReachWatersheds.csv."))
        #open file for edit
        shell(filein, wait = TRUE, invisible=FALSE)
        #get waterids
        select_targetReachWatersheds <- fread(filein,header=TRUE,stringsAsFactors=FALSE,
                                              dec = csv_decimalSeparator,sep=csv_columnSeparator)
        
        
        #save flag file to subdirectory
        fileout <- paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,scenario_name,"_",run_id,"_flag_TargetReachWatersheds.csv",sep="")
        fwrite(select_targetReachWatersheds,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,col.names=TRUE,
               dec = csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")
        
        #extract only flagged waterids
        select_targetReachWatersheds <-select_targetReachWatersheds[which(select_targetReachWatersheds$flag==1),]$waterid
        
        if (length(select_targetReachWatersheds)==0){
          message(paste0("WARNING : No target reach watershed flagged in ",filein,".  \nScenario run with all reaches"))
          select_targetReachWatersheds<-NA
        }
      }
    }else{
      select_targetReachWatersheds<-eval(parse(text=paste0("c(",select_targetReachWatersheds,")")))
    }
  }else{#not Rshiny
    ChangeCoef<-rep("no",length(scenario_sources))
    sourceCoef<-NA
  }
  
  
  
  ###target reach selection
  if (!is.na(select_targetReachWatersheds[1])){
    
    subdata_target<-hydseqTerm(subdata, select_targetReachWatersheds)
    subdata_target<-subdata_target[order(match(subdata_target$waterid,subdata$waterid)),]
    reachFlag<-ifelse(!is.na(subdata_target$hydseq),1,0)
    
  }else{#original terminal reaches
    reachFlag<-rep(1,nrow(subdata))
  } 
  
  
  #setup S_(source) and S_(source)_LU variables 
  for (i in 1:(length(scenario_sources))){
    if (select_scenarioReachAreas=="all reaches"){
      if (ChangeCoef[i]=="no"){
        eval(parse(text = paste0("S_",scenario_sources[i],"<- rep(",scenario_factors[i],",nrow(subdata))")))
        
        if (!is.na(landuseConversion[i])){
          eval(parse(text = paste0("S_",scenario_sources[i],"_LC <- rep('",landuseConversion[i],"',nrow(subdata))")))
        }else{
          suppressWarnings(rm(list = c(paste0("S_",scenario_sources[i],"_LC"))))
        }
      }
    }else{#selected reaches
      
      if (!is.na(landuseConversion[i]) & !exists(paste0("S_",scenario_sources[i],"_LC"))){
        if (Rshiny==TRUE){
          eval(parse(text = paste0("S_",scenario_sources[i],"_LC <- ifelse(S_",scenario_sources[i],"!=1,'",landuseConversion[i],"',NA)"))) 
        }
      }else if (Rshiny==FALSE & exists(paste0("S_",scenario_sources[i],"_LC"))){
        temp<-eval(parse(text =paste0("S_",scenario_sources[i],"_LC") ))
        
        if (length(unique(temp))==1 & is.na(unique(temp))){
          
          suppressWarnings(rm(list = c(paste0("S_",scenario_sources[i],"_LC"))))
        }
      }
      
    }#selected reaches
  }#fore each source
  
  
  #create scenarioAreaFlag
  scenarioAreaFlag<-rep(0,nrow(data))
  for (i in 1:(length(scenario_sources))){
    if (exists(paste0("S_",scenario_sources[i]))){
      temp<-eval(parse(text = paste0("S_",scenario_sources[i])))
      scenarioAreaFlag<-ifelse(!is.na(temp),1,scenarioAreaFlag)
    }else if (select_scenarioReachAreas=="all reaches"){
      scenarioAreaFlag<-rep(1,nrow(subdata))
    }
  }
  #combine with reachFlag
  scenarioFlag<-ifelse(reachFlag==1 & scenarioAreaFlag==1,1,0)
  
  #create sumarea matrix
  sumarea<-matrix(0,ncol=length(srcvar),nrow=nrow(data))
  #create reduction matrix
  reducMat<-matrix(0,ncol=length(srcvar),nrow=nrow(data))
  #create error matrix
  errorMat<-matrix(0,ncol=length(srcvar),nrow=nrow(data))
  #create empty vector for invalid LUs
  LUsourceError<-character(0)
  
  
  #loop through scenario_sources
  for (i in 1:length(scenario_sources)){
    if (exists(paste0("S_",scenario_sources[i]))){
      #get source reductions
      temp<-eval(parse(text = paste0("S_",scenario_sources[i])))
      temp<-ifelse(is.na(temp),1,temp)
      assign(paste0("S_",scenario_sources[i]),temp)
      
      #get lU conversion
      if (exists(paste0("S_",scenario_sources[i],"_LC"))){
        temp_LC<-eval(parse(text = paste0("S_",scenario_sources[i],"_LC")))
        
        #check for valid source types
        LUsourceCheck<-as.character(na.omit(unique(temp_LC)))
        LUsourceCheck<-LUsourceCheck[which(!LUsourceCheck %in% srcvar)]
        LUsourceError<-c(LUsourceError,LUsourceCheck)
        
        if (length(LUsourceCheck)==0){
          
          #populate reducMat
          errorMat[,which(srcvar==scenario_sources[i])]<-ifelse(scenarioFlag==1,ifelse(temp!=1,temp,0),errorMat) 
          
          
          #loop through srcvar
          for (j in 1:length(srcvar)){
            reducMat[which(srcvar[j]==temp_LC),which(srcvar==scenario_sources[i])]<-ifelse(temp[which(srcvar[j]==temp_LC)] < 1 & scenarioFlag[which(srcvar[j]==temp_LC)]==1,
                                                                                           (reducMat[which(srcvar[j]==temp_LC),j]+
                                                                                              data[which(srcvar[j]==temp_LC),jsrcvar[which(srcvar==scenario_sources[i])]] * 
                                                                                              temp[which(srcvar[j]==temp_LC)]),
                                                                                           
                                                                                           ifelse(scenarioFlag[which(srcvar[j]==temp_LC)]==1,  
                                                                                                  (reducMat[which(srcvar[j]==temp_LC),j]+
                                                                                                     data[which(srcvar[j]==temp_LC),jsrcvar[which(srcvar==scenario_sources[i])]] *
                                                                                                     (1- temp[which(srcvar[j]==temp_LC)])),
                                                                                                  0))
            
            #populate sumarea matrix
            sumarea[which(srcvar[j]==temp_LC), j] <- ifelse(temp[which(srcvar[j]==temp_LC)] < 1 & scenarioFlag[which(srcvar[j]==temp_LC)]==1,
                                                            
                                                            (sumarea[which(srcvar[j]==temp_LC),j]+
                                                               data[which(srcvar[j]==temp_LC),jsrcvar[which(srcvar==scenario_sources[i])]] * 
                                                               temp[which(srcvar[j]==temp_LC)]),
                                                            
                                                            ifelse(scenarioFlag[which(srcvar[j]==temp_LC)]==1,
                                                                   (sumarea[which(srcvar[j]==temp_LC),j]+
                                                                      data[which(srcvar[j]==temp_LC),jsrcvar[which(srcvar==scenario_sources[i])]] *
                                                                      (1- temp[which(srcvar[j]==temp_LC)])),
                                                                   0)) 
            
            
            
          }#for j
        }#if LUsourceCheck
      }else{#if not LU conversion
        reducMat[,which(srcvar==scenario_sources[i])]<-ifelse(temp < 1 & scenarioFlag==1,
                                                              
                                                              (reducMat[,which(srcvar==scenario_sources[i])]+
                                                                 data[,jsrcvar[which(srcvar==scenario_sources[i])]] * 
                                                                 temp),
                                                              ifelse(scenarioFlag==1,
                                                                     (reducMat[,which(srcvar==scenario_sources[i])]+
                                                                        data[,jsrcvar[which(srcvar==scenario_sources[i])]] *
                                                                        (1- temp)),0))
        
      }##else      
    }#end not coefficient  
  }#for i
  
  #coefficient change
  coefSource<-sourceCoef[which(ChangeCoef=="yes")]
  coefFactor<-scenario_factors[which(ChangeCoef=="yes")]
  if (length(names(input)[which(names(input)=="cfFuncs")])!=0){
    for (f in input$cfFuncs){
      eval(parse(text = f))
    }
  }
  for (c in 1:length(betalst)){
    if (JacobResults$Parmnames[c] %in% coefSource){
      if (!exists(paste0("S_",JacobResults$Parmnames[c],"_CF"))){
        if (coefFactor[which(coefSource==JacobResults$Parmnames[c])]<1){
          betalst[c]<-betalst[c]-betalst[c]*coefFactor[which(coefSource==JacobResults$Parmnames[c])] 
          beta1[,c]<-ifelse(scenarioFlag==1,betalst[c],beta1[,c])
          scenarioCoefficients<-rbind(scenarioCoefficients,
                                      data.frame(PARAMETER = JacobResults$Parmnames[c],
                                                 ESTIMATE = betalst[c],
                                                 PercentChange = coefFactor[which(coefSource==JacobResults$Parmnames[c])]*-100))
          
        }else{
          betalst[c]<-betalst[c]*coefFactor[which(coefSource==JacobResults$Parmnames[c])] 
          beta1[,c]<-ifelse(scenarioFlag==1,betalst[c],beta1[,c])
          scenarioCoefficients<-rbind(scenarioCoefficients,
                                      data.frame(PARAMETER = JacobResults$Parmnames[c],
                                                 ESTIMATE = betalst[c],
                                                 PercentChange = coefFactor[which(coefSource==JacobResults$Parmnames[c])]*100-100))
        }
      }else{#apply S_source_CF function
        temp<-eval(parse(text = paste0("S_",JacobResults$Parmnames[c],"_CF")))
        message("line331")
        if (coefFactor[which(coefSource==JacobResults$Parmnames[c])]<1){
          betalst[c]<-betalst[c]-betalst[c]*coefFactor[which(coefSource==JacobResults$Parmnames[c])] 
          beta1[,c]<-ifelse(!is.na(temp) & reachFlag==1,betalst[c],beta1[,c])
          scenarioCoefficients<-rbind(scenarioCoefficients,
                                      data.frame(PARAMETER = JacobResults$Parmnames[c],
                                                 ESTIMATE = betalst[c],
                                                 PercentChange = coefFactor[which(coefSource==JacobResults$Parmnames[c])]*-100))
        }else{
          betalst[c]<-betalst[c]*coefFactor[which(coefSource==JacobResults$Parmnames[c])] 
          beta1[,c]<-ifelse(!is.na(temp) & reachFlag==1,betalst[c],beta1[,c])
          scenarioCoefficients<-rbind(scenarioCoefficients,
                                      data.frame(PARAMETER = JacobResults$Parmnames[c],
                                                 ESTIMATE = betalst[c],
                                                 PercentChange = coefFactor[which(coefSource==JacobResults$Parmnames[c])]*100-100))
        }
      }
    }
  }
  
  
  
  
  #test for snowball
  #if sumarea!=0 where reduction occurs
  errorLU<-any(errorMat[which(sumarea!=0)]!=0)
  
  if (errorLU==FALSE){
    #apply landuse conversions
    testApply<-data
    testApply[,jsrcvar]<-testApply[,jsrcvar]+sumarea
    
    #test for negative land use data
    negTest<-testApply[,jsrcvar]
    negTest<-which(sumarea!=0  & negTest<0 ,arr.ind = TRUE)
    negTest<-ifelse(nrow(negTest)!=0,TRUE,FALSE)
    
  }
  
  if (errorLU==FALSE & length(LUsourceError)==0 & negTest==FALSE){#apply reductions to data
    data[,jsrcvar]<-data[,jsrcvar]+sumarea
    data[,jsrcvar]<-data[,jsrcvar]-reducMat  
    
  }
  
  if (negTest==TRUE){#negative landuse data
    negTest<-testApply[,jsrcvar]
    negTest<-which(sumarea!=0 & negTest<0,arr.ind = TRUE)
    
    data[-negTest[,1],jsrcvar]<-data[-negTest[,1],jsrcvar]+sumarea[-negTest[,1],]
    data[-negTest[,1],jsrcvar]<-data[-negTest[,1],jsrcvar]-reducMat[-negTest[,1],]
    
    negTest<-data[negTest[,1],]
    
    colnames(negTest)<-DataMatrix.list$dataNames
    negTest<-subdata[which(subdata$waterid %in% negTest[,1]),]
    fileout <- paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,scenario_name,"_",run_id,"_NegativeLanduseFound.csv",sep="")
    fwrite(negTest,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,col.names=TRUE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")
    message("\n \nWARNING : Invalid Landuse Conversion.  Negative landuse data found for ",nrow(negTest)," waterids, saved to \n",fileout)
    
    
  }else if (any(sumarea!=0) & errorLU==TRUE){#compound reductions
    message("\n \nWARNING : Invalid Landuse Conversion.  Compound reductions found.\nNO SCENARIO EXECUTED")
    scenarioError<-TRUE
  }else if (length(LUsourceError)!=0){
    message(paste0("\n \nWARNING : The following selections in landuseConversion are NOT SOURCE variables:\n",
                   LUsourceError,"\nlanduseConversion NOT applied to ",scenario_name))
    scenarioError<-TRUE
  }
  
  
  #outputlist
  scenarioPrep.list<-named.list(data,scenario_name,scenario_sources,
                                select_scenarioReachAreas,select_targetReachWatersheds,
                                scenario_factors,landuseConversion,
                                scenarioFlag,beta1,scenarioCoefficients,scenarioError)
  
  
  
  
  
  return(scenarioPrep.list) 
  
}#end function

#'@title modelCompare
#'@description function to compare groups of models to the current model using the output from 
#'            'estimate/summaryCSV' directory \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'Executes Routines: unPackList.R \\cr
#'@param file.output.list list of control settings and relative paths used for input and 
#'                        output of external files.  Created by `generateInputList.R`
#'@param compare_models character string control setting indicated the run_ids of preivously 
#'       run model to which the current model is to be compared
#'@param modelComparison_name character string control setting that gives the name of the 
#'       model comparision being executed
#'@param if_spatialAutoCorr yes/no control setting to specifiy if the spatial autocorrelation 
#'       diagnostic graphics for Moran's I test are to be output



modelCompare<-function(file.output.list,compare_models,modelComparison_name,
                       if_spatialAutoCorr){
  
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  # define title output function
  outcharfun<-function(char) {
    outchar <- data.frame(char)
    row.names(outchar ) <- c(" ")
    colnames(outchar ) <- c(" ")
    return(outchar)
  }
  # define "space" for printing
  ch <- character(1)
  space <- data.frame(ch)
  row.names(space) <- ch
  colnames(space) <- c(" ")
  
  #compare results of previous models
  if (length(na.omit(compare_models))!=0){
    message("Running Model Comparison...")
    #get current model data
    path_summary<-paste0(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"summaryCSV",.Platform$file.sep)
    modelPerformance<-read.csv(file=paste0(path_summary,"ModelPerformanceMonitoringAdj.csv"),
                               dec = csv_decimalSeparator,sep=csv_columnSeparator)
    ch <- " "
    row.names(modelPerformance) <- ch
    eigenSpread<-read.csv(file=paste0(path_summary,"EigenValueSpread.csv"),
                          dec = csv_decimalSeparator,sep=csv_columnSeparator)
    row.names(eigenSpread) <- ch
    
    paramEst<-read.csv(file=paste0(path_summary,"ParameterEstimates.csv"),
                       dec = csv_decimalSeparator,sep=csv_columnSeparator)
    
    if (if_spatialAutoCorr=="yes"){
      moran<-read.csv(file=paste0(path_summary,"EuclideanMoransI.csv"),
                      dec = csv_decimalSeparator,sep=csv_columnSeparator)
    }
    
    #create directory for comparison
    options(warn = -1)
    dir.create(paste0(dirname(path_results),.Platform$file.sep,modelComparison_name,.Platform$file.sep))
    options(warn=0)
    sinkFile<-paste0(dirname(path_results),.Platform$file.sep,modelComparison_name,.Platform$file.sep,modelComparison_name,"_summary.txt")
    sink(file=sinkFile,split="FALSE",append=FALSE)
    
    #for sink to txt file
    print(outcharfun("SPARROW NLLS MODEL COMPARISION SUMMARY"))
    print(outcharfun(paste0("MODEL COMPARISION NAME: ",modelComparison_name)))
    print(space)
    print(space)
    print(outcharfun("SPARROW NLLS MODEL SUMMARY"))
    print(outcharfun(paste0("MODEL NAME: ",run_id)))
    print(outcharfun("MODEL PERFORMANCE (Monitoring adjustment)"))
    print(modelPerformance)
    print(space)
    
    print(space)
    print(outcharfun("PARAMETER ESTIMATES"))
    print(paramEst)
    print(space)
    print(outcharfun("EigenValue Spread"))
    print(eigenSpread)
    print(space)
    if (if_spatialAutoCorr=="yes"){
      print(outcharfun("Euclidean Moran's I"))
      print(moran)
      print(space)
    }
    
    #add rownames
    ch<-1
    row.names(modelPerformance) <- ch
    row.names(eigenSpread) <- ch
    
    #add model name
    modelPerformance$modelName<-run_id
    eigenSpread$modelName<-run_id
    paramEst$modelName<-run_id
    if (if_spatialAutoCorr=="yes"){
      moran$modelName<-run_id
    }
    
    for (m in compare_models){
      path_summary<-paste0(dirname(path_results),.Platform$file.sep,m,.Platform$file.sep,"estimate",.Platform$file.sep,"summaryCSV",.Platform$file.sep)
      if (dir.exists(path_summary)){
        #get model data
        cmodelPerformance<-read.csv(file=paste0(path_summary,"ModelPerformanceMonitoringAdj.csv"),
                                    dec = csv_decimalSeparator,sep=csv_columnSeparator)
        ch <- " "
        row.names(cmodelPerformance) <- ch
        ceigenSpread<-read.csv(file=paste0(path_summary,"EigenValueSpread.csv"),
                               dec = csv_decimalSeparator,sep=csv_columnSeparator)
        row.names(ceigenSpread) <- ch
        cifdiag<-read.csv(file=paste0(dirname(path_results),.Platform$file.sep,m,.Platform$file.sep,m,"_userSettings.csv"),
                          dec = csv_decimalSeparator,sep=csv_columnSeparator)
        cifdiag<-as.character(cifdiag[which(cifdiag$setting=="if_spatialAutoCorr"),]$value)
        cifdiag<-gsub("\"","",cifdiag)
        cparamEst<-read.csv(file=paste0(path_summary,"ParameterEstimates.csv"),
                            dec = csv_decimalSeparator,sep=csv_columnSeparator)
        
        if (cifdiag=="yes"){
          cmoran<-read.csv(file=paste0(path_summary,"EuclideanMoransI.csv"),
                           dec = csv_decimalSeparator,sep=csv_columnSeparator)
        }
        
        #sink to txt file
        print(outcharfun("SPARROW NLLS MODEL SUMMARY"))
        print(outcharfun(paste0("MODEL NAME: ",m)))
        print(outcharfun("MODEL PERFORMANCE (Monitoring adjustment)"))
        print(cmodelPerformance)
        print(space)
        
        print(space)
        print(outcharfun("PARAMETER ESTIMATES"))
        print(cparamEst)
        print(space)        
        print(outcharfun("EigenValue Spread"))
        print(ceigenSpread)
        print(space)
        if (cifdiag=="yes"){
          print(outcharfun("Euclidean Moran's I"))
          print(cmoran)
          print(space)
        }
        #add rownames
        ch<-1
        row.names(cmodelPerformance) <- ch
        row.names(ceigenSpread) <- ch
        
        #add model name
        cmodelPerformance$modelName<-m
        ceigenSpread$modelName<-m
        cparamEst$modelName<-m
        if (cifdiag=="yes"){
          cmoran$modelName<-m
        }
        
        #compile for csv files
        modelPerformance<-rbind.fill(modelPerformance,cmodelPerformance)
        eigenSpread<-rbind.fill(eigenSpread,ceigenSpread)
        paramEst<-rbind.fill(paramEst,cparamEst)
        if (if_spatialAutoCorr=="yes"){
          if (cifdiag=="yes"){
            moran<-rbind.fill(moran,cmoran)
          }
        }
        
        
      }else{
        message(paste0(path_summary," DOES NOT EXIST.  MODEL COMPARISON NOT RUN."))
      }
      
    }#for each model to compare
    
    sink()
    #output csv files
    dirOUT<-dirname(sinkFile)
    fwrite(file=paste0(dirOUT,.Platform$file.sep,modelComparison_name,"_ModelPerformanceMonitoringAdj.csv"),modelPerformance,row.names=FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    fwrite(file=paste0(dirOUT,.Platform$file.sep,modelComparison_name,"_EigenValueSpread.csv"),eigenSpread,row.names=FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    fwrite(file=paste0(dirOUT,.Platform$file.sep,modelComparison_name,"_ParameterEstimates.csv"),paramEst,row.names=FALSE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    if (if_spatialAutoCorr=="yes"){
      fwrite(file=paste0(dirOUT,.Platform$file.sep,modelComparison_name,"_EuclideanMoransI.csv"),moran,row.names=FALSE,
             dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
    }
  }#if models to compare
  
  
  
}#end function

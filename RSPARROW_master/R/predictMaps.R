#'@title predictMaps
#'@description executes prediction and source reduction scenario stream and catchment mapping \\cr \\cr
#'Executed By: \\itemize\{\\item batchMaps.R
#'             \\item interactiveBatchRun.R
#'             \\item goShinyPlot.R
#'             \\item predictScenarios.R\} \\cr
#'Executes Routines: \\itemize\{\\item checkBinaryMaps.R
#'             \\item mapBreaks.R
#'             \\item unPackList.R\} \\cr
#'@param input top level interactive user input in Shiny app
#'@param allMetrics character string of all load, yield, uncertainty, and data dictionary 
#'       variables to map in shiny batch mode
#'@param output_map_type character string control setting to identify type of map(s) to output 
#'       to PDF file from "stream","catchment", or "both"
#'@param Rshiny TRUE/FALSE indicating whether routine is being run from the Shiny app
#'@param BootUncertainties Uncertainty values if available, if uncertainty analysis was not 
#'       run NA
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param subdata data.frame input data (subdata)
#'@param mapScenarios TRUE/FALSE indicating whether source change scenario mapping is being run
#'@param scenario_map_list character vector of load and yield metrics to map in the source 
#'       change scenario
#'@param scenarioFlag binary vector indicating whether a reach is included in the source 
#'       reduction scenario
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



predictMaps<-function(#Rshiny
  input,allMetrics,output_map_type, Rshiny, 
  #regular
  file.output.list,
  #map_uncertainties,BootUncertainties,
  data_names,mapping.input.list,
  #predict.list,
  subdata,
  #scenarios
  mapScenarios,
  scenario_map_list,
  predictScenarios.list,
  scenarioFlag,
  batch_mode) {
  
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  # obtain uncertainties, if available
  objfile <- paste(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_BootUncertainties",sep="")
  if(file.exists(objfile) == TRUE) {
    load(objfile)
    map_uncertainties <- c("se_pload_total","ci_pload_total")
  } else {
    map_uncertainties <- NA 
    BootUncertainties <- NA
  }
  
  testList<-character(0)
  if (mapScenarios==FALSE){
    #test if mapped variable not prediction or scenario
    if (Rshiny==TRUE){ #shiny
      if (input$batch=="Interactive"){
        master_map_list<-c(trimws(gsub("-","",input$var)))
        
      }else{
        master_map_list<-c(trimws(gsub("-","",allMetrics)))
      } 
    }else{#not shiny
      master_map_list<-mapping.input.list$master_map_list
    }
    testList<-names(subdata)[which(names(subdata) %in% master_map_list)]
    noMaplist<-character(0)
    #only try to map variables with same length as subdata
    if (length(testList)!=0){
      for (l in testList){
        testvar<-subdata[,which(names(subdata)==l)]
        testvar<-na.omit(testvar)
        if (length(testvar)!=nrow(subdata)){
          lengthNA<-nrow(subdata)-length(testvar)
          if (lengthNA==nrow(subdata)){#all missing
            testList<-testList[which(testList!=l)]
            noMaplist<-c(noMaplist,l)
            message(paste0("\n \nWARNING : ALL MISSING VALUES FOUND IN ", l, " MAPPING NOT COMPLETED."))
            if (batch_mode=="yes"){
              cat(' \nWARNING : ALL MISSING VALUES FOUND IN ', l, ' MAPPING NOT COMPLETED.',sep='')
            }
          }
          
          
          message(paste0("\n \nWARNING : MISSING VALUES FOUND IN ", l, " MAPPING MAY NOT BE COMPLETE."))
          message(paste0(lengthNA," MISSING VALUES FOUND AND REPLACED WITH ZEROS \n \n"))
          if (batch_mode=="yes"){
            cat(' \nWARNING : MISSING VALUES FOUND IN ', l, ' MAPPING MAY NOT BE COMPLETE.',sep='')
            cat(lengthNA," MISSING VALUES FOUND AND REPLACED WITH ZEROS\n ",sep="")
          }
        }#remove variable from mapping list
        
        #test numeric
        testvar<-subdata[,which(names(subdata)==l)]
        testNum<-class(testvar)
        if (testNum!="numeric"){
          testList<-testList[which(testList!=l)]
          noMaplist<-c(noMaplist,l)
          message(paste0("\n \nWARNING : MAPPING VARIABLE ", l, " NOT NUMERIC MAPPING NOT COMPLETED."))
          if (batch_mode=="yes"){
            cat(' \nWARNING : MAPPING VARIABLE ', l, ' NOT NUMERIC MAPPING NOT COMPLETED.',sep='')
          }
        }
        
      }#for each non prediction variable
    }#if non prediction variables exist
    
  }#if map scenarios FALSE
  
  if ((file.exists(paste(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list",sep="")) & mapScenarios==FALSE) | 
      mapScenarios==TRUE |
      (length(testList)>0 & mapScenarios==FALSE)){
    if (mapScenarios==FALSE & file.exists(paste(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list",sep=""))){
      if (!exists("predict.list")){
        load(paste(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list",sep=""))
      }
    }
    
    
    # Setup variable lists 
    # create global variable from list names (mapping.input.list)
    output_map_typeArg<-output_map_type
    unPackList(lists = list(mapping.input.list = mapping.input.list),
               parentObj = list(NA))
    output_map_type<-output_map_typeArg
    
    if (mapScenarios==FALSE & exists("predict.list")){
      # create global variable from list names (predict.list)
      unPackList(lists = list(predict.list = predict.list),
                 parentObj = list(NA))
      
    }else if (mapScenarios==TRUE){
      # create global variable from list names (predict.list)
      unPackList(lists = list(predictScenarios.list = predictScenarios.list),
                 parentObj = list(NA))
      
    }else if (!exists("predict.list")){
      master_map_list<-master_map_list[which(master_map_list %in% names(subdata))]
    }
    
    # required names
    datalstreq <- data_names$sparrowNames
    datalstunits <- data_names$varunits
    
    # transfer required variables to global environment from SUBDATA
    unPackList(lists = list(datalstreq = datalstreq),
               parentObj = list(subdata = subdata))
    
    if (Rshiny==TRUE){ #shiny
      if (input$batch=="Interactive"){
        master_map_list<-c(trimws(gsub("-","",input$var)))
        
      }else{
        master_map_list<-c(trimws(gsub("-","",allMetrics)))
        
      }
      
      # get cosmetic mapping variables
      predictionTitleSize<-as.numeric(input$predictionTitleSize)
      predictionLegendSize<-as.numeric(input$predictionLegendSize)
      predictionLegendBackground<-gsub("\"","",gsub("'","",input$predictionLegendBackground))  
      predictionClassRounding<-as.numeric(input$predictionClassRounding)
      predictionMapBackground<-gsub("\"","",gsub("'","",input$predictionMapBackground))
      lineWidth<-as.numeric(input$lineWidth)
      if (mapScenarios==FALSE){
        predictionMapColors<-eval(parse(text=input$predictionMapColors))
      }else{
        scenarioMapColors<-eval(parse(text=input$scenarioMapColors))
      }
    }
    
    if (mapScenarios==FALSE){
      mapgo.list <- numeric(length(master_map_list))
      mapunits.list <- character(length(master_map_list))
      nintervals <- numeric(length(master_map_list))
      intervals <- matrix(0,nrow=length(master_map_list),ncol=length(predictionMapColors)+1)
    }else{
      if (Rshiny==FALSE){
        master_map_list<-scenario_map_list
        
      }
    }#scenario
    if (Rshiny==TRUE){#Rshiny
      
      if (input$batch=="Batch"){
        master_map_list<-allMetrics
        if (mapScenarios==TRUE){
          scenario_map_list<-allMetrics
        }
        output_map_type<-tolower(as.character(input$outCheck))
        
        if ((input$mapType=="Stream" | (mapScenarios==TRUE & regexpr("stream",paste(output_map_type,collapse=","))>0)) & input$shapeFile=="yes"){
          outputERSImaps[1]<-"yes"
        }
        if ((input$mapType=="Catchment" | (mapScenarios==TRUE & regexpr("catchment",paste(output_map_type,collapse=","))>0)) & input$shapeFile=="yes"){
          outputERSImaps[2]<-"yes"
        }
      }else{
        output_map_type<-tolower(as.character(input$outType))
        scenario_map_list<-c(trimws(gsub("-","",input$var)))
        #scenario_map_list<-"pload_total"
        master_map_list<-scenario_map_list
        
        
      }
    }#ewnd Rshiny
    if (mapScenarios==TRUE){
      mapgo.list <- numeric(length(scenario_map_list))
      mapunits.list <- character(length(scenario_map_list))
      nintervals <- numeric(length(scenario_map_list))
      intervals <- matrix(0,nrow=length(scenario_map_list),ncol=length(scenarioMapColors))
    }
    
    #get geoLines
    existGeoLines<-checkBinaryMaps(LineShapeGeo,path_gis,batch_mode)
    if (existGeoLines==TRUE){
      load(paste(path_gis,.Platform$file.sep,"GeoLines",sep=""))
    }
    
    existlineShape<-FALSE
    if ((paste(output_map_type,collapse="") %in% c("stream","both") & Rshiny==FALSE) | 
        (Rshiny==TRUE & input$mapType=="Stream" & mapScenarios==FALSE) | 
        (Rshiny==TRUE & regexpr("stream",paste(output_map_type,collapse=","))>0 & mapScenarios==TRUE)){
      #get lineShape
      existlineShape<-checkBinaryMaps(lineShapeName,path_gis,batch_mode)
      if (existlineShape==TRUE){
        load(paste(path_gis,.Platform$file.sep,"lineShape",sep=""))
      }
      
    }
    existpolyShape<-FALSE
    if ((paste(output_map_type,collapse="") %in% c("catchment","both") & Rshiny==FALSE) | 
        (Rshiny==TRUE & input$mapType=="Catchment" & mapScenarios==FALSE) |
        (Rshiny==TRUE & regexpr("catchment",paste(output_map_type,collapse=","))>0  & mapScenarios==TRUE)){
      #get polyShape
      existpolyShape<-checkBinaryMaps(polyShapeName,path_gis,batch_mode)
      if (existpolyShape==TRUE){
        load(paste(path_gis,.Platform$file.sep,"polyShape",sep=""))
      }
    }
    
    
    commonvar<-"tempID"
    
    #---------------------------------------------------------------#
    # Loop through variable list
    
    
    MAPID <- eval(parse(text=paste("subdata$","waterid_for_RSPARROW_mapping",sep="") ))   # added 3-25-2017
    dmapfinal <- data.frame(MAPID)                                   # added 3-25-2017
    colnames(dmapfinal) <- c(commonvar)
    break1<-list()
    testNA<-list()
    
    
    
    # remove bad variables from master_map_list
    
    if (mapScenarios==FALSE){
      master_map_list<-master_map_list[which(!master_map_list %in% noMaplist)]
      testmaster<-character(0)
      if (exists("oparmlist")){testmaster<-c(testmaster,master_map_list[which(master_map_list %in% oparmlist)])}
      if (exists("oyieldlist")){testmaster<-c(testmaster,master_map_list[which(master_map_list %in% oyieldlist)])}
      if (mapScenarios==FALSE & !is.na(map_uncertainties[1])){testmaster<-c(testmaster,master_map_list[which(master_map_list %in% map_uncertainties)])}
      testmaster<-c(testmaster,master_map_list[which(master_map_list %in% datalstreq)])
      testmaster<-master_map_list
    }else{
      testmaster<-scenario_map_list
    }
    
    if (length(testmaster)!=0){
      for (k in 1:length(master_map_list)) {
        
        # Load matrix
        icolumn<-0
        if (exists("oparmlist")){
          for(i in 1:length(oparmlist)) {
            if (regexpr("ratio_",master_map_list[k])<0 | mapScenarios==FALSE){
              if(oparmlist[i] == master_map_list[k]) {
                icolumn <- i
                
              }
            }else{#ratio
              ratioMetric<-ifelse(master_map_list[k]=="ratio_total","pload_total","pload_inc")
              if(oparmlist[i] == ratioMetric) {
                icolumn <- i
              }
            }
          }
          
          if(icolumn>0) {
            if (mapScenarios==FALSE | regexpr("ratio_",master_map_list[k])<0){
              vvar <- predmatrix[,icolumn] 
              if (mapScenarios==TRUE){
                vvar<-ifelse(scenarioFlag==0,NA,vvar)
              }
              
              mapunits <- loadunits[icolumn]
            }else{
              vvar <- predmatrix_chg[,icolumn]
              
              mapunits <- "Ratio of updated to baseline metric"
            }
            
            
          }
        }#exists oparmlist
        
        # Yield matrix  
        if (exists("oyieldlist")){
          if(icolumn == 0) {
            for(i in 1:length(oyieldlist)) {
              if (regexpr("ratio_",master_map_list[k])<0 | mapScenarios==FALSE){
                if(oyieldlist[i] == master_map_list[k]) {
                  icolumn <- i
                }
                
              }else{#ratio
                ratioMetric<-ifelse(master_map_list[k]=="ratio_total","pload_total","pload_inc")
                if(oyieldlist[i] == ratioMetric) {
                  icolumn <- i
                }
              }
            }
            if(icolumn>0) {
              if (mapScenarios==FALSE | regexpr("ratio_",master_map_list[k])<0){
                vvar <- yldmatrix[,icolumn] 
                if (mapScenarios==TRUE){
                  vvar<-ifelse(scenarioFlag==0,NA,vvar)
                }
                mapunits <- yieldunits[icolumn]
                
              }else{
                vvar <- yldmatrix_chg[,icolumn] 
                
                mapunits <- "Ratio of updated to baseline metric"
              }
            }
          }
        }#exists oyieldlist
        
        if (mapScenarios==FALSE){
          # check list of uncertainties:  map_uncertainties  
          if(icolumn == 0) {
            if(!is.na(map_uncertainties[1])){
              for(i in 1:length(map_uncertainties)) {
                if(map_uncertainties[i] == master_map_list[k]) {
                  icolumn <- i
                  mapunits <- "Percent"
                }
              }
              if(icolumn>0) {
                dname <- paste("vvar <- BootUncertainties$",map_uncertainties[icolumn],sep="")
                eval(parse(text=dname)) 
              }
            }
          }
          
          
          # check required variable list:  datalstreq  
          if(icolumn == 0) {
            for(i in 1:length(datalstreq)) {
              if(datalstreq[i] == master_map_list[k]) {
                icolumn <- i
                mapunits <- datalstunits[icolumn]
              }
            }
            if(icolumn>0) {
              dname <- paste("vvar <- ",datalstreq[icolumn],sep="")
              eval(parse(text=dname)) 
            }
          }
        }
        if(icolumn > 0) {
          
          
          
          
          mapgo.list[k] <- icolumn
          mapunits.list[k] <- mapunits
          
          #for output to shapefile
          if (k==1){
            dmapAll<-data.frame(MAPID,vvar)
            names(dmapAll)[1]<-commonvar
          }else{
            dmapAll<-cbind(dmapAll,vvar)
          }
          names(dmapAll)[length(dmapAll)]<-master_map_list[k]
          
          # check for NAs
          eval(parse(text = paste0("testNA$",master_map_list[k],"<-length(vvar[which(is.na(vvar))])")))
          testNAvar<- eval(parse(text = paste0("testNA$",master_map_list[k])))
          
          if (testNAvar!=0){
            vvar1<-vvar[which(is.na(vvar))]
            vvar2<-na.omit(vvar)
          }
          ###############test 1 class
          
          
          if (mapScenarios==TRUE & regexpr("ratio_",master_map_list[k])>0){
            vvar1 <- vvar[vvar==1]
            vvar2 <- vvar[vvar!=1]
          }
          
          
          #set breakpoints
          if (mapScenarios==FALSE | regexpr("ratio_",master_map_list[k])<0){
            
            #set colors
            if (mapScenarios==TRUE){
              Mcolors<-scenarioMapColors[2:length(scenarioMapColors)]
            }else{
              Mcolors<-predictionMapColors
            }
            
            if (testNAvar==0){
              brks<-mapBreaks(vvar,Mcolors)$brks
              uniqueBrks <- unique(brks) # define quartiles
              
              
              
              if (length(brks)>=2){
                
                qvars <- as.integer(cut(vvar, brks, include.lowest=TRUE))  # classify variable
                
                Mcolors <- Mcolors[1:(length(brks)-1)]
                nintervals[k] <- length(uniqueBrks)-1
              }else{
                if (!is.na(brks)){
                  qvars <- as.integer(cut(vvar, brks, include.lowest=TRUE))  # classify variable
                  
                  
                }else{
                  qvars<-rep(1,length(vvar))
                  uniqueBrks<-unique(vvar)
                  
                }              
                Mcolors <- Mcolors[1:1]
                nintervals[k] <- length(uniqueBrks)
              }
              # http://research.stowers-institute.org/efg/R/Color/Chart/index.htm
              MAPCOLORS <- Mcolors[qvars] 
              
            }else{#testNAvar!=0
              chk1 <- mapBreaks(vvar2,Mcolors)$brks
              iprob<-mapBreaks(vvar2,Mcolors)$iprob
              chk <- unique(chk1) # define quartiles with values of 1.0 removed
              qvars<-as.integer(cut(as.numeric(vvar), chk1, include.lowest=TRUE))
              qvars<-ifelse(is.na(qvars),0,qvars)
              qvars<-qvars+1
              if (mapScenarios==FALSE){
                Mcolors <- c("gray",Mcolors)
              }else{
                Mcolors<-scenarioMapColors
              }
              
              Mcolors <- Mcolors[1:(length(chk1)+1)]
              
              
              
              # http://research.stowers-institute.org/efg/R/Color/Chart/index.htm
              MAPCOLORS <- Mcolors[qvars]
            }
            
          }else{#ratio plot
            if (length(vvar2)!=0){
              chk1 <- mapBreaks(vvar2,scenarioMapColors[1:(length(scenarioMapColors)-1)])$brks
              iprob<-mapBreaks(vvar2,scenarioMapColors[1:(length(scenarioMapColors)-1)])$iprob
              chk <- unique(chk1) # define quartiles with values of 1.0 removed
              
              
              chk[iprob+1] <- chk[iprob+1]+1
              qvar1 <- vvar
              qvar1[ qvar1 == 1 ] <- 9999   # code ratios=1 separately
              if (iprob!=0){
                for (i in 1:iprob) {
                  qvar1[ qvar1 >= chk[i] & qvar1 < chk[i+1] ] <- 9999+i
                }
                max <- 9999+i+2
                qvar1[ qvar1 == 9999] <- 1       # code values of 1.0
                for (i in 2:(iprob+1)) {         # reverse code to associate largest reductions with hottest colors
                  qvar1[ qvar1 == (max-i)] <- i
                }
                chk[iprob+1] <- chk[iprob+1]-1
                
                Mcolors <- scenarioMapColors
                Mcolors <- Mcolors[1:(length(chk1)+1)]
                
                # http://research.stowers-institute.org/efg/R/Color/Chart/index.htm
                MAPCOLORS <- Mcolors[qvar1]
              }else{
                MAPCOLORS<-rep(scenarioMapColors[1],length(MAPID))
              }
            }else{#no change scenario
              MAPCOLORS<-rep(scenarioMapColors[1],length(MAPID))
              chk<-1
              iprob<-0
            }
          }#end ratio plot
          
          dmap <- data.frame(MAPID,as.character(MAPCOLORS))   # ,vvar)    # added 3-25-2017
          mapvarname <- paste("MAPCOLORS",k,sep="")
          colnames(dmap) <- c(commonvar,mapvarname) # ,master_map_list[k])
          if ((mapScenarios==FALSE | regexpr("ratio_",master_map_list[k])<0) & testNAvar==0){
            intervals[k,1:length(uniqueBrks)] <- uniqueBrks
          }else{
            intervals[k,1:length(chk)] <- chk
            nintervals[k] <- iprob+1
            
          }
          
          
          eval(parse(text=paste("break1$",master_map_list[k],"<-as.character(intervals[1:nintervals[k]])",sep="")))
          if (mapScenarios==FALSE | regexpr("ratio_",master_map_list[k])<0){
            if (testNAvar==0){
              if (length(unique(vvar))!=1){
                for (i in 1:nintervals[k]) {
                  break1[k][[1]][i] <- paste(round(intervals[k,i],digit=predictionClassRounding)," TO ",round(intervals[k,i+1],digit=predictionClassRounding),sep="")
                }
              }else{
                break1[k][[1]][1] <- paste(round(unique(vvar),digit=predictionClassRounding)," TO ",round(unique(vvar),digit=predictionClassRounding),sep="")
              }
            }else{#testNAvar!=0
              if (mapScenarios==FALSE){
                break1[k][[1]][1] <- 'NA'
              }else{
                break1[k][[1]][1] <- 'No Change'
              }
              
              j<-1
              for (i in 2:nintervals[k]) {
                j <- j+1
                break1[k][[1]][j] <- paste(round(intervals[k,i-1],digit=predictionClassRounding)," TO ",round(intervals[k,i],digit=predictionClassRounding),sep="")
              }
            }
          }else{
            break1[k][[1]][1] <- '1.0 (No Change)'
            if (length(vvar2)!=0){
              j<-1
              for (i in (nintervals[k]):2) {
                j <- j+1
                break1[k][[1]][j] <- paste(round(intervals[k,i-1],digit=predictionClassRounding)," TO ",round(intervals[k,i],digit=predictionClassRounding),sep="")
              }
            }#legnth(vvar2)!=0
          }
          
          
          nlty <-rep(1,nintervals[k])
          nlwd <- rep(lineWidth,nintervals[k])
          
        }
        
        if(mapgo.list[k] > 0){
          
          
          dmapfinal <- merge(dmapfinal,dmap,by=commonvar)
          mapvarname <- paste("dmapfinal$MAPCOLORS",k," <- as.character(dmapfinal$MAPCOLORS",k,")",sep="")
          eval(parse(text=mapvarname))
          
        }
      } # end variable loop
      
      #------------------------------------------------------------#     
      
      
      # merge selected variables to the shape file\
      if ((paste(output_map_type,collapse="") %in% c("stream","both") & Rshiny==FALSE) | 
          (Rshiny==TRUE & input$mapType=="Stream" & mapScenarios==FALSE) | 
          (Rshiny==TRUE & regexpr("stream",paste(output_map_type,collapse=","))>0 & mapScenarios==TRUE)){
        commonvar <- lineWaterid
        names(dmapfinal)[1]<-commonvar
        names(dmapAll)[1]<-commonvar
        lineShape <- sp::merge(lineShape, dmapfinal, by.x = commonvar, by.y = commonvar)
        
        if (Rshiny==FALSE){
          if (mapScenarios==FALSE){
            # Create and output maps
            filename <- paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,run_id,"_prediction_stream_maps.pdf",sep="") 
          }else{
            filename <- paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,scenario_name,"_",run_id,"_prediction_stream_maps.pdf",sep="")
            
          }
          
          pdf(file=filename)
        }
        # loop through each of the variables...
        for (k in 1:length(master_map_list)) {
          testNAvar<-eval(parse(text = paste0("testNA$",master_map_list[k])))
          if (mapScenarios==FALSE | regexpr("ratio_",master_map_list[k])<0){
            #set up colors
            if (mapScenarios==FALSE){
              Mcolors <- predictionMapColors
            }else{
              Mcolors<-scenarioMapColors[2:length(scenarioMapColors)]
            }
            #set NA class
            if (testNAvar!=0){
              if (mapScenarios==FALSE){
                Mcolors <- c("gray",Mcolors)
              }else{
                Mcolors<-scenarioMapColors
              }
            }
            
          }else{
            Mcolors <- scenarioMapColors
          }
          
          
          if (input$batch=="Batch" & Rshiny==TRUE){
            if (mapScenarios==FALSE){
              filename<- paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"Stream",.Platform$file.sep,run_id,"_",master_map_list[k],".pdf",sep="")
            }else{
              if (!dir.exists(paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,sep=""))){
                dir.create(paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,sep=""),showWarnings = FALSE)
              }
              if (!dir.exists(paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,.Platform$file.sep,"Stream",.Platform$file.sep,sep=""))){
                dir.create(paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,.Platform$file.sep,"Stream",.Platform$file.sep,sep=""),showWarnings = FALSE)
              }
              filename<- paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,.Platform$file.sep,"Stream",.Platform$file.sep,
                               input$scenarioName,"_",run_id,"_",scenario_map_list[k],".pdf",sep="")
            }
            pdf(filename)
          }
          
          if (existGeoLines==TRUE){
            plot(GeoLines,col=1,lwd=0.1,xlim=lon_limit,ylim=lat_limit,bg = predictionMapBackground)
          }
          
          # obtain variable settings
          mapvarname <- paste("lineShape$MAPCOLORS",k,sep="")
          # select the shading colors for a given mapping variable
          if (existGeoLines==TRUE){
            xtext <- paste("sp::plot(lineShape,col=",mapvarname,",lwd=lineWidth, add=TRUE)",sep="")
            eval(parse(text=xtext))
          } else {
            xtext <- paste("sp::plot(lineShape,col=",mapvarname,",lwd=lineWidth,bg = predictionMapBackground))",sep="")
            eval(parse(text=xtext))
          }
          if (mapScenarios==FALSE){
            title(master_map_list[k],cex.main = predictionTitleSize)
          }else{
            if (Rshiny==FALSE){
              title(paste(scenario_name,scenario_map_list[k],sep=" "))  
            }else{
              title(paste(input$scenarioName,master_map_list[k],sep=" "),cex.main = predictionTitleSize)  
            }
          }
          
          
          
          legend("bottomleft",break1[k][[1]],lty=nlty,cex=predictionLegendSize,title=mapunits.list[k],
                 bg=predictionLegendBackground,lwd=nlwd, col=Mcolors[1:length(break1[k][[1]])], bty="o")
          
          if (input$batch=="Batch" & Rshiny==TRUE){
            dev.off()
            if (k==length(master_map_list)){
              if (regexpr("catchment",paste(output_map_type,collapse=","))<0){
                shell.exec(filename)
              }
            }
          }
        }#end variable loop
        
        if (Rshiny==FALSE){
          dev.off()  # shuts down current graphics device
          graphics.off()  # shuts down all open graphics devices    
        }
        
        #output shapefile
        if (outputERSImaps[1]=="yes"){
          lineShape <- sp::merge(lineShape, dmapAll, by.x = commonvar, by.y = commonvar)
          lineShape<-lineShape[,which(regexpr("MAPCOLORS",names(lineShape))<0)]
          
          if (Rshiny==FALSE){
            if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""))){
              dir.create(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""),showWarnings = FALSE)
            }
            if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,sep=""))){
              dir.create(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,sep=""),showWarnings = FALSE)
            }
            
            writeSpatialShape(lineShape,paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,"lineShape",sep=""))
            cat(showWKT(proj4string(lineShape)),file=paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,"lineShape.prj",sep="")) 
            
          }else if (mapScenarios==TRUE & input$batch=="Batch"){
            if (!dir.exists(paste(path_results,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""))){
              dir.create(paste(path_results,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""),showWarnings = FALSE)
            }
            
            maptools::writeSpatialShape(lineShape,paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"lineShape",sep=""))
            cat(rgdal::showWKT(sp::proj4string(lineShape)),file=paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"lineShape.prj",sep="")) 
            
          }else if (input$batch=="Batch"){
            if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""))){
              dir.create(paste(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""),showWarnings = FALSE)
            }
            if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,sep=""))){
              dir.create(paste(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,sep=""),showWarnings = FALSE)
            }
            
            maptools::writeSpatialShape(lineShape,paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,"lineShape",sep=""))
            cat(rgdal::showWKT(sp::proj4string(lineShape)),file=paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,"lineShape.prj",sep="")) 
          }
        }
        
        
      }
      
      if (((paste(output_map_type,collapse="") %in% c("catchment","both") & Rshiny==FALSE) | 
           (Rshiny==TRUE & input$mapType=="Catchment" & mapScenarios==FALSE) |
           (Rshiny==TRUE & regexpr("catchment",paste(output_map_type,collapse=","))>0  & mapScenarios==TRUE)) & existpolyShape==TRUE) {
        commonvar <- polyWaterid
        names(dmapfinal)[1]<-commonvar
        names(dmapAll)[1]<-commonvar
        # merge selected variables to the shape file
        polyShape <- sp::merge(polyShape, dmapfinal, by.x = commonvar, by.y = commonvar)
        
        if (Rshiny==FALSE){
          # Create and output maps
          if (mapScenarios==FALSE){
            filename <- paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,run_id,"_prediction_catchment_maps.pdf",sep="")
          }else{
            filename <- paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,scenario_name,"_",run_id,"_prediction_catchment_maps.pdf",sep="")
          }
          pdf(file=filename)
        }
        
        # loop through each of the variables...
        for (k in 1:length(master_map_list)) {
          
          
          if (input$batch=="Batch"){
            if (mapScenarios==FALSE){
              filename<- paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"Catchment",.Platform$file.sep,run_id,"_",master_map_list[k],".pdf",sep="")
            }else{
              if (!dir.exists(paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,sep=""))){
                dir.create(paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,sep=""),showWarnings = FALSE)
              }
              if (!dir.exists(paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,.Platform$file.sep,"Catchment",.Platform$file.sep,sep=""))){
                dir.create(paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,.Platform$file.sep,"Catchment",.Platform$file.sep,sep=""),showWarnings = FALSE)
              }
              filename<- paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,input$scenarioName,.Platform$file.sep,"Catchment",.Platform$file.,
                               input$scenarioName,"_",run_id,"_",scenario_map_list[k],".pdf",sep="")
            }
            
            pdf(filename)
          }
          if (existGeoLines==TRUE){
            plot(GeoLines,col=1,lwd=0.1,xlim=lon_limit,ylim=lat_limit,bg = predictionMapBackground)
          }
          
          # obtain variable settings
          mapvarname <- paste("polyShape$MAPCOLORS",k,sep="")
          
          # select the shading colors for a given mapping variable
          if (existGeoLines==TRUE){
            xtext <- paste("sp::plot(polyShape,col=",mapvarname,",lwd=0.01, lty=0, add=TRUE)",sep="")
            eval(parse(text=xtext))
          } else {
            xtext <- paste("sp::plot(polyShape,col=",mapvarname,",lwd=0.01, lty=0,bg = predictionMapBackground)",sep="")
            eval(parse(text=xtext))
          }
          
          if (mapScenarios==FALSE){
            title(master_map_list[k],cex.main = predictionTitleSize)
          }else{
            if (Rshiny==FALSE){
              title(paste(scenario_name,scenario_map_list[k],sep=" "))  
            }else{
              title(paste(input$scenarioName,scenario_map_list[k],sep=" "),cex.main = predictionTitleSize)  
            }
            
            
          }
          
          
          
          legend("bottomleft",break1[k][[1]],lty=nlty,cex=predictionLegendSize,title=mapunits.list[k],
                 bg=predictionLegendBackground,lwd=nlwd, col=Mcolors[1:length(break1[k][[1]])], bty="o")
          if (input$batch=="Batch"){
            dev.off()
            if (k==length(master_map_list)){
              shell.exec(filename)
            }
          }
          
        }#end variable loop
        
        if (Rshiny==FALSE){
          dev.off()  # shuts down current graphics device
          graphics.off()  # shuts down all open graphics devices
        }
        
        #output shapefile
        if (outputERSImaps[2]=="yes"){
          polyShape <- sp::merge(polyShape, dmapAll, by.x = commonvar, by.y = commonvar)
          polyShape<-polyShape[,which(regexpr("MAPCOLORS",names(polyShape))<0)]
          
          if (Rshiny==FALSE){
            if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""))){
              dir.create(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""),showWarnings = FALSE)
            }
            if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,sep=""))){
              dir.create(paste(path_results,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,sep=""),showWarnings = FALSE)
            }
            
            
            writeSpatialShape(polyShape,paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,"polyShape",sep=""))
            cat(showWKT(proj4string(polyShape)),file=paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,"polyShape.prj",sep="")) 
            
            
          }else if (mapScenarios==TRUE  & input$batch=="Batch"){
            if (!dir.exists(paste(path_results,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""))){
              dir.create(paste(path_results,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""),showWarnings = FALSE)
            }
            
            maptools::writeSpatialShape(polyShape,paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"polyShape",sep=""))
            cat(rgdal::showWKT(sp::proj4string(polyShape)),file=paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"polyShape.prj",sep="")) 
            
          }else if (input$batch=="Batch"){
            if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""))){
              dir.create(paste(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""),showWarnings = FALSE)
            }
            if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,sep=""))){
              dir.create(paste(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,sep=""),showWarnings = FALSE)
            }
            
            maptools::writeSpatialShape(polyShape,paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,"polyShape",sep=""))
            cat(rgdal::showWKT(sp::proj4string(polyShape)),file=paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"prediction",.Platform$file.sep,"polyShape.prj",sep="")) 
          }
        }
      }
    }else {#if length(master_map_list)
      message(' \nWARNING : No mapping executions because predictions are not available and/or mapping variable not available for all reaches\n ')
      if (batch_mode=="yes"){
        cat(' \nWARNING : No mapping executions because predictions are not available and/or mapping variable not available for all reaches\n ')
      }
    }
  } else {
    message(' \nWARNING : No mapping executions because predictions are not available and/or mapping variable not available for all reaches\n ')
    if (batch_mode=="yes"){
      cat(' \nWARNING : No mapping executions because predictions are not available and/or mapping variable not available for all reaches\n ')
    }
  }#end if file.exists(predict.list) or if_predict="yes"
  
  
}#end function

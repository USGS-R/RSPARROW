#'@title executionTree
#'@description Function to trace all function and routine executions from the `startRoutine` 
#'            forward and either print a data.tree of the executions or output a data.table with executions 
#'            and line numbers of executions \\cr \\cr
#'@param path_main character string path to RSPARROW_master directory
#'@param startRoutine character string name of functional RPSPARROW routine
#'@param includeTypes character string vector indicating which types of functions/routines to 
#'       include in executionTree from the function types in RPSARROW_master/inst/tables/funcTypes.csv
#'@param includeList character string vector of functions/routines to include in executionTree 
#'       even if function/routine not in includeTypes
#'@param excludeList character string vector of functions/routines to exclude in executionTree 
#'       even if function/routine in includeTypes
#'@param allOccurances TRUE/FALSE indicating whether all executions of selected functions 
#'       should be output in the data.table of the executionTree.  If `outputType = 'data.tree', 
#'       allOccurances will be reset to FALSE
#'@param outputType character string indicating whether to output a data.table of the exectutionTree or 
#'                  print a data.tree.  Select 'data.table' or 'data.tree'.  Default value is 'data.tree'
#'@param pruneTree number of execution levels to include in the execution tree
#'@param treeLimit number of lines to print in data.tree
#'@return `traceProgram` data.table of all function/routine executions and line number of 
#'            executions after the `startRoutine`



executionTree<-function(path_main,startRoutine = "runRsparrow.R",
                        includeTypes = c("manageDirVars","sparrowSetup","sparrowExec",
                                         "shiny","batch","errorTrap","lists","fortran","rmd"),
                        includeList = NA, excludeList = c("errorOccurred.R","named.list.R"),allOccurances = FALSE,
                        outputType = "data.tree",pruneTree = NA, treeLimit = NA){
  
  #if output to data.tree only use first occurances
  allOccurances<-ifelse(allOccurances & outputType=="data.tree",FALSE,allOccurances)
  
  
  #get all files
  files<-list.files(path_main,full.names = TRUE,pattern="\\.R",recursive = TRUE)
  files<-files[which(sapply(files, function(x) substr(x,nchar(x)-1,nchar(x))==".R") | regexpr("\\.Rmd",files)>0)]
 # files<-files[which(sapply(files, function(x) substr(x,nchar(x)-1,nchar(x))==".R"))]
  forFiles<-list.files(path_main,full.names = TRUE,pattern="\\.for",recursive = TRUE)
  files<-c(files,forFiles)
  files<-files[which(regexpr("\\/inst\\/doc\\/",files)<0)]
  files<-files[which(regexpr("\\/vignettes\\/",files)<0)]
  
  
  
  #find all routines (don't include batchRun--duplication)
  allRoutines<-basename(files)
  
  if (includeTypes[1]!="all"){
    #function types
    funcTypes<-fread(paste0(path_main,"/inst/tables/funcTypes.csv"))
    for (f in 2:length(funcTypes)){
      funcTypes[[f]]<-ifelse(funcTypes[[f]]==1,funcTypes[[1]],NA)
    }
    unPackList(lists = list(types = names(funcTypes)),
               parentObj = list(funcTypes = funcTypes))
    
    
    
    #allTypes
    allTypes<-c("manageDirVars","sparrowSetup","sparrowExec",
                "shiny","batch","errorTrap","utility","lists","rmd","fortran","external")
    #exclude Types
    for (i in allTypes){
      #if not in includeTypes remove utitilty from allRoutines
      if (length(which(tolower(includeTypes)==tolower(i)))==0){  
        typeValues<-na.omit(get(i))
        allRoutines<-allRoutines[which(!allRoutines %in% typeValues)]
      }
    }
    #includeTypes
    for (i in allTypes){
      if (length(which(tolower(includeTypes)==tolower(i)))!=0){
        typeValues<-na.omit(get(i))
        allRoutines<-unique(c(allRoutines,typeValues))
      }
    }
    
    #excludeList
    if (!is.na(excludeList[1])){
      allRoutines<-allRoutines[which(!allRoutines %in% excludeList)]
    }
    #includeList
    if (!is.na(includeList[1])){
      allRoutines<-unique(c(allRoutines,includeList))
    }
    
    #always include "executeRSPARROW.R"
    allRoutines<-unique(c(allRoutines,"executeRSPARROW.R"))
  }#if not all types
  
  #format allRoutines to catch only function calls (don't include batchRun--duplication)
  formatRoutines<-function(routine){
    if (regexpr("\\.for",routine)>0){
      formated<-paste0(gsub("\\.for","",routine),"\\(")
    }else if (regexpr("batch",routine,ignore.case=TRUE)>0 |regexpr("estimateFeval",routine)>0 ){
      formated<-gsub("\\.R","",routine) 
    }else if (regexpr("\\.Rmd",routine)<0){
      formated<-paste0(gsub("\\.R","",routine),"\\(")
      }else if (regexpr("child",routine,ignore.case = TRUE)<0){#rmd
      formated<-paste0('rmarkdown::render\\(paste0\\(path_master,\\"',routine)
      }else{#child rmd
        formated<-paste0('knit_expand\\(path_',gsub("\\.Rmd","",routine))
    }
    return(formated)
  }
  allRoutines<-unlist(lapply(allRoutines, function(r) formatRoutines(r)))
  
  allRoutines<-allRoutines[which(!allRoutines %in% c("batchRun","interactiveBatchRun"))]

  
  
  traceProgram<-data.table(routine = startRoutine,stringsAsFactors = FALSE)
  noCallList<-character(0)
  traceP<-function(startCol,startRow,tr,noCallList,envir= parent.frame()){
    
    
  
    #read routine
    if (endsWith(tr,"\\.R")){
      f<-files[which(gsub("\\.R","",basename(files))==gsub("\\.R","",tr))] 
    }else if (endsWith(tr,"\\.Rmd")){
      f<-files[which(gsub("\\.Rmd","",basename(files))==gsub("\\.Rmd","",tr))]   
    }else{#fortran
      f<-files[which(gsub("\\.for","",basename(files))==gsub("\\.for","",tr))] 
    }

    x <- suppressWarnings(trimws(readLines(f)))
   
    #find exectuted routines
    lines<-sapply(allRoutines, function(r) which(((regexpr(r,x)>0 &  !paste0(gsub("\\\\\\(","",r),".for") %in% basename(forFiles))|
                                                    regexpr(paste0("callModule\\(",gsub("\\\\\\(","",r)),x)>0 |
                                                    regexpr(paste0("\\.Fortran\\('",gsub("\\\\\\(","",r),"'"),x)>0) 
                                                 & !startsWith(x,"#'")))
    lines <- lines[sapply(lines, function(i) length(i)!=0)]
    lines<-lines[which(names(lines)!=gsub("\\.R","",tr))]
   
    if (tr %in% basename(forFiles)){
      lines<-lines[which(names(lines)!=paste0(gsub("\\.for","",tr),"\\("))]
    }
    
    if (tr=="estimateFevalNoadj.R"){
      
      lines<-lines[which(names(lines)!="estimateFeval")]
    }
    
    
    #if any executed
    if (length(lines)!=0){
      
      execRoutines<-data.table(line = unlist(lines, use.names = FALSE))
      
      execRoutines$executes<-unlist(lapply(names(lines), function(x) rep(x,length(lines[[x]]))))
      execRoutines<-execRoutines[,2:1]
      names(execRoutines)<-c("executes","line")
      
      unFormat<-function(routine, files){
        if (paste0(gsub("\\\\\\(","",routine),".R") %in% basename(files)){
          unformated<-paste0(gsub("\\\\\\(","",routine),".R")
        }else if (paste0(gsub("\\\\\\(","",routine),".for") %in% basename(files)){
          unformated<-paste0(gsub("\\\\\\(","",routine),".for")
        }else if (regexpr("child",routine,ignore.case=TRUE)<0){#rmd
         unformated<-gsub('rmarkdown::render\\\\\\(paste0\\\\\\(path_master,\\\\\\"',"",routine)
        }else{#child rmd
          unformated<-paste0(gsub('knit_expand\\\\\\(path_',"",routine),".Rmd")
        }
        return(unformated)
      }
      execRoutines$executes<-sapply(execRoutines$executes, function(x) unFormat(x,files))

      
      
      if (!allOccurances){
        execRoutines<- execRoutines[, lapply(.SD, min), by=executes]
      }
      
      
      execRoutines<-execRoutines[order(execRoutines$line),]
      execRoutines<-execRoutines[which(execRoutines$executes!=tr),]
      
      if (startCol>=4){
        names(execRoutines)[length(execRoutines)-1]<-paste0("executes",startCol/2+1)
        names(execRoutines)[length(execRoutines)]<-paste0("line",startCol/2+1) 
      }else{
        names(execRoutines)[length(execRoutines)-1]<-paste0("executes",startCol)
        names(execRoutines)[length(execRoutines)]<-paste0("line",startCol) 
      }
      
      
      #add to traceProgram
      if (startCol==1){
        traceProgramNew<-traceProgram
      }else{
        traceProgramNew<-traceProgram[startRow,]
        
      }
      
      #replicate traceProgram for length of executed routines     
      traceProgramNew<-as.data.table(matrix(sapply(traceProgramNew, 
                                                   rep.int, times=nrow(execRoutines)),
                                            ncol = length(traceProgramNew)),stringsAsFactors = FALSE)
      
      
      
      #populate names
      names(traceProgramNew)<-names(traceProgram)
      
      # cbind executed routines
      if (startCol==1){
        traceProgramNew<-cbind(traceProgramNew[,1:startCol],execRoutines) 
      }else{
        traceProgramNew<-cbind(traceProgramNew[,1:(startCol+1)],execRoutines)
      }
      
      
      #rbind to traceProgram
      if (!is.null(nrow(traceProgram[-startRow,]))){
        if (nrow(traceProgram[-startRow,])!=0){
          traceProgramNew<-rbind(traceProgramNew,traceProgram[-startRow,],fill = TRUE)
        }
      }
      
    }#end length(lines)
    
    if (exists("traceProgramNew")){
      
      #if last routine
      if (tr==as.character(na.omit(unique(traceProgramNew[[startCol]])))
          [length(na.omit(as.character(unique(traceProgramNew[[startCol]]))))]){
        
        if (length(traceProgram)==length(traceProgramNew)){#stop its done
          
          #exit() 
          return(traceProgram)  
        }else{#go to next column
          
          traceProgram<-traceProgramNew
          
          if (startCol==1){
            startCol<-startCol+1
          }else{
            startCol<-startCol+2
          }
          #go to first row
          startRow<-1
          #assign global
          assign("traceProgram",traceProgram,envir = parent.frame())
          #get nxext routine
          tr<-as.character(na.omit(unique(traceProgram[[startCol]][startRow:nrow(traceProgram)])))[1]
          while (tr %in% noCallList){
            
            if (all(is.na(traceProgram[[startCol]][startRow+1:nrow(traceProgram)]))){
              
              #go to next column
              if (startCol==1){
                startCol<-startCol+1
              }else{
                startCol<-startCol+2
                
              }
              if (length(traceProgram)<startCol+1){#stop your done
                
                #exit() 
                return(traceProgram)
              }
              startRow<-1
            }else{#go to next row
              startRow<-startRow+1
              
              tr<-as.character(na.omit(unique(traceProgram[[startCol]][startRow:nrow(traceProgram)])))[1]
              
            }
          }#end while
          
          Recall(startCol,startRow,tr,noCallList)
        }  
        
      }else{#not last routine in column
        
        # find the next row
        if (startCol==1){
          startRow<-which(is.na(traceProgramNew[[startCol+1]]!=tr))[1]
        }else{
          startRow<-which(is.na(traceProgramNew[[startCol+2]]!=tr))[1]
        }
        
        #assign
        traceProgram<-traceProgramNew
        
        assign("traceProgram",traceProgram,envir = parent.frame())
        #get next routine
        tr<-as.character(na.omit(unique(traceProgram[[startCol]][startRow:nrow(traceProgram)])))[1]
        while (tr %in% noCallList){
          
          if (all(is.na(traceProgram[[startCol]][startRow+1:nrow(traceProgram)]))){
            
            #go to next column
            if (startCol==1){
              startCol<-startCol+1
            }else{
              startCol<-startCol+2
              
            }
            if (length(traceProgram)<startCol+1){#stop your done
              return(traceProgram)
            }
            startRow<-1
          }else{#go to next row
            startRow<-startRow+1
            
            tr<-as.character(na.omit(unique(traceProgram[[startCol]][startRow:nrow(traceProgram)])))[1]
            
          }
        }#end while
        Recall(startCol,startRow,tr,noCallList)
        
      } 
    }else{#no traceProgramNew
      #add to nocallList
      if (!tr %in% noCallList){
        noCallList<-c(noCallList,tr)
        
      }
      
      #if last routine in column
      if (all(is.na(traceProgram[[startCol]][startRow+1:nrow(traceProgram)]))){
        if (startCol==1){
          startCol<-startCol+1
        }else{
          startCol<-startCol+2
          
        }
        #start row 1
        startRow<-1
        
        if (length(traceProgram)<startCol+1){#stop your done
          return(traceProgram)
        }else{
          #assign
          assign("traceProgram",traceProgram,envir = parent.frame())
          
          #get next routine
          tr<-as.character(na.omit(unique(traceProgram[[startCol]][startRow:nrow(traceProgram)])))[1]
          while (tr %in% noCallList){
            
            if (all(is.na(traceProgram[[startCol]][startRow+1:nrow(traceProgram)]))){
              
              #go to next column
              if (startCol==1){
                startCol<-startCol+1
              }else{
                startCol<-startCol+2
                
              }
              if (length(traceProgram)<startCol+1){#stop your done
                return(traceProgram)
              }
              startRow<-1
            }else{#go to next row
              startRow<-startRow+1
              
              tr<-as.character(na.omit(unique(traceProgram[[startCol]][startRow:nrow(traceProgram)])))[1]
              
            }
          }#end while
          
          Recall(startCol,startRow,tr,noCallList)
        }
        
      }else{#go to next row
        
        startRow<-startRow+1
        #assign
        assign("traceProgram",traceProgram,envir = parent.frame())
        #find next routine not in noCallList
        tr<-as.character(na.omit(unique(traceProgram[[startCol]][startRow:nrow(traceProgram)])))[1]
        
        while (tr %in% noCallList){
          
          if (all(is.na(traceProgram[[startCol]][startRow+1:nrow(traceProgram)]))){
            
            #go to next column
            if (startCol==1){
              startCol<-startCol+1
            }else{
              startCol<-startCol+2
              
            }
            if (length(traceProgram)<startCol+1){#stop your done
              return(traceProgram)
            }
            startRow<-1
          }else{#go to next row
            startRow<-startRow+1
            
            tr<-as.character(na.omit(unique(traceProgram[[startCol]][startRow:nrow(traceProgram)])))[1]
            
          }
        }#end while
        
        
        Recall(startCol,startRow,tr,noCallList)
        
      }
    }
    
    #}#end for
    
  }#end func 
  
  #run program trace
  traceP(1,1,startRoutine,noCallList,envir= parent.frame()) 
  
  #order by line
  if (length(traceProgram)!=1){
    lineCols<-which(regexpr("line",names(traceProgram))>0)
    traceProgram[, (lineCols) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = lineCols]
    linenames<-which(regexpr("line",names(traceProgram))>0)

    strOrder<-paste0("'",names(traceProgram)[linenames],"'")
    strOrder<-paste(strOrder,collapse = ",")
    strOrder<-paste0("setorderv(traceProgram,c(",strOrder,"))")
    eval(parse(text = strOrder))

    
    if (outputType=="data.tree"){
      if (is.na(treeLimit)){
        treeLimit<-100000
      }
      #get executes columns
      execCols<-names(traceProgram)[which(regexpr("execute",names(traceProgram))>0)]
      #prune tree
      if (!is.na(pruneTree)){
        pruneTree<-which(as.numeric(as.character(gsub("executes","",execCols)))<=
                           as.numeric(as.character(gsub("executes","",paste0("executes",pruneTree)))))
        execCols<-execCols[pruneTree] 
      }
      #build nodes
      strNode<-paste0("traceProgram$",execCols,collapse = ",")
      strNode<-paste("traceProgram$pathString <- paste(startRoutine,",strNode,",sep='/')")
      eval(parse(text = strNode))
      traceProgram$pathString <-gsub("NA/","",traceProgram$pathString)
      traceProgram$pathString <-gsub("/NA","",traceProgram$pathString)
      
      #print tree
      trNode<-data.tree::as.Node(traceProgram)
      print(trNode, limit = treeLimit)
      
    }else{# return as data.table
      
      return(traceProgram)
    }
    
    
  }else{#no routines executed from startRoutine
    message(paste0("No Routines executed from ",traceProgram))
  }
  
}#end function

#'@title findCodeStr
#'@description find all instances where a given routine is executed, determine all arguments 
#'            for a given routine, find all routines that use a given control setting or functional argument, 
#'            or determine all locations within the system a given string occurs \\cr \\cr
#'@param path_master character string path to RSPARROW_master directory.  Internally reset to 
#'       'RSPARROW_master/R/' subdirectory
#'@param str character string to find in function
#'@param strType type of string search, 'routine' indicates that all instances of a routine 
#'       (str) being executed will be found, 'args' indicates that all function arguments for the routine 
#'       (str) are found, 'setting' indicates that all functions that use the setting (str) as an 
#'       argument are found, and 'all' indicates that all instances of the str should be found



findCodeStr<-function(path_master,str,strType){
  #get all files
  files<-list.files(path_master,full.names = TRUE,pattern="\\.R",recursive = TRUE)
  files<-files[which(sapply(files, function(x) substr(x,nchar(x)-1,nchar(x))==".R") | regexpr("\\.Rmd",files)>0)]
  forFiles<-list.files(path_master,full.names = TRUE,pattern="\\.for",recursive = TRUE)
  files<-c(files,forFiles)
  files<-files[which(regexpr("\\/inst\\/doc\\/",files)<0)]
  files<-files[which(regexpr("\\/vignettes\\/",files)<0)]
  
  for (f in files){
    
    x <- suppressWarnings(readLines(f))
    x<-sapply(x,trimws)
    if (strType=="routine"){
      str<-gsub("\\.for","",str)
      if(regexpr("batch",str,ignore.case=TRUE)>0 |regexpr("estimateFeval",str)>0 ){
        findStr<-gsub("\\.R","",str)
      }else if (f %in% forFiles){
        findStr<-paste0(gsub("\\.for","",str),"\\(")
      }else if (regexpr("\\.Rmd",str)>0 & (regexpr("child",str,ignore.case = TRUE)<0)){#parent rmd
        findStr<-paste0('rmarkdown::render\\(paste0\\(path_master,\\"',str)
      }else if (regexpr("\\.Rmd",str)>0){#child rmd
        findStr<-paste0('knit_expand\\(path_',gsub("\\.Rmd","",str))
        }else{
        findStr<-paste0(gsub("\\.R","",str),"\\(")
      }
      
      lines<-which(((regexpr(findStr,x)>0 &  !paste0(gsub("\\\\\\(","",findStr),".for") %in% basename(forFiles))|
                      regexpr(paste0("callModule\\(",gsub("\\\\\\(","",findStr)),x)>0 |
                      regexpr(paste0("\\.Fortran\\('",gsub("\\\\\\(","",findStr),"'"),x)>0) 
                   & !startsWith(x,"#'"))
      if (gsub("\\.R","",basename(f))==gsub("\\.R","",str) | basename(f) %in% c("executionTree.R","findCodeStr.R")){
        lines<-numeric(0)
      }
      
      
    }else if (strType=="setting"){
      if (regexpr("\\.Rmd",basename(f))<0){
       routine<-gsub("\\.R","",basename(f)) 
      }
      
      if (regexec("\\.for",routine)<0 & regexec("batch",routine, ignore.case = TRUE)<0){
        #find start of function call
        if (regexpr("\\.Rmd",basename(f))<0){
        startStr<-which(regexpr(paste0(routine,"<-function"),gsub(" ","",x))>0)
        }else if (regexpr("child",str,ignore.case = TRUE)<0){#Rmd start
          startStr<-which(regexpr("params:",x)>0)+1
        }
        if (length(startStr)==0){#not a real routine
          findStr<-x[which(regexpr(str,x)>0)]
          lines<-which(regexpr(paste0("\b",str,"\b"),x)>0)  
        }else if (regexpr("\\.Rmd",basename(f))<0){      
          findStr<-x[startStr:length(x)]
          
          #find end of function call
          endStr<-findStr[which(regexpr(")",trimws(findStr))>0)[1]]
          endStr<-which(x==endStr)
          
          #find str
          #findStr<-x[1:endStr]
          findStr<-x[startStr:endStr] 
          if (length(findStr)==1){
            splitFind<-strsplit(findStr,",")[[1]]
            splitFind<-unlist(lapply(splitFind, function(x) strsplit(x,"\\(")[[1]]), use.names = FALSE)
            splitFind<-unlist(sapply(splitFind, function(x) strsplit(x,"\\)")[[1]]), use.names = FALSE)
            lines<-which(trimws(splitFind)==str)
            if (length(lines)!=0){
              lines<-startStr
            }
          }else{
            splitFind<-strsplit(findStr,",")
            splitFind<-lapply(splitFind, function(x) strsplit(x,"\\("))
            splitFind<-lapply(splitFind, function(x) unlist(lapply(x,function(y) strsplit(y,"\\)")),use.names = FALSE))
            splitFind<-lapply(splitFind, function(x) which(trimws(x)==str))
            #lines<-which(regexpr(paste0("\b",str,"\b"),findStr)>0 & !startsWith(findStr,"#"))+ startStr-1
            lines<-unlist(splitFind[which(lapply(splitFind,length)!=0)],use.names = FALSE)
            
          }
          
        }else if (regexpr("child",str,ignore.case = TRUE)<0){#get Rmd params list
           findStr<-x[startStr:length(x)]
           
           #find end of function call
           endStr<-findStr[which(regexpr("---",trimws(findStr))>0)[1]]
           endStr<-which(x==endStr)[2]-1
           
           findStr<-x[startStr:endStr] 
           splitFind<-strsplit(findStr,":")
           splitFind<-unique(trimws(unlist(splitFind)))
           lines<-which(trimws(splitFind)==str)+startStr-1
         }
      }else{
        lines<-numeric(0)
      }
    }else if (strType=="args"){
      if (regexpr("\\.Rmd",basename(f))<0){
      routine<-gsub("\\.R","",basename(f))
      }
      if (routine==gsub("\\.R","",str) & regexpr("\\.Rmd",basename(f))<0){
        #find start of function call
        startStr<-which(regexpr(paste0(routine,"<-function"),gsub(" ","",x))>0)
        
        if (length(startStr)==0){#not a real routine 
          message(paste0("'",str,"' IS NOT A FUNCTIONAL ROUTINE WITH ARGUMENTS"))
          lines<-character(0)
        }else{
          findStr<-x[startStr:length(x)]
          
          #find end of function call
          if (routine!="executionTree"){
            endStr<-findStr[which(regexpr(")",trimws(findStr))>0)[1]]
          }else{
            endStr<-findStr[which(regexpr(")",trimws(findStr))>0)[3]]
          }
          endStr<-which(x==endStr)
          
          #find args
          findStr<-x[startStr:endStr]
          lines<-as.character(unlist(strsplit(findStr,",")))
          lines<-as.character(unlist(strsplit(lines,"\\(")))
          lines<-lines[which(regexpr('\\"',lines)<0)]
          lines[1]<-as.character(unlist(strsplit(trimws(lines[1]),"function\\(")))[2]
          lines[length(lines)]<-as.character(unlist(strsplit(trimws(lines[length(lines)]),"\\)")))[1]
          lines<-lines[which(sapply(lines, function(x) substr(x,1,1)!="#"))]
          lines<-sapply(lines, function(x) trimws(x))
          lines<-lines[which(lines!="" & lines!="..." & !is.na(lines) & regexpr("parent.frame",lines)<0 & regexpr("GlobalEnv",lines)<0)]
        }
      }else if (gsub("\\.Rmd","",basename(f))==gsub("\\.Rmd","",str) & regexpr("\\.Rmd",basename(f))>0 & (regexpr("child",f,ignore.case = TRUE)<0)){#if Rmd
        startStr<-which(regexpr("params:",x)>0)+1
        
        findStr<-x[startStr:length(x)]
        
        #find end of function call
        endStr<-findStr[which(regexpr("---",trimws(findStr))>0)[1]]
        endStr<-which(x==endStr)[2]-1
        
        findStr<-x[startStr:endStr] 
        splitFind<-strsplit(findStr,":")
        lines<-unique(trimws(unlist(splitFind)))
      
        }else{#routine!=str
        lines<-character(0)
      }
    }else{#general str search
      findStr<-x[which(regexpr(str,x)>0)]
      lines<-which(regexpr(str,x)>0)
    }#end if strType
    
    if (length(lines)!=0){ #if str found
      if (strType!="args"){
        found<-data.frame(file = rep(basename(f),length(lines)),
                          line = lines)
      }else{
        found<-data.frame(file = rep(basename(f),length(lines)),
                          arguments = lines)
      }
      
      #compile all strings found
      if (!exists("allReps")){
        allReps<-found
      }else{
        allReps<-rbind(allReps,found)
      }#end exist allReps
    }#end length(lines)
  }#end for f
  
  #print list of found
  if (exists("allReps")){
    return(allReps)
  }else{
    message(paste0("'",str,"' NOT FOUND IN ",path_master))
  }
}#end function

#'@title createInitialParameterControls
#'@description Creates parameters.csv file and design_matrix.csv based on the `varType` column 
#'            of the dataDictionary.csv file. Opens the new parameters.csv and design_matrix.csv files for 
#'            edit. RSPARROW execution is terminated. \\cr \\cr
#'Executed By: executeRSPARROW.R \\cr
#'Executes Routines: \\itemize\{\\item importCSVcontrol.R
#'             \\item unPackList.R\} \\cr
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



createInitialParameterControls<-function(file.output.list,batch_mode){
  exit <- function() {
    .Internal(.invokeRestart(list(NULL, NULL), NULL))
  }
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  #load varnames
  #test if file exists if not design and betas cannot be created
  if (file.exists(file.path(paste(dirname(path_results),.Platform$file.sep,"dataDictionary.csv",sep="")))==TRUE){
    filein <- file.path(paste(dirname(path_results),.Platform$file.sep,"dataDictionary.csv",sep=""))
    Ctype <- c("character","character","character","character","character")
    NAMES<-c("varType","sparrowNames","data1UserNames","varunits","explanation")
    
    #read file
    varnames<-importCSVcontrol(filein,Ctype,NAMES,"paste0('\n \nRUN EXECUTION TERMINATED')",
                               file.output.list,TRUE,batch_mode)
    
    varnames<-varnames[which(!is.na(varnames$sparrowNames) & varnames$sparrowNames!=""),]  
    
    #get relavant types
    allTypes<-c("SOURCE","DELIVF","STRM","RESV")
    betaTypes<-varnames[which(varnames$varType %in% allTypes),]
    missingTypes<-betaTypes[which(!betaTypes$varType %in% allTypes),] 
    
    #test if all missing types
    if(nrow(betaTypes)!=0){
      
      #test if previous design matrix exists
      if (file.exists(file.path(paste(dirname(path_results),.Platform$file.sep,"design_matrix.csv",sep="")))==FALSE){
        
        #create initial design Matrix
        initialdesignMatrix<-data.frame(sparrowNames=betaTypes[which(betaTypes$varType=="SOURCE"),]$sparrowNames)
        delivery<-betaTypes[which(betaTypes$varType=="DELIVF"),]$sparrowNames
        initialdesignMatrix<-cbind(initialdesignMatrix,
                                   as.data.frame(matrix(rep(0,nrow(initialdesignMatrix)),
                                                        ncol=length(delivery),nrow=nrow(initialdesignMatrix))))
        names(initialdesignMatrix)<-c("sparrowNames",as.character(delivery))
        
        #write file
        fwrite(file=paste(dirname(path_results),.Platform$file.sep,"design_matrix.csv",sep=""),initialdesignMatrix,
               row.names=FALSE, col.names=TRUE,showProgress = FALSE,dec = csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")
        cat("\n \n")
        message(paste("INITIAL DESIGN_MATRIX FILE : ",paste(dirname(path_results),.Platform$file.sep,"design_matrix.csv",sep="")," AVAILABLE FOR EDIT",sep=""))
        shell.exec(paste(dirname(path_results),.Platform$file.sep,"design_matrix.csv",sep=""))
        #report missing SOURCE or DELIVF types
        missing<-as.character(missingTypes[which(missingTypes$varType %in% c("SOURCE","DELIVF")),]$varType)
        cat("\n \n")
        if (length(missing)!=0){
          message("MISSING varTypes FOR CREATING INITIAL DESIGN MATRIX :")
          for (i in missing){
            message(i)
          }
        }#if missing
        
        #if betas exists test for mismatches in SOURCE and DELIVF
        if (file.exists(file.path(paste(dirname(path_results),"parameters.csv",sep="")))==TRUE){
          filebetas <- file.path(paste(dirname(path_results),"parameters.csv",sep=""))
          Ctype <- c("character","character","character","numeric","numeric","numeric","character","numeric")
          NAMES<- c("sparrowNames","description","parmUnits","parmInit","parmMin","parmMax","parmType","parmCorrGroup")   
          
          #read file
          betas<-importCSVcontrol(filebetas,Ctype,NAMES,"paste0('\n \nRUN EXECUTION TERMINATED')",
                                  file.output.list,TRUE,batch_mode)
          
          
          sources<-as.character(betas[which(betas$parmType=="SOURCE"),]$sparrowNames)
          delivery<-as.character(betas[which(betas$parmType=="DELIVF"),]$sparrowNames)
          missingSources<-sources[which(!sources %in% initialdesignMatrix$sparrowNames)]
          missingDelivery<-delivery[which(!delivery %in% names(initialdesignMatrix))]
          #report missing SOURCE or DELIVF variables
          cat("\n \n")
          if (length(missingSources)!=0){
            message("MISSING SOURCE VARIABLES FOUND IN PARAMETERS CONTROL FILE :")
            for (i in missingSources){
              message(i)
            }
          }#if missingSources
          if (length(missingDelivery)!=0){
            cat("\n \n")
            message("MISSING DELIVF VARIABLES FOUND IN PARAMETERS CONTROL FILE :")
            for (i in missingDelivery){
              message(i)
            }
          }#if missingDelivery
          
        }else{ #if betas file does not exist create it
          #create get variables and types
          for (t in allTypes[which(allTypes %in% betaTypes$varType)]){
            if (t == allTypes[which(allTypes %in% betaTypes$varType)][1]){
              initialBetas<-data.frame(sparrowNames = betaTypes[which(betaTypes$varType==t),]$sparrowNames,
                                       description = betaTypes[which(betaTypes$varType==t),]$explanation,
                                       parmUnits = betaTypes[which(betaTypes$varType==t),]$varunits,
                                       parmType=rep(t,length(betaTypes[which(betaTypes$varType==t),]$sparrowNames)))
            }else{
              initialBetas<-rbind(initialBetas,
                                  data.frame(sparrowNames = betaTypes[which(betaTypes$varType==t),]$sparrowNames,
                                             description = betaTypes[which(betaTypes$varType==t),]$explanation,
                                             parmUnits = betaTypes[which(betaTypes$varType==t),]$varunits,
                                             parmType=rep(t,length(betaTypes[which(betaTypes$varType==t),]$sparrowNames))))
            }
          }
          #add other columns
          initialBetas<-cbind(initialBetas,as.data.frame(matrix(rep(0,nrow(initialBetas)),ncol=4,nrow=nrow(initialBetas))))
          names(initialBetas)[5:length(initialBetas)]<-c("parmInit","parmMin","parmMax","parmCorrGroup")  
          #order columns
          initialBetas<-initialBetas[,match(c("sparrowNames","description","parmUnits","parmInit","parmMin",
                                              "parmMax","parmType","parmCorrGroup"),names(initialBetas))]
          
          #write file
          fwrite(file=paste(dirname(path_results),.Platform$file.sep,"parameters.csv",sep=""),initialBetas,
                 row.names=FALSE, col.names=TRUE,showProgress = FALSE,dec = csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")
          cat("\n \n")
          message(paste("INITIAL PARAMETERS FILE : ",paste(dirname(path_results),.Platform$file.sep,"parameters.csv",sep="")," AVAILABLE FOR EDIT",sep=""))
          shell.exec(paste(dirname(path_results),.Platform$file.sep,"parameters.csv",sep=""))
          
          #report for missing types STRM or RESV
          missing<-as.character(missingTypes[which(missingTypes$varType %in% c("STRM","RESV")),]$varType)
          cat("\n \n")
          if (length(missing)!=0){
            message("MISSING varTypes FOR CREATING INITIAL PARAMETERS FILE :")
            for (i in missing){
              message(i)
            }
          }#if missing
          cat("\n \n")
          message("RUN EXECUTION TERMINATED")
          exit()
        }#if no betas or design
        
      }else{#if no design
        cat("\n \n")
        message(paste(paste(dirname(path_results),.Platform$file.sep,"design_matrix.csv",sep="")," ALREADY EXISTS.\n
NEW DESIGN_MATRIX FILE NOT CREATED.\n
SET create_initial_parameterControlFiles<-'no' to RUN RSPARROW WITH CURRENT DESIGN_MATRIX.",sep=""))
        if (exists("initialBetas")==TRUE){
          cat("\n \n")
          message("RUN EXECUTION TERMINATED")
          exit()
        }else{#if betas exist terminate
          cat("\n \n")
        }
      }#end if no design
      
      #####################################################################################################################
      if (exists("initialBetas")==FALSE){    
        #test if previous betas exists
        if (file.exists(file.path(paste(dirname(path_results),.Platform$file.sep,"parameters.csv",sep="")))==FALSE){
          #create get variables and types
          for (t in allTypes[which(allTypes %in% betaTypes$varType)]){
            if (t == allTypes[which(allTypes %in% betaTypes$varType)][1]){
              initialBetas<-data.frame(sparrowNames = betaTypes[which(betaTypes$varType==t),]$sparrowNames,
                                       description = betaTypes[which(betaTypes$varType==t),]$explanation,
                                       parmUnits = betaTypes[which(betaTypes$varType==t),]$varunits,
                                       parmType=rep(t,length(betaTypes[which(betaTypes$varType==t),]$sparrowNames)))
            }else{
              initialBetas<-rbind(initialBetas,
                                  data.frame(sparrowNames = betaTypes[which(betaTypes$varType==t),]$sparrowNames,
                                             description = betaTypes[which(betaTypes$varType==t),]$explanation,
                                             parmUnits = betaTypes[which(betaTypes$varType==t),]$varunits,
                                             parmType=rep(t,length(betaTypes[which(betaTypes$varType==t),]$sparrowNames))))
            }
          }
          #add other columns
          initialBetas<-cbind(initialBetas,as.data.frame(matrix(rep(0,nrow(initialBetas)),ncol=4,nrow=nrow(initialBetas))))
          names(initialBetas)[5:length(initialBetas)]<-c("parmInit","parmMin","parmMax","parmCorrGroup")  
          #order columns
          initialBetas<-initialBetas[,match(c("sparrowNames","description","parmUnits","parmInit","parmMin",
                                              "parmMax","parmType","parmCorrGroup"),names(initialBetas))]
          
          #write file
          fwrite(file=paste(dirname(path_results),.Platform$file.sep,"parameters.csv",sep=""),initialBetas,
                 row.names=FALSE, col.names=TRUE,showProgress = FALSE,dec = csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")
          cat("\n \n")
          message(paste("INITIAL PARAMETERS FILE : ",paste(dirname(path_results),.Platform$file.sep,"parameters.csv",sep="")," AVAILABLE FOR EDIT",sep=""))
          shell.exec(paste(dirname(path_results),.Platform$file.sep,"parameters.csv",sep=""))
          
          #report for missing types STRM or RESV
          missing<-as.character(missingTypes[which(missingTypes$varType %in% allTypes),]$varType)
          cat("\n \n")
          if (length(missing)!=0){
            message("MISSING varTypes FOR CREATING INITIAL PARAMETERS FILE :")
            for (i in missing){
              message(i)
            }
          }#if missing
          
          #test for missing source or delivf variables found in design matrix
          if (exists("initialdesignMatrix")==FALSE){
            initialdesignMatrix<-read.csv(file=file.path(paste(dirname(path_results),"design_matrix.csv",sep="")),
                                          dec = csv_decimalSeparator,sep=csv_columnSeparator)
            
            
            initialdesignMatrix<-initialdesignMatrix[apply(initialdesignMatrix,1, function(x) any(!is.na(x))),]
            initialdesignMatrix<-initialdesignMatrix[,apply(initialdesignMatrix,2, function(x) any(!is.na(x)))]
            
          }
          sources<-as.character(initialBetas[which(initialBetas$parmType=="SOURCE"),]$sparrowNames)
          delivery<-as.character(initialBetas[which(initialBetas$parmType=="DELIVF"),]$sparrowNames)
          missingSources<-as.character(initialdesignMatrix[which(!initialdesignMatrix$sparrowNames %in% sources),]$sparrowNames)
          
          missingDelivery<-names(initialdesignMatrix)[which(!names(initialdesignMatrix) %in% c("sparrowNames",delivery))]
          #report missing SOURCE or DELIVF variables
          cat("\n \n")
          if (length(missingSources)!=0){
            message("MISSING SOURCE VARIABLES FOUND IN DESIGN_MATRIX CONTROL FILE :")
            for (i in missingSources){
              message(i)
            }
          }#if missingSources
          if (length(missingDelivery)!=0){
            cat("\n \n")
            message("MISSING DELIVF VARIABLES FOUND IN DESIGN_MATRIX CONTROL FILE :")
            for (i in missingDelivery){
              message(i)
            }
          }#if missingDelivery
          
          cat("\n \n")
          message("RUN EXECUTION TERMINATED")
          exit()
        }else{#if no betas file
          cat("\n \n")
          message(paste(paste(dirname(path_results),.Platform$file.sep,"parameters.csv",sep="")," ALREADY EXISTS.\n
NEW PARAMETERS FILE NOT CREATED.\n
SET create_initial_parameterControlFiles<-'no' to RUN RSPARROW WITH CURRENT PARAMETERS FILE.\n
RUN EXECUTION TERMINATED.",sep=""))
          exit()
        }#end if not betas file
      }#if no initialBetas
      
    }else{# if all missing types
      message("NO VALID varTypes FOUND.\n
NEW PARAMETER CONTROL FILES NOT CREATED.\n
SET varType = 'SOURCE','DELIVF','STRM', or, 'RESV' TO CREATE PARAMETER CONTROL FILES. \n
RUN EXECUTION TERMINATED.")
      exit()
    }
  }else{#no varnames file
    message("NO dataDictionary FILE FOUND.\n
NEW PARAMETER CONTROL FILES NOT CREATED.\n
SET create_initial_parameterControlFiles<-'no' to RUN RSPARROW WITH CURRENT PARAMETER CONTROL FILES.\n
RUN EXECUTION TERMINATED.")
    exit()
  }#end no varnames      
  
  
}#end function

#'@title predictScenarios
#'@description Executes tasks for the management source-change scenarios, including generating 
#'            load predictions for the scenario, saving the predictions to the 'predictScenarios.list', and 
#'            executing the mapping function 'predictMaps'. \\cr \\cr
#'Executed By: \\itemize\{\\item interactiveBatchRun.R
#'             \\item controlFileTasksModel.R
#'             \\item goShinyPlot.R\} \\cr
#'Executes Routines: \\itemize\{\\item batchMaps.R
#'             \\item named.list.R
#'             \\item outputSettings.R
#'             \\item predictMaps.R
#'             \\item predictScenariosOutCSV.R
#'             \\item predictScenariosPrep.R
#'             \\item unPackList.R
#'             \\item deliv_fraction.for
#'             \\item mptnoder.for
#'             \\item ptnoder.for\} \\cr
#'@param input top level interactive user input in Shiny app
#'@param allMetrics character string of all load, yield, uncertainty, and data dictionary 
#'       variables to map in shiny batch mode
#'@param output_map_type character string control setting to identify type of map(s) to output 
#'       to PDF file from "stream","catchment", or "both"
#'@param Rshiny TRUE/FALSE indicating whether routine is being run from the Shiny app
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param JacobResults list output of Jacobian first-order partial derivatives of the model 
#'       residuals `estimateNLLSmetrics.R` contained in the estimate.list object.  For more details see 
#'       documentation Section 5.2.4.5.
#'@param if_predict yes/no indicating whether or not prediction is run
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'       parmMin>=0) | parmType!="SOURCE")`
#'@param subdata data.frame input data (subdata)
#'@param add_vars additional variables specified by the setting `add_vars` to be included in 
#'       prediction, yield, and residuals csv and shape files
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@param RSPARROW_errorOption 



predictScenarios <- function(#Rshiny
  input,allMetrics, output_map_type,Rshiny,
  #regular
  estimate.input.list,
  predict.list,scenario.input.list,
  data_names,JacobResults, if_predict,
  #bootcorrection,
  DataMatrix.list,SelParmValues,subdata,
  #predictStreamMapScenarios
  file.output.list,
  #scenarios out
  add_vars,
  mapping.input.list,
  batch_mode,
  RSPARROW_errorOption) {
  
  # 
  # Output matrices in returned object 'predictScenarios.list':
  #  predmatrix - absolute loads reflecting change from load-reduction scenarios
  #  yldmatrix - absolute concentration and yields reflecting change from load-reduction scenarios
  #  predmatrix_chg - change from baseline loads, expressed as ratio of new to baseline load
  #  yldmatrix_chg - change from baseline concentration and yields, expressed as a ratio of new to baseline conditions
  
  #################################################
  
  
  unPackList(lists = list(file.output.list = file.output.list,
                          scenario.input.list = scenario.input.list,
                          estimate.input.list = estimate.input.list),
             parentObj = list(NA,NA,NA)) 
  
  if (Rshiny==TRUE){
    scenario_name<-as.character(input$scenarioName)
  }
  #delete files in subdirectory
  dirout <- paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,sep="")
  if (dir.exists(dirout)){
    filesList<-list.files(dirout,recursive = TRUE,full.names=TRUE)
    if (length(filesList)!=0){
      unlink(filesList,recursive = TRUE)
    }
  }
  
  if (Rshiny==FALSE){
    #save flag_TargetReachWatersheds.csv
    fileout <- paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,"flag_TargetReachWatersheds.csv",sep="")
    outFlag<-subdata[,which(names(subdata) %in% c("waterid_for_RSPARROW_mapping","demtarea","demiarea","rchname",add_vars))]
    names(outFlag)[which(names(outFlag)=="waterid_for_RSPARROW_mapping")]<-"waterid"
    outFlag$flag<-rep(0,nrow(outFlag))
    outFlag<-outFlag[c("flag","waterid",names(outFlag)[which(!names(outFlag) %in% c("flag","waterid"))])]
    fwrite(outFlag,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,col.names=TRUE,
           dec = csv_decimalSeparator,sep=csv_columnSeparator,na = "NA")
  }
  
  if ((select_scenarioReachAreas!="none" & Rshiny==FALSE) | Rshiny==TRUE){ 
    
    if (Rshiny==TRUE){
      scenario_sources<-as.character(input$scenario_sources)
    }
    
    # Calculate and output with bias-corrected predictions
    if(is.null(JacobResults$mean_exp_weighted_error) == TRUE) {
      bootcorrection <- 1.0
    } else {
      bootcorrection <- JacobResults$mean_exp_weighted_error
    }
    if (file.exists(paste(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list",sep="")) | if_predict == "yes"){
      if (if_predict == "no"){
        load(paste(path_results,.Platform$file.sep,"predict",.Platform$file.sep,run_id,"_predict.list",sep=""))
      } 
      
      # perform checks on scenario variable names designated by user
      #  scenario only executed if all source variables match
      
      vcheck<-0
      for (i in 1:length(JacobResults$Parmnames)) {
        for (j in 1:length(scenario_sources)) {
          if(scenario_sources[j] == JacobResults$Parmnames[i]) {vcheck<-vcheck+1}
        }
      }
      
      if(vcheck == length(scenario_sources)) {  # source names match
        
        message("Running predict scenarios...")
        
        #set data object for predictScenarios              
        data <- DataMatrix.list$data 
        
        
        scenarioPrep.list<-predictScenariosPrep(##Rshiny
          input,allMetrics, output_map_type,Rshiny,
          ##regular
          scenario.input.list,
          data_names,
          if_predict,
          #data
          data,
          SelParmValues$srcvar,DataMatrix.list$data.index.list$jsrcvar,
          DataMatrix.list$dataNames,JacobResults,
          subdata,
          #paths
          file.output.list)
        
        
        # create global variable from list names (JacobResults)
        # 'oEstimate' containing the estimated mean parameters for all non-constant and constant parameters
        # 'Parmnames' list of variable names 
        # transfer required variables to global environment from SUBDATA 
        # create global variable from list names
        # transfer required variables to global environment from 'DataMatrix.list$data.index.list'
        # transfer required variables to global environment from 'scenarioPrep.list'
        unPackList(lists = list(JacobResults = JacobResults,
                                datalstCheck = data_names$sparrowNames,
                                SelParmValues = SelParmValues,
                                estimate.input.list = estimate.input.list,
                                data.index.list = DataMatrix.list$data.index.list,
                                scenarioPrep.list = scenarioPrep.list),
                   parentObj = list(NA,
                                    subdata = subdata,
                                    NA,
                                    NA,
                                    NA,
                                    NA))
        ###################################
        ###################################
        ###################################
        
        if (scenarioError==FALSE){  
          
          # transfer the baseline predictions for load and yield
          predmatrix_base <- predict.list$predmatrix
          yldmatrix_base <- predict.list$yldmatrix
          
          
          ################################
          # Setup variables for prediction 
          
          nreach <- length(data[,1])
          numsites <- sum(ifelse(data[,10] > 0,1,0))  # jdepvar site load index
          
          
          # setup for REACH decay
          jjdec <- length(jdecvar)
          if(sum(jdecvar) > 0) { 
            rchdcayf <- matrix(1,nrow=nreach,ncol=1)
            for (i in 1:jjdec){
              rchdcayf[,1] <- rchdcayf[,1] * eval(parse(text=reach_decay_specification))
            } 
          } else {  
            rchdcayf <- matrix(1,nrow=nreach,ncol=1)
          }
          
          # setup for RESERVOIR decay
          jjres <- length(jresvar)
          if(sum(jresvar) > 0) {
            resdcayf <- matrix(1,nrow=nreach,ncol=1)
            for (i in 1:jjres){
              resdcayf[,1] <- resdcayf[,1] * eval(parse(text=reservoir_decay_specification))
            }
          } else { 
            resdcayf <- matrix(1,nrow=nreach,ncol=1)
          } 
          
          # Setup for SOURCE DELIVERY # (nreach X nsources)
          jjdlv <- length(jdlvvar)
          jjsrc <- length(jsrcvar)
          
          
          ddliv1 <- matrix(0,nrow=nreach,ncol=jjdlv)
          if(sum(jdlvvar) > 0) {
            for (i in 1:jjdlv){
              ddliv1[,i] <- (beta1[,jbdlvvar[i]] * data[,jdlvvar[i]])
            }
            ddliv2 <- matrix(0,nrow=nreach,ncol=jjsrc)
            ddliv2 <- eval(parse(text=incr_delivery_specification))     # "exp(ddliv1 %*% t(dlvdsgn))"
          } else {
            ddliv2 <- matrix(1,nrow=nreach,ncol=jjsrc)   # change ncol from =1 to =jjsrc to avoid non-conformity error (2-19-2013)
          }
          
          # Setup for SOURCE
          ddliv3 <- (ddliv2 * data[,jsrcvar]) * beta1[,jbsrcvar]
          if(sum(jsrcvar) > 0) {
            dddliv <- matrix(0,nrow=nreach,ncol=1)
            for (i in 1:jjsrc){
              dddliv[,1] <- dddliv[,1] + ddliv3[,i]
            }
          } else {
            dddliv <- matrix(1,nrow=nreach,ncol=1)
          }
          
          ####################################################
          # incremental delivered load for decayed and nondecayed portions
          
          incdecay <- rchdcayf**0.5 * resdcayf   # incremental reach and reservoir decay
          totdecay <- rchdcayf * resdcayf        # total reach and reservoir decay
          
          incddsrc <- rchdcayf**0.5 * resdcayf * dddliv
          incddsrc_nd <- dddliv
          
          # Compute the reach transport factor
          carryf <- data[,jfrac] * rchdcayf * resdcayf
          carryf_nd <- data[,jfrac] 
          
          ####################################################
          # Store the incremental loads for total and sources
          
          pload_inc <- as.vector(dddliv)  # create incremental load variable
          
          srclist_inc <- character(length(jsrcvar))
          
          for (j in 1:length(jsrcvar)) {  
            ddliv <- as.matrix((ddliv2[,j] * data[,jsrcvar[j]]) * beta1[,jbsrcvar[j]] ) 
            assign(paste("pload_inc_",Parmnames[j],sep=""),as.vector(ddliv))   # create variable 'pload_inc_(source name)'
            srclist_inc[j] <- paste("pload_inc_",Parmnames[j],sep="")
          }
          
          ####################################################
          # Store the total decayed and nondecayed loads
          
          nnode <- max(data[,jtnode],data[,jfnode])
          ee <- matrix(0,nrow=nreach,ncol=1)
          pred <- matrix(0,nrow=nreach,ncol=1)
          
          i_obs <- 1
          
          data2 <- matrix(0,nrow=nreach,ncol=4)
          data2[,1] <- data[,jfnode]
          data2[,2] <- data[,jtnode]
          data2[,3] <- data[,jdepvar]
          data2[,4] <- data[,jiftran]
          
          
          # Total decayed load (no monitoring adjustment)
          incddsrc <- ifelse(is.na(incddsrc),0,incddsrc)
          carryf <- ifelse(is.na(carryf),0,carryf)
          ifadjust <- 0     # no monitoring load adjustment
          
          # accumulate loads
          return_data <- .Fortran('ptnoder',
                                  ifadjust=as.integer(ifadjust),
                                  nreach=as.integer(nreach),
                                  nnode=as.integer(nnode),
                                  data2=as.double(data2),
                                  incddsrc=as.double(incddsrc),
                                  carryf=as.double(carryf),
                                  ee=as.double(ee),PACKAGE="ptnoder") 
          pred <- return_data$ee
          
          pload_total <- pred   # nonadjusted total load 
          
          
          # Total decayed load (with monitoring adjustment)
          
          incddsrc <- ifelse(is.na(incddsrc),0,incddsrc)
          carryf <- ifelse(is.na(carryf),0,carryf)
          ifadjust <- 1     # monitoring load adjustment
          
          # Fortran subroutine to accumulate mass climbing down the reach network, compute and accumulate incremental RCHLD
          #   tnoder.dll must be placed in SYSTEM PATH accessible directory
          
          return_data <- .Fortran('ptnoder',
                                  ifadjust=as.integer(ifadjust),
                                  nreach=as.integer(nreach),
                                  nnode=as.integer(nnode),
                                  data2=as.double(data2),
                                  incddsrc=as.double(incddsrc),
                                  carryf=as.double(carryf),
                                  ee=as.double(ee),PACKAGE="ptnoder") 
          pred <- return_data$ee
          
          mpload_total <- pred  # monitoring-adjusted total load
          
          
          # Total nondecayed load 
          
          incddsrc_nd <- ifelse(is.na(incddsrc_nd),0,incddsrc_nd)
          carryf_nd <- ifelse(is.na(carryf_nd),0,carryf_nd)
          pred <- matrix(0,nrow=nreach,ncol=1)
          ifadjust <- 0     # no monitoring load adjustment
          
          # Fortran subroutine to accumulate mass climbing down the reach network, compute and accumulate incremental RCHLD
          #   tnoder.dll must be placed in SYSTEM PATH accessible directory
          
          return_data <- .Fortran('ptnoder',
                                  ifadjust=as.integer(ifadjust),
                                  nreach=as.integer(nreach),
                                  nnode=as.integer(nnode),
                                  data2=as.double(data2),
                                  incddsrc_nd=as.double(incddsrc_nd),
                                  carryf_nd=as.double(carryf_nd),
                                  ee=as.double(ee),PACKAGE="ptnoder") 
          pred <- return_data$ee
          
          pload_nd_total <- pred   
          
          
          # Total load for each SOURCE (decayed and nondecayed)
          
          srclist_total <- character(length(jsrcvar))
          srclist_nd_total <- character(length(jsrcvar))
          srclist_mtotal <- character(length(jsrcvar))
          
          for (j in 1:length(jsrcvar)) { 
            
            ddliv <- as.matrix((ddliv2[,j] * data[,jsrcvar[j]]) * beta1[,jbsrcvar[j]] ) 
            
            # incremental delivered load
            incddsrc <- rchdcayf**0.5 * resdcayf * ddliv
            incddsrc_nd <- ddliv
            
            # Compute the reach transport factor
            carryf <- data[,jfrac] * rchdcayf * resdcayf
            carryf_nd <- data[,jfrac] 
            
            # Decayed total source load
            pred <- matrix(0,nrow=nreach,ncol=1)
            i_obs <- 1
            
            incddsrc <- ifelse(is.na(incddsrc),0,incddsrc)
            carryf <- ifelse(is.na(carryf),0,carryf)
            ifadjust <- 0     # no monitoring load adjustment
            
            return_data <- .Fortran('ptnoder',
                                    ifadjust=as.integer(ifadjust),
                                    nreach=as.integer(nreach),
                                    nnode=as.integer(nnode),
                                    data2=as.double(data2),
                                    incddsrc=as.double(incddsrc),
                                    carryf=as.double(carryf),
                                    ee=as.double(ee),PACKAGE="ptnoder") 
            pred <- return_data$ee
            
            assign(paste("pload_",Parmnames[j],sep=""),pred)   # create variable 'pload_(source name)'
            srclist_total[j] <- paste("pload_",Parmnames[j],sep="")
            
            assign(paste("mpload_",Parmnames[j],sep=""),pred)   # create variable 'mpload_(source name)'
            srclist_mtotal[j] <- paste("mpload_",Parmnames[j],sep="")
            
            # Nondecayed total source load
            pred <- matrix(0,nrow=nreach,ncol=1)
            i_obs <- 1
            
            incddsrc_nd <- ifelse(is.na(incddsrc_nd),0,incddsrc_nd)
            carryf_nd <- ifelse(is.na(carryf_nd),0,carryf_nd)
            ifadjust <- 0     # no monitoring load adjustment
            
            return_data <- .Fortran('ptnoder',
                                    ifadjust=as.integer(ifadjust),
                                    nreach=as.integer(nreach),
                                    nnode=as.integer(nnode),
                                    data2=as.double(data2),
                                    incddsrc_nd=as.double(incddsrc_nd),
                                    carryf_nd=as.double(carryf_nd),
                                    ee=as.double(ee),PACKAGE="ptnoder") 
            pred <- return_data$ee
            
            assign(paste("pload_nd_",Parmnames[j],sep=""),pred)   # create variable 'pload_(source name)'
            srclist_nd_total[j] <- paste("pload_nd_",Parmnames[j],sep="")
          } 
          
          
          # Monitoring-adjusted source loads (computed as shares of nonadjusted total loads)
          if(numsites > 0) {
            srclist_mtotal <- character(length(jsrcvar))
            for (j in 1:length(jsrcvar)) {
              share <- eval(parse(text=srclist_total[j])) / pload_total   # source share of total load
              share <- ifelse(is.na(share),0,share)
              
              ddliv <- as.matrix((ddliv2[,j] * data[,jsrcvar[j]]) * beta1[,jbsrcvar[j]] ) 
              
              # incremental delivered load
              incddsrc <- rchdcayf**0.5 * resdcayf * ddliv
              incddsrc_nd <- ddliv
              
              # Compute the reach transport factor
              carryf <- data[,jfrac] * rchdcayf * resdcayf
              carryf_nd <- data[,jfrac] 
              
              # Decayed total source load
              pred <- matrix(0,nrow=nreach,ncol=1)
              
              incddsrc <- ifelse(is.na(incddsrc),0,incddsrc)
              carryf <- ifelse(is.na(carryf),0,carryf)
              ifadjust <- 1     # monitoring load adjustment
              
              return_data <- .Fortran('mptnoder',
                                      ifadjust=as.integer(ifadjust),
                                      share=as.double(share),
                                      nreach=as.integer(nreach),
                                      nnode=as.integer(nnode),
                                      data2=as.double(data2),
                                      incddsrc=as.double(incddsrc),
                                      carryf=as.double(carryf),
                                      ee=as.double(ee),PACKAGE="mptnoder") 
              pred <- return_data$ee
              
              assign(paste("mpload_",Parmnames[j],sep=""),pred)   # create variable 'mpload_(source name)'
              srclist_mtotal[j] <- paste("mpload_",Parmnames[j],sep="")
            }
          }
          
          # Delivery fraction
          data2 <- matrix(0,nrow=nreach,ncol=5)
          data2[,1] <- data[,jfnode]
          data2[,2] <- data[,jtnode]
          data2[,3] <- data[,jfrac]
          data2[,4] <- data[,jiftran]
          data2[,5] <- data[,jtarget]    # termflag indicators (=1, =3)
          
          deliver <- function(incdecay) { 
            sumatt <- matrix(0,nrow=nreach,ncol=1)
            fsumatt <- matrix(0,nrow=nreach,ncol=1)
            return_data <- .Fortran('deliv_fraction',
                                    numrchs=as.integer(nreach),
                                    waterid=as.integer(waterid),
                                    nnode=as.integer(nnode),
                                    data2=as.double(data2),
                                    incdecay=as.double(incdecay),
                                    totdecay=as.double(totdecay),
                                    sumatt=as.double(sumatt),PACKAGE="deliv_fraction") 
            fsumatt <- return_data$sumatt
            return(fsumatt) 
          }  # end sumatts function
          deliv_frac <- deliver(incdecay)
          
          #######################################
          # Output load predictions
          
          srclist_inc_deliv <- character(length(jsrcvar))   # delivered incremental load
          for (i in 1:length(jsrcvar)) {
            srclist_inc_deliv[i] <- paste(srclist_inc[i],"_deliv",sep="")
          }
          
          srclist_inc_share <- character(length(jsrcvar))   # incremental source share (percent)
          for (i in 1:length(jsrcvar)) {
            srclist_inc_share[i] <- paste("share_inc_",srcvar[i],sep="")
          }
          
          srclist_total_share <- character(length(jsrcvar))   # total source share (percent)
          for (i in 1:length(jsrcvar)) {
            srclist_total_share[i] <- paste("share_total_",srcvar[i],sep="")
          }
          
          oparmlist <- c("waterid","pload_total",srclist_total,
                         "mpload_total",srclist_mtotal, 
                         "pload_nd_total",srclist_nd_total, 
                         "pload_inc",srclist_inc, 
                         "deliv_frac",
                         "pload_inc_deliv",srclist_inc_deliv,
                         srclist_total_share,srclist_inc_share) 
          
          # create matrix with predictions
          ncols <- 7 + length(srclist_total) + length(srclist_mtotal) + length(srclist_nd_total) + 
            length(srclist_inc) + length(srclist_inc) + length(srclist_inc) + length(srclist_inc)
          predmatrix <- matrix(0,nrow=length(pload_total),ncol=ncols)
          
          
          predmatrix[,1] <- subdata$waterid
          
          # total load
          predmatrix[,2] <- pload_total * as.vector(bootcorrection)
          for (i in 1:length(srclist_total)){
            predmatrix[,2+i] <- eval(parse(text=srclist_total[i])) * as.vector(bootcorrection)
            predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+i)] <- 
              predmatrix[,2+i] / predmatrix[,2] * 100  # source share
            
            # avoids reporting NAs for share
            predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+i)] <- 
              ifelse(is.na(predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+i)]),
                     0,predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+i)]) 
          }
          # monitored-adjusted total load
          predmatrix[,(3+length(srclist_total))] <- mpload_total 
          for (i in 1:length(srclist_mtotal)){
            predmatrix[,(3+length(srclist_total)+i)] <- eval(parse(text=srclist_mtotal[i]))
          } 
          # nondecayed (ND) total load
          predmatrix[,(4+length(srclist_total)+length(srclist_mtotal))] <- pload_nd_total * as.vector(bootcorrection)
          for (i in 1:length(srclist_nd_total)){
            predmatrix[,(4+length(srclist_total)+length(srclist_mtotal)+i)] <- eval(parse(text=srclist_nd_total[i])) * as.vector(bootcorrection)
          }
          # incremental load
          predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total))] <- pload_inc * as.vector(bootcorrection)
          for (i in 1:length(srclist_inc)){
            predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+i)] <- eval(parse(text=srclist_inc[i])) * as.vector(bootcorrection) 
            predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+
                           length(srclist_inc)+length(srclist_inc)+length(srclist_inc)+i)] <- 
              predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+i)] / 
              predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total))] * 100  # source share
            
            # avoids reporting NAs for share
            predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+
                           length(srclist_inc)+length(srclist_inc)+length(srclist_inc)+i)] <- 
              ifelse(is.na(predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+length(srclist_inc)+i)]),
                     0,predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+length(srclist_inc)+i)])
          }
          # delivery fraction
          predmatrix[,(6+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc))] <- deliv_frac
          
          # delivered incremental load
          dload <- predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total))] * deliv_frac
          predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc))] <- dload
          for (i in 1:length(srclist_inc)){
            dload <- predmatrix[,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+i)] * deliv_frac
            predmatrix[,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+i)] <- dload
          }
          
          
          # Output yield predictions
          
          # replace "pload" with "yld" in each string
          srclist_yield <- gsub("pload", "yield", srclist_total)
          srclist_myield <- gsub("pload", "yield", srclist_mtotal)
          srclist_yldinc <- gsub("pload", "yield", srclist_inc)
          srclist_yldinc_deliv <- gsub("pload", "yield", srclist_inc_deliv)
          
          oyieldlist <- c("waterid","concentration","yield_total",srclist_yield,
                          "myield_total",srclist_myield, 
                          "yield_inc",srclist_yldinc, 
                          "yield_inc_deliv",srclist_yldinc_deliv) 
          
          
          ncols <- 6 + length(srclist_yield) + length(srclist_myield) + length(srclist_yldinc) + length(srclist_yldinc_deliv)
          yldmatrix <- matrix(0,nrow=length(pload_total),ncol=ncols)
          
          yldmatrix[,1] <- subdata$waterid
          # total yield
          for (i in 1:length(subdata$waterid)) {
            if(demtarea[i] > 0) {
              if(meanq[i] > 0) { yldmatrix[i,2] <- predmatrix[i,2] / meanq[i] * ConcFactor }   # concentration
              yldmatrix[i,3] <- predmatrix[i,2] / demtarea[i]* yieldFactor
              
              for (j in 1:length(srclist_total)){
                yldmatrix[i,3+j] <- predmatrix[i,2+j] / demtarea[i]* yieldFactor
              }
              # monitored-adjusted total yield
              yldmatrix[i,(4+length(srclist_total))] <- predmatrix[i,(3+length(srclist_total))] / demtarea[i]* yieldFactor
              for (j in 1:length(srclist_mtotal)){
                yldmatrix[i,(4+length(srclist_total)+j)] <- predmatrix[i,(3+length(srclist_total)+j)] / demtarea[i]* yieldFactor
              }
            }
          }
          # incremental yield
          for (i in 1:length(subdata$waterid)) {
            if(demiarea[i] > 0) {
              yldmatrix[i,(5+length(srclist_total)+length(srclist_mtotal))] <- predmatrix[i,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total))] / demiarea[i]
              
              for (j in 1:length(srclist_inc)){
                yldmatrix[i,(5+length(srclist_total)+length(srclist_mtotal)+j)] <- predmatrix[i,(5+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+j)] / demiarea[i]
              }
              
              # delivered incremental yield
              yldmatrix[i,(6+length(srclist_total)+length(srclist_mtotal)+length(srclist_inc))] <- 
                predmatrix[i,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc))] / demiarea[i]
              
              for (j in 1:length(srclist_inc)){
                yldmatrix[i,(6+length(srclist_total)+length(srclist_mtotal)+length(srclist_inc)+j)] <- 
                  predmatrix[i,(7+length(srclist_total)+length(srclist_mtotal)+length(srclist_nd_total)+length(srclist_inc)+j)]  / demiarea[i]
              }
            }
          }
          
          
          # Do not include monitoring-adjusted (conditional) prediction label objects in saved list
          predict.source.list <- named.list(srclist_total,srclist_inc,srclist_inc_deliv,
                                            srclist_nd_total,srclist_yield,srclist_yldinc,
                                            srclist_yldinc_deliv)
          
          
          # Compute the change from load-reduction scenarios, expressed as ratio of new to baseline load
          predmatrix_chg <- matrix(0,nrow=length(pload_total),ncol=ncol(predmatrix_base))
          yldmatrix_chg <- matrix(0,nrow=length(pload_total),ncol=ncol(yldmatrix_base))
          
          predmatrix_chg[,1] <- predmatrix[,1]
          yldmatrix_chg[,1] <- yldmatrix[,1]
          for (i in 2:ncol(predmatrix_base)) {
            temp <- predmatrix[,i] / predmatrix_base[,i]
            temp <- ifelse(is.na(temp),0,temp)
            predmatrix_chg[,i] <- ifelse(temp==0,1,temp)  # added 4-19-2019 to address cases where pload_total and ratio is 0
            predmatrix_chg[,i] <- ifelse(is.infinite(predmatrix_chg[,i]),NA,predmatrix_chg[,i])
          }
          for (i in 2:ncol(yldmatrix_base)) {
            temp <- yldmatrix[,i] / yldmatrix_base[,i]
            temp <- ifelse(is.na(temp),0,temp)
            yldmatrix_chg[,i] <- ifelse(temp==0,1,temp)   # added 4-19-2019 to address cases where pload_total and ratio is 0
            yldmatrix_chg[,i] <- ifelse(is.infinite(yldmatrix_chg[,i]),NA,yldmatrix_chg[,i])
          }
          
          
          # Convert matrices to data frames with column headers (previously performed in the predictScenariosOutCSV.R function)
          predmatrix <- as.data.frame(predmatrix)
          colnames(predmatrix) <- oparmlist 
          
          yldmatrix <- as.data.frame(yldmatrix)
          colnames(yldmatrix) <- oyieldlist 
          
          predmatrix_chg <- as.data.frame(predmatrix_chg)
          colnames(predmatrix_chg) <- oparmlist 
          
          yldmatrix_chg <- as.data.frame(yldmatrix_chg)
          colnames(yldmatrix_chg) <- oyieldlist 
          
          # Drop monitoring-adjusted (conditional) predictions from objects (headers and predictions)
          dropLoadNames <- c("mpload_total",srclist_mtotal,"deliv_frac")
          dropYieldNames <-c("myield_total",srclist_myield)
          
          oparmlist <- c("waterid","pload_total",srclist_total,
                         "pload_nd_total",srclist_nd_total, 
                         "pload_inc",srclist_inc, 
                         "pload_inc_deliv",srclist_inc_deliv,
                         srclist_total_share,srclist_inc_share) 
          oyieldlist <- c("waterid","concentration","yield_total",srclist_yield,
                          "yield_inc",srclist_yldinc, 
                          "yield_inc_deliv",srclist_yldinc_deliv)
          
          predmatrix <- predmatrix[, !(colnames(predmatrix) %in% dropLoadNames)]
          yldmatrix <- yldmatrix[, !(colnames(yldmatrix) %in% dropYieldNames)]
          predmatrix_chg <- predmatrix_chg[, !(colnames(predmatrix_chg) %in% dropLoadNames)]
          yldmatrix_chg <- yldmatrix_chg[, !(colnames(yldmatrix_chg) %in% dropYieldNames)]
          
          # Assign the load and yield units
          loadunits <- rep(loadUnits,ncol(predmatrix))
          loadunits[1] <- "Reach ID Number"
          for (i in 1:length(srclist_total)){
            loadunits[(5+length(srclist_total)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+i)] <- "Percent"  # source share
            loadunits[(5+length(srclist_total)+length(srclist_nd_total)+length(srclist_inc)+length(srclist_inc)+length(srclist_inc)+i)] <- "Percent"  # source share
          }
          yieldunits <- rep(yieldUnits,ncol(yldmatrix))
          yieldunits[1] <- "Reach ID Number"
          yieldunits[2] <- ConcUnits
          
          
          if (Rshiny==TRUE){
            scenario_name<-as.character(input$scenarioName)
          }
          
          
          predictScenarios.list <- named.list(select_scenarioReachAreas,select_targetReachWatersheds,scenario_name,
                                              scenario_sources,scenario_factors,landuseConversion,
                                              oparmlist,loadunits,predmatrix,oyieldlist,yieldunits,yldmatrix,
                                              predict.source.list,predmatrix_chg,yldmatrix_chg,scenarioCoefficients,scenarioFlag)
          assign("predictScenarios.list",predictScenarios.list, envir=.GlobalEnv)
          
          objfile <- paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,scenario_name,"_",run_id,"_predictScenarios.list",sep="")
          save(predictScenarios.list,file=objfile)
          
          # Save the data input matrix with altered source values per scenario adjustments
          dataNames <- DataMatrix.list$dataNames
          DataMatrixScenarios.list <- named.list(dataNames,data)
          assign("DataMatrixScenarios.list",DataMatrixScenarios.list, envir=.GlobalEnv)
          
          objfile <- paste(path_results,.Platform$file.sep,"scenarios",.Platform$file.sep,scenario_name,.Platform$file.sep,scenario_name,"_",run_id,"_DataMatrixScenarios.list",sep="")
          save(DataMatrixScenarios.list,file=objfile)
          
          #########################################
          #########################################
          ##################end predictScenarios#######
          
          #output csv files
          predictScenariosOutCSV(#Rshiny 
            input, Rshiny,
            #regular
            file.output.list,estimate.list,predictScenarios.list,subdata,add_vars,
            scenario_name,scenarioFlag,data_names,scenarioCoefficients) 
          
          ###########Start predictStreamMapsScenarios###
          
          if(!is.na(scenario_map_list[1]) & Rshiny==FALSE){  
            message("Running scenario mapping in batch mode...")
            mapScenarios<-TRUE
            
            dir.create(paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"batch",sep=""))
            save(list = c(as.character(outputSettings(file.output.list,FALSE)$setting),
                          "runScript","run2","file.output.list","scenario.input.list","RSPARROW_errorOption",
                          ls()[which(regexpr("file_",ls())>0)],"estimate.input.list","mapping.input.list","Rshiny","allMetrics","scenarioFlag",
                          "mapScenarios","predictScenarios.list"),
                 file=paste(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"batch.RData",sep=""))
            
            save(list=c("data_names"),
                 file=paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"batch",.Platform$file.sep,"batch.RData",sep=""),compress = FALSE)
            
            system(paste(Sys.which("Rscript.exe")," ",file.path(paste(path_main,.Platform$file.sep,"batch",.Platform$file.sep,"batchMaps.R",sep="")),sep=""), wait = TRUE, invisible = TRUE)
            
            
            unlink(paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"batch",sep=""),recursive=TRUE)
            
          }else{
            mapScenarios<-TRUE
            
            
            if (input$batch!="Batch"){
              output_map_type<-tolower(c(trimws(gsub("-","",input$outType))))
            }
            
            predictMaps(#Rshiny
              input, allMetrics, output_map_type,Rshiny,
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
              batch_mode)
          }
          # }#!is.na(scenario_map_list[1]
          
          #############################################
          #############################################
        }#scenarioError
      }  else { # check on variable names; names do not match
        message(' \nWARNING : No scenarios executed because source variables are not correct in scenario_sources setting\n ')
        if (batch_mode=="yes"){
          cat(' \nWARNING : No scenarios executed because source variables are not correct in scenario_sources setting\n ')
        }
      }#conditions from controlfiletaskmodel
      
    }#initial condition 
    
  }#if predict scenarios 
  
  
}#end function


#'@title checkDrainageareaErrors
#'@description Executes drainage area checks for newly computed areas, based on the 
#'            if_verify_demtarea control setting (section 2 of the control script). If any differences are found 
#'            between the user's original data for Total Drainage Area vs. Total Drainage Area calculated by 
#'            RSPARROW, a plot of user's original data for Total Drainage Area vs. Total Drainage Area calculated 
#'            by RSPARROW is output. For the control setting if_verify_demtarea_maps<-"yes", maps are output 
#'            of `demtarea` and `hydseq` for unmatched areas as a ratio of RSPARROW calculated:original. A CSV 
#'            file is output of all differences found to ~/estimate/(run_id)_diagnostic_darea_mismatches.csv. \\cr \\cr
#'Executed By: verifyDemtarea.R \\cr
#'Executes Routines: \\itemize\{\\item checkBinaryMaps.R
#'             \\item unPackList.R\} \\cr
#'@param DAreaFailCheckObj data.frame of all rows of subdata in which the user's original data 
#'       for Total Drainage Area vs. Total Drainage Area calculated by RSPARROW differ
#'@param data1 input data (data1)
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



checkDrainageareaErrors <- function(file.output.list,mapping.input.list,
                                    #sub1.plot,
                                    DAreaFailCheckObj,data1,
                                    batch_mode) {
  
  unPackList(lists = list(file.output.list = file.output.list),
             parentObj = list(NA)) 
  
  if (length(na.omit(DAreaFailCheckObj$demtarea))!=0){
    filename <- paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_diagnostic_darea_mismatches.pdf",sep="")
    pdf(file=filename)
    
    # plot comparison for mis-matched reaches
    plot(DAreaFailCheckObj$demtarea,DAreaFailCheckObj$demtarea_new,log="xy",
         xlab="Pre-calculated Total Drainage Area",ylab="Newly-calculated Total Drainage Area",
         ,main="Comparison of Total Drainage Areas")
    lines(DAreaFailCheckObj$demtarea,DAreaFailCheckObj$demtarea,col=2)
    
    # Setup variable lists 
    # create global variable from list names (mapping.input.list)
    unPackList(lists = list(mapping.input.list = mapping.input.list),
               parentObj = list(NA))
    
    if (if_verify_demtarea_maps=="yes"){
      #get geoLines
      existGeoLines<-checkBinaryMaps(LineShapeGeo,path_gis,batch_mode)
      
      #get lineShape
      existlineShape<-checkBinaryMaps(lineShapeName,path_gis,batch_mode)
      
      commonvar <- lineWaterid
      
      ##############################################################
      # Loop through variable list
      
      #if (!is.na(LineShapeGeo) & !is.na(lineShapeName)) {  # map if shape files available
      if (existGeoLines==TRUE & existlineShape==TRUE){# map if shape files available
        
        map.vars.list <- c("demtarea","hydseq","hydseq_new","AreaRatio_NewOld")
        title_name <- c("Pre-calculated DEMTAREA","HYDSEQ","new HYDSEQ for unmatched areas","AreaRatio_New:Old")
        #  par(mfrow=c(1,1), pch=16) 
        
        for (k in 1:length(map.vars.list)) {
          
          if(k >= 3) {
            dname <- paste("vvar <- DAreaFailCheckObj$",map.vars.list[k],sep="")
            eval(parse(text=dname)) 
            waterid <- DAreaFailCheckObj$waterid
          } else {
            dname <- paste("vvar <- data1$",map.vars.list[k],sep="")
            eval(parse(text=dname)) 
            waterid <- data1$waterid
          }
          
          # check for NAs
          vvar<-ifelse(is.na(vvar),0.0,vvar)
          
          # link MAPCOLORS for variable to shape object (https://gist.github.com/mbacou/5880859)
          # Color classification of variable
          iprob<-5
          set_unique_breaks <- function(x,ip) {
            chk1 <- quantile(vvar, probs=0:ip/ip)
            chk <- unique(quantile(vvar, probs=0:ip/ip)) # define quartiles
            # exit if the condition is met
            if (length(chk1) == length(chk)) return(ip)
            ip<-ip-1
            Recall(x,ip) # run the function again
          }
          iprob <- set_unique_breaks(vvar,iprob)
          
          if(iprob >=2 ) {
            
            #if(!is.na(LineShapeGeo)) {
            if(existGeoLines==TRUE){
              plot(GeoLines,col=1,lwd=0.1,xlim=lon_limit,ylim=lat_limit)
            }
            
            chk1 <- quantile(vvar, probs=0:iprob/iprob)
            chk <- unique(quantile(vvar, probs=0:iprob/iprob)) # define quartiles
            qvars <- as.integer(cut(vvar, quantile(vvar, probs=0:iprob/iprob), include.lowest=TRUE))  # classify variable
            Mcolors <- c("blue","dark green","gold","red","dark red")
            Mcolors <- Mcolors[1:(length(chk1)-1)]
            # http://research.stowers-institute.org/efg/R/Color/Chart/index.htm
            MAPCOLORS <- as.character(Mcolors[qvars])
            
            dmap <- data.frame(waterid,MAPCOLORS,vvar)
            colnames(dmap) <- c(commonvar,"MAPCOLORS","VVAR")
            dmap$MAPCOLORS <- as.character(dmap$MAPCOLORS)
            
            if(k >= 3) {
              # add background color for matched drainage areas 
              fwaterid <- data1$waterid
              fMAPCOLORS <- rep("grey",length(fwaterid))
              fdf <- data.frame(fwaterid,fMAPCOLORS)
              newdf <- merge(dmap,fdf, by.y = "fwaterid", by.x = commonvar, all.x=TRUE, all.y=TRUE)
              newdf$MAPCOLORS <- ifelse(is.na(newdf$MAPCOLORS),"grey",as.character(newdf$MAPCOLORS))
              
              lineShape <- merge(lineShape, newdf, by.x = commonvar, by.y = commonvar)
            } else {
              lineShape <- merge(lineShape, dmap, by.x = commonvar, by.y = commonvar)
              
            }
            
            #if(!is.na(LineShapeGeo)) {
            if(existGeoLines==TRUE) {
              plot(lineShape,col=lineShape$MAPCOLORS,lwd=0.8, add=TRUE)
            } else {
              plot(lineShape,col=lineShape$MAPCOLORS,lwd=0.8)
            }
            
            title(title_name[k])
            
            if(k >= 3) {
              break1 <- as.character(chk[1:iprob]+1)
              for (i in 1:iprob) {
                break1[i] <- paste(round(chk[i],digit=2)," TO ",round(chk[i+1],digit=2),sep="")
              }
              break1[iprob+1] <- "Areas Match"
              nlty <-rep(1,iprob)
              nlwd <- rep(0.8,iprob)
              Mcol <- length(Mcolors)+1
              Mcol[1:iprob] <- Mcolors[1:iprob]
              Mcol[iprob+1] <- "light grey"
              legend("bottomleft",break1,lty=nlty,cex=0.6,title=title_name[k],
                     bg="grey",lwd=nlwd, col=Mcol, bty="o")
            } else {
              break1 <- as.character(chk[1:iprob])
              for (i in 1:iprob) {
                break1[i] <- paste(round(chk[i],digit=2)," TO ",round(chk[i+1],digit=2),sep="")
              }
              nlty <-rep(1,iprob)
              nlwd <- rep(0.8,iprob)
              legend("bottomleft",break1,lty=nlty,cex=0.6,title=title_name[k],
                     bg="grey",lwd=nlwd, col=Mcolors, bty="o")
            }
            
            drops <- c("MAPCOLORS","VVAR","fMAPCOLORS") # list of col names
            lineShape <- lineShape[,!(names(lineShape) %in% drops)]   # drop MAPCOLORS for re-creation
          }
        }
        
      } # execute if shape files exist
    }
    dev.off()  # shuts down current graphics device
    graphics.off()  # shuts down all open graphics devices
    
    #popup pdf of plot
    shell.exec(filename)
  }#end if all missing original demtarea
  
  # Output mis-matched reach data
  waterid <- DAreaFailCheckObj$waterid
  fnode_pre <- DAreaFailCheckObj$fnode
  tnode_pre <- DAreaFailCheckObj$tnode
  frac_pre <- DAreaFailCheckObj$frac
  demtarea_pre <- DAreaFailCheckObj$demtarea
  demtarea_post <- DAreaFailCheckObj$demtarea_new
  hydseq_new <- DAreaFailCheckObj$hydseq_new
  headflag_new <- DAreaFailCheckObj$headflag_new
  headflag_check <- DAreaFailCheckObj$Headflag_NewOld
  AreaRatio_NewOld <- DAreaFailCheckObj$AreaRatio_NewOld
  
  origWaterid<-data1[,which(names(data1) %in% c("waterid","waterid_for_RSPARROW_mapping"))]
  origWaterid<-origWaterid[which(origWaterid$waterid %in% waterid),]
  origWaterid<-origWaterid[order(match(origWaterid$waterid,waterid)),]
  origWaterid<-origWaterid$waterid_for_RSPARROW_mapping
  
  pout <- data.frame(waterid,origWaterid,fnode_pre,tnode_pre,frac_pre,demtarea_pre,demtarea_post,hydseq_new,
                     AreaRatio_NewOld,headflag_new,headflag_check)
  fileout <- paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_diagnostic_darea_mismatches.csv",sep="")
  write.table(pout,file=fileout,row.names=F,append=F,quote=F,
              dec=csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE)
  
  
}#end function

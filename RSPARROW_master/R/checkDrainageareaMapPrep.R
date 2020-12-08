#'@title checkDrainageareaMapPrep
#'@description Compiles and prepares necessary map data and parameters for checkDrainageErrors 
#'             maps \\cr \\cr
#'Executed By: checkDrainageareaErrorsChild.Rmd \\cr
#'Executes Routines: \\itemize\{\\item named.list.R
#'             \\item unPackList.R\} \\cr
#'@param file.output.list list of control settings and relative paths used for input and output 
#'                        of external files.  Created by `generateInputList.R`
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, polyShapeName,
#'                          ployWaterid, LineShapeGeo, LineShapeGeo, CRStext, 
#'                          convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, if_verify_demtarea_maps
#'@param DAreaFailCheckObj data.frame of all rows of subdata in which the user's original data 
#'                         for Total Drainage Area vs. Total Drainage Area calculated by RSPARROW
#'                        differ
#'@param data1 input data (data1)
#'@param existGeoLines TRUE/FALSE indicating whether the Geolines shape file is present
#'@param commonvar string indicating the column to join the map data and the shapefile
#'@param map.vars.list character vector indicating which checkDrainageErrors maps to generate
#'@param k numeric index for current map
#'@return `prepReturns.list` a named list containing `dmap` the data to map, `break1` vector
#'                           of legend breaks, `nlty` numeric vector indicating line types,
#'                           `nlwd` numeric vector of line widths, `Mcol`character vector of
#'                           unique map colors

checkDrainageareaMapPrep<-function(file.output.list,mapping.input.list,
                          DAreaFailCheckObj,data1,
                          existGeoLines, commonvar, map.vars.list, k){
  
  unPackList(lists = list(file.output.list = file.output.list,
                          mapping.input.list = mapping.input.list),
             parentObj = list(NA,NA)) 

    
    if(k >= 3) {
      dname <- paste0("vvar <- DAreaFailCheckObj$",map.vars.list[k])
      eval(parse(text=dname)) 
      waterid <- DAreaFailCheckObj$waterid
    } else {
      dname <- paste0("vvar <- data1$",map.vars.list[k])
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
      
 
      
      chk1 <- quantile(vvar, probs=0:iprob/iprob)
      chk <- unique(quantile(vvar, probs=0:iprob/iprob)) # define quartiles
      qvars <- as.integer(cut(vvar, quantile(vvar, probs=0:iprob/iprob), include.lowest=TRUE))  # classify variable
      #Mcolors <- c("blue","dark green","gold","red","dark red")
      Mcolors<-predictionMapColors
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
        dmap<-newdf
      }
      
      
      
      if(k >= 3) {
        break1 <- as.character(chk[1:iprob]+1)
        for (i in 1:iprob) {
          break1[i] <- paste0(round(chk[i],digit=2)," TO ",round(chk[i+1],digit=2))
        }
        break1[iprob+1] <- "Areas Match"
        nlty <-rep(1,iprob)
        nlwd <- rep(0.8,iprob)
        Mcol <- length(Mcolors)+1
        Mcol[1:iprob] <- Mcolors[1:iprob]
        Mcol[iprob+1] <- "grey"
      } else {
        break1 <- as.character(chk[1:iprob])
        for (i in 1:iprob) {
          break1[i] <- paste0(round(chk[i],digit=2)," TO ",round(chk[i+1],digit=2))
        }
        nlty <-rep(1,iprob)
        nlwd <- rep(0.8,iprob)
        Mcol<-Mcolors
      }
      
      prepReturns.list<-named.list(dmap,break1, nlty, nlwd, Mcol)

    }else{
      prepReturns.list<-NA
    }

      return(prepReturns.list)
      
    }#end func
#'@title diagnosticSpatialAutoCorr
#'@description Generates diagnostics for spatial dependence (autocorrelation) in the model 
#'            residuals. SiteIDs are determined from hydseq sorted file to ensure consistency in hydrologic 
#'            distance and other ID ordering across programs. Outputs plots to 
#'            ~/estimate/(run_id)_diagnostic_spatialautocor.pdf. Outputs Morans I stats to ~/estimate/(run_id)_diagnostic_spatialautocor.txt. 
#'            Outputs Morans I stats to ~/estimate/summaryCSV/(run_id)_EuclideanMoransI.csv \\cr \\cr
#'Executed By: controlFileTasksModel.R \\cr
#'Executes Routines: \\itemize\{\\item fixDupLatLons.R
#'             \\item unPackList.R\} \\cr
#'@param classvar character vector of user specified spatially contiguous discrete 
#'       classification variables from sparrow_control.  First element is reach classification variable.
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0), ]`
#'@param numsites number of sites selected for calibration
#'@param subdata data.frame input data (subdata)



diagnosticSpatialAutoCorr <- function(file.output.list,classvar,sitedata,numsites,estimate.list,
                                      estimate.input.list,subdata) { 
  
  
  
  #  z = (Moran's I statistic - Moran's Expectation) / sqrt(Variance) = Moran's standard deviate
  
  
  #############################################
  
  
  # transfer required variables to global environment from 'estimate.list$Mdiagnostics.list'
  # create global variable from list names
  # transfer required variables to global environment from 'DataMatrix.list$data.index.list'
  unPackList(lists = list(Mdiagnostics.list = estimate.list$Mdiagnostics.list,
                          estimate.input.list = estimate.input.list,
                          data.index.list = DataMatrix.list$data.index.list,
                          file.output.list = file.output.list),
             parentObj = list(NA,
                              NA,
                              NA,
                              NA))
  
  data <- DataMatrix.list$data
  nreach <- length(data[,1])
  nstas <- sum(ifelse(data[,jdepvar] > 0,1,0))  # jdepvar site load index
  
  # contiguous class variables by sites
  class <- array(0,dim=c(nrow=nrow(sitedata),ncol=length(classvar))) 
  for (k in 1:length(classvar)) { 
    for (i in 1:nrow(sitedata)) {
      class[i,k] <- as.numeric(eval(parse(text=paste("sitedata$",classvar[k],"[",i,"]",sep=""))))
    } 
  }
  
  set.ZeroPolicyOption(TRUE) # setting required for hydrological distance tests
  
  options(width = 200, max.print = 999999)
  
  #############################################
  filename <- paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_diagnostic_spatialautocor.pdf",sep="")
  pdf(file=filename)
  
  par(mfrow=c(1,1), pch=16, cex = 0.6)  # 1 plots on one page
  
  #add text explanation
  par(mfrow=c(1,1)) 
  
  strExplanation<-paste("
    -CDF of Station Hydrological Distances (units of 'length' variable)\n
    -CDF of Station Euclidean Distances (kilometers)\n
    -Four panel plot with Moran's I results by river basin:\n
       -P-value (Euclidean weights)\n
       -Standard deviate (Euclidean weights)\n
       -P-value (Hydrological weights)\n
       -Standard deviate (Hydrological weights)\n
    -Two panel plot with Moran's I results by Class variable and full domain:\n
       -P-value (Euclidean weights)\n
       -Standard deviate (Euclidean weights)
          ")
  gplots::textplot(strExplanation,valign="top", cex=0.8, halign="left")
  title(main=paste(run_id,"_diagnostic_spatialautocor.pdf Document Contents",sep=""))
  
  ##############################################
  nnode <- max(data[,jtnode],data[,jfnode])
  
  # sort file in upstream order
  updata1 <- subdata      # requires 'staidseq' and 'staid'
  updata <- updata1[with(updata1,order(-updata1$hydseq)), ]
  
  # Make site, distance, area upstream transfers
  #  (note:  distance and area are only for hydrologic flow paths; do not include tributary drainage
  #           which would increase incremental area between sites)
  snode <- array(0,dim=nnode)  # 2-digit ID
  stnode <- array(0,dim=nnode) # 8-digit ID
  dnode <- array(0,dim=nnode)  # distance (km)
  anode <- array(0,dim=nnode)  # incremental area
  tanode <- array(0,dim=nnode)  # total area
  dnsite <- numeric(nstas)
  upsite <- numeric(nstas)
  siteid <- numeric(nstas)
  dist <- numeric(nstas)
  area <- numeric(nstas)
  totarea <- numeric(nstas)
  shydseq <- numeric(nstas)
  site_tarea <- array(0,dim=nstas)
  is <- 0
  
  for (k in 1:nreach) {
    tnode <- updata$tnode[k]
    fnode <- updata$fnode[k]
    sitereach <- updata$staidseq[k]    # Station ID value obtained from siteincr.R execution
    
    if (updata$staid[k] > 0) {              # check station sequence number (1-6-2015)
      is <- is+1
      #  store site transition
      dnsite[is] <- snode[tnode]
      siteid[is] <- stnode[tnode]
      upsite[is] <- sitereach
      dist[is] <- dnode[tnode]
      area[is] <- anode[tnode]
      shydseq[is] <- updata$hydseq[k]
      totarea[is] <- tanode[tnode]
      site_tarea[sitereach] <- updata$demtarea[k]  # total area indexed by site ID
      
      #  reset transfer values to current reach containing site
      iarea <- updata$demiarea[k]
      tarea2 <- updata$demtarea[k]
      idist <- updata$length[k] 
      sitereach <- updata$staidseq[k]     # Station ID value obtained from siteincr.R execution
      siteid2 <- updata$staid[k]          # station ID sequence number assigned in hydrologic order
    } else {
      
      #  transfer values to upstream reach
      iarea <- updata$demiarea[k] + anode[tnode]
      tarea2 <- tanode[tnode]
      idist <- (updata$length[k] + dnode[tnode]) * updata$frac[k]
      sitereach <- snode[tnode]
      siteid2 <- stnode[tnode]
    }  # end site check
    
    anode[fnode] <- iarea
    tanode[fnode] <- tarea2
    dnode[fnode] <- idist
    snode[fnode] <- sitereach   # from siteincr.R execution
    stnode[fnode] <- siteid2
  }
  
  #############################################################
  # RESORT BY HYDSEQ to track downstream connections....
  #   run sequentially - no multiple divergences will exist.
  
  sdata <- data.frame(siteid,dnsite,upsite,dist,area,totarea,shydseq)
  sdata <- sdata[with(sdata,order(sdata$shydseq)), ]
  
  #############################################################
  # Create site matrix of hydrologic distances from SITE MATRIX (sdistance is nstas x nstas matrix)
  # track each site fully upstream recording each site and distance found
  
  sdistance <- matrix(0,nrow=nstas,ncol=nstas)
  for (i in 1:nstas) {
    if (sdata$dnsite[i] > 0) {
      dns <- sdata$dnsite[i]
      dnd <- dist[i]
      sdistance[sdata$upsite[i],dns] <- dnd   # record site for tracking downstream
      if (i < nstas) {
        for (j in (i+1):nstas) {
          if (dns == sdata$upsite[j]) {
            dns <- sdata$dnsite[j]
            dnd <- dnd + sdata$dist[j]
            sdistance[sdata$upsite[i],dns] <- dnd   # record next downstream site
          }
        }
      }
    }
  }
  
  # Station linkages in 'sdistance' matrix
  # sdistance[22,] == downstream sites linked to site 22
  # sdistance[,85] == all upstream sites linked to site downstream site 85
  # print site ID indices:  cbind(sitedata$station_name,sitedata$station_id,xstaid)
  
  scount <- numeric(nstas)
  for (i in 1:nstas) {
    for (j in 1:nstas) {
      if(sdistance[j,i] > 0) { 
        scount[i] <- scount[i] + 1   # upstream sites linked with site i
      }
    }
  }
  sum(scount)   # Number of correlations to execute:  527
  sdist <- numeric(sum(scount))
  is <- 0
  for (i in 1:nstas) {
    for (j in 1:nstas) {
      if(sdistance[j,i] > 0) { 
        is <- is+1
        sdist[is] <- sdistance[j,i]
      }
    }
  }
  plot.ecdf(sdist,xlab="Distance between Sites",main="Station Hydrologic Distances")
  
  #####################################
  # obtain station header information  (change subdata to updata1)
  
  staname <- character(nstas)
  ttarea <- numeric(nstas)
  stano <- numeric(nstas)
  shydseq <- numeric(nstas)
  ssta <- numeric(nstas)
  xlon <- numeric(nstas)
  xlat <- numeric(nstas)
  
  is <- 0
  for (i in 1:nreach) {
    if (updata1$staid[i]>0) {
      is <- is+1
      staname[is] <- updata1$station_name[i]
      ttarea[is] <- updata1$demtarea[i]
      stano[is] <- updata1$staid[i]
      shydseq[is] <- updata1$hydseq[i]
      xlon[is] <- updata1$lon[i]
      xlat[is] <- updata1$lat[i]
    }
  }
  index <- rep(1:nstas)
  siteheader <- data.frame(index,ssta,shydseq,stano,staname,ttarea,xlon,xlat)
  
  ##############################################
  options(width = 200) 
  
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
  
  filename <- paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,run_id,"_diagnostic_spatialautocor.txt",sep="")
  sink(file=filename,split="FALSE",append=FALSE)
  
  print(outcharfun("MORAN'S I EUCLIDEAN AND HYDROLOGIC DISTANCE WEIGHTED RESULTS"))
  dname <- "  Inverse distance weight function: "
  dd <- data.frame(dname,MoranDistanceWeightFunc)
  colnames(dd)<-c(" "," ")
  row.names(dd) <- c(" ")
  print(dd)
  
  # Sorted list of stations with upstream station counts (smallest to largest)
  xx <- data.frame(sitedata$station_name,sitedata$station_id,sitedata$staid,scount)
  xx <- xx[with(xx,order(xx$scount,xx$sitedata.staid)), ]
  x1 <- xx[(xx$scount >=  1), ]  # replace subset
  nest_sites <- length(x1$scount)/ length(xx$scount)    # fraction of total sites that are nested 
  
  
  #############################################################
  # Calculate all site correlations between Residuals for hydrologically connected sites
  
  # Extract Residuals into matrix:  mres(nstas), indexed by site number
  mres <- numeric(nstas)
  mbias <- numeric(nstas)
  mobsd <- numeric(nstas)
  myld <- numeric(nstas)
  siteindex <- numeric(nstas)
  for (k in 1:nstas) {
    mres[k] <- Resids[k]
    mbias[k] <- Obs[k] / predict[k]
    mobsd[k] <- Obs[k] 
    myld[k] <- yldobs[k]
  }
  
  ###############################################################
  # obtain Euclidean distance for all station pairs
  Lat <- siteheader$xlat
  Lon <- siteheader$xlon
  
  ##############################
  
  
  Lat <- fixDupLatLons(Lat)  # add small random increment to duplicates
  Lon <- fixDupLatLons(Lon)
  
  #########################################################
  
  edistance <- matrix(0,nrow=nstas,ncol=nstas)
  
  for (i in 1:(nstas-1)) {
    for (j in (i+1):nstas) {
      lat1 <- Lat[i] * pi / 180
      lat2 <- Lat[j] * pi / 180
      lon1 <- Lon[i] * pi / 180
      lon2 <- Lon[j] * pi / 180
      R <- 6371    # radius of the earth in km
      x <- (lon2 - lon1) * cos( 0.5*(lat2+lat1) )
      y <- lat2 - lat1
      edistance[i,j] <- R * sqrt( x*x + y*y )    # Euclidean kilometer distance
    }
  }
  
  edist <- numeric((nstas*nstas-1)/2)    # 2957 sites gives 4,371,924 pairs
  is <- 0
  for (i in 1:nstas) {
    for (j in 1:nstas) {
      if(edistance[j,i] > 0) { 
        is <- is+1
        edist[is] <- edistance[j,i]
      }
    }
  }
  plot.ecdf(edist,xlab="Distance between Sites",main="Station Euclidean Distances (kilometers)")
  
  ############################################################
  
  par(mfrow=c(2,2), pch=16, cex = 0.6)  # 4 plots on one page
  
  #    Moran's I computed separately for each river basin
  
  checkdist <- sdistance
  checkdist <- ifelse(sdistance > 0,1,0)
  checkcount <- scount
  for (j in nstas:1) {      # reverse hydrologic order to identify most downstream site in river basin
    if (scount[j] > 4 & sum(checkdist[,j]) == scount[j] ) {  # minimum of 5 sites gives 10 pairwise comparisons
      checkcount[j] <- scount[j]   # downstream site identifier 
      for (i in 1:nstas) {
        if(checkdist[i,j] > 0) {
          checkdist[i,] <- 0       # zero all matches with this site in the river basin
        }
      }
    } else {
      checkcount[j] <- 0
    }
  }
  xx <- checkcount[checkcount>0]   # replace subset
  
  pmoran <- numeric(length(xx))   
  pmoran_dev <- numeric(length(xx))
  bpmoran <- numeric(length(xx))   
  bpmoran_dev <- numeric(length(xx))
  ind <- rep(1:(length(pmoran)))
  cind <- character(length(ind))
  dsiteid <- numeric(length(xx))
  
  # transfer river basin sites info for Moran test
  #  test executed and reported for only the most downstream site
  ibasin <- 0
  for (j in 1:nstas) {                
    if (checkcount[j] > 0) { # downstream site identified
      ibasin <- ibasin+1
      dsiteid[ibasin] <- j
      is <- 0
      xresids <- numeric(checkcount[j]+1)
      xLat <- numeric(checkcount[j]+1)
      xLon <- numeric(checkcount[j]+1)
      ires <- numeric(checkcount[j]+1)
      
      bdistance <- matrix(0,nrow=checkcount[j]+1,ncol=checkcount[j]+1)
      bres <- numeric(checkcount[j]+1)
      bsites <- numeric(checkcount[j]+1)
      
      #  add the initial downstream site to the vectors
      is <- is+1
      ires[is] <- is
      xresids[is] <- mres[j]
      xLat[is] <- Lat[j]
      xLon[is] <- Lon[j]
      
      bres[is] <- mres[j]
      bsites[is] <- j
      
      for (i in 1:nstas) {
        if (sdistance[i,j] > 0) {    # obtain sites upstream of outlet site j
          is <- is+1
          ires[is] <- is
          xresids[is] <- mres[i]
          xLat[is] <- Lat[i]
          xLon[is] <- Lon[i]
          
          bres[is] <- mres[i]
          bsites[is] <- i
        }
      }
      
      # River basin Euclidean distance weights for Moran's
      xmoran <- data.frame(ires,xresids,xLat,xLon)
      xmoran.dists <- as.matrix(dist(cbind(xmoran$xLon, xmoran$xLat)),method = "euclidean")   # planar coordinates
      
      distance <- xmoran.dists
      xmoran.dists.inv <- eval(parse(text=MoranDistanceWeightFunc))
      diag(xmoran.dists.inv) <- 0
      
      cind[ibasin] <- as.character(j)
      
      # convert w to a row standardised general weights object
      lw <- mat2listw(xmoran.dists.inv)
      lwW <- nb2listw(lw$neighbours, glist=lw$weights, style="W")
      morantest.obj <- moran.test(xmoran$xresids, lwW, alternative="two.sided")    # SPDEP
      
      pmoran[ibasin] <- morantest.obj$p.value
      pmoran_dev[ibasin] <- morantest.obj$statistic
      
      # River basin hydrological distance weights for Moran's 
      bdistance[1:is,1:is] <- sdistance[bsites,bsites]
      # fill-in cross-tabs
      for (i in 1:is) {
        for (k in 1:is) {
          if(bdistance[i,k]==0) {
            bdistance[i,k] <- bdistance[k,i]           
          }
        }
      }
      
      # Hydrologic distance weighting
      distance <- bdistance
      xmoran.dists.inv <- ifelse(!distance==0,eval(parse(text=MoranDistanceWeightFunc)),0)
      diag(xmoran.dists.inv) <- 0
      
      # convert w to a row standardised general weights object (same standardization as used in ape::Moran.I)
      lw <- mat2listw(xmoran.dists.inv)
      lwW <- nb2listw(lw$neighbours, glist=lw$weights, style="W",zero.policy=TRUE)
      
      # mres[1:nstas] residuals
      morantest.obj <- moran.test(bres, lwW, alternative="two.sided",adjust.n=TRUE,na.action=na.exclude,zero.policy=TRUE)    # SPDEP
      
      bpmoran[ibasin] <- morantest.obj$p.value
      bpmoran_dev[ibasin] <- morantest.obj$statistic    
      
    } # end loop for selecting the most downstream site for execution of Moran's
  } # end site loop
  
  
  # Create plots
  
  # Euclidean weighted results
  pmoran <- ifelse(pmoran == 0.0,min(pmoran[pmoran > 0]),pmoran)  # apply minimum non-zero to zero values
  plot(factor(cind),pmoran,main="Moran's I P Value by River Basin",ylab="P Value (Euclidean distance weighting within basin)",
       xlab="River Basin ID Index",las=2,pch=16,ylim=c(0,1))
  abline(h = 0.1, col = "red", lwd = 1) 
  
  plot(factor(cind),pmoran_dev,main="Moran's I Standard Deviate by River Basin",ylab="Standard Deviate (Euclidean distance weighting within basin)",
       xlab="River Basin ID Index",las=2,pch=16)
  abline(h = 0, col = "red", lwd = 1) 
  
  # Hydrological weighted results
  bpmoran <- ifelse(bpmoran == 0.0,min(bpmoran[bpmoran > 0]),bpmoran)  # apply minimum non-zero to zero values
  plot(factor(cind),bpmoran,main="Moran's I P Value by River Basin",ylab="P Value (Hydrologic distance weighting)",
       xlab="River Basin ID Index",las=2,pch=16,ylim=c(0,1))
  abline(h = 0.1, col = "red", lwd = 1) 
  
  plot(factor(cind),bpmoran_dev,main="Moran's I Standard Deviate by River Basin",ylab="Standard Deviate (Hydrologic distance weighting)",
       xlab="River Basin ID Index",las=2,pch=16)
  abline(h = 0, col = "red", lwd = 1) 
  
  
  # River basin text output
  # Obtain list of river basin outlets with significant Moran's I
  x1 <- data.frame(sitedata$station_name,sitedata$station_id,sitedata$staid,scount)
  x2 <- x1[(x1$scount > 0), ]  # replace subset
  
  xx <- data.frame(dsiteid,pmoran,pmoran_dev,bpmoran,bpmoran_dev)
  x2$dsiteid <- x2$sitedata.staid
  sites_sigmoran <- merge(x2,xx,by="dsiteid",all.y=TRUE,all.x=TRUE) 
  
  sites_sigmoran <- sites_sigmoran[(!is.na(sites_sigmoran$pmoran)),]
  sites_sigmoran <- sites_sigmoran[,-1]
  colnames(sites_sigmoran) <- c("Site Name"," Site ID"," Downstrm Site ID"," Upstrm Site Count"," P-Value(E)"," Standard Deviate(E)"," P-Value(H)"," Standard Deviate(H)")
  
  print(outcharfun("RIVER BASIN RESULTS"))
  print(outcharfun(" Euclidean (E) and hydrologic (H) distance weighting (reported for most downstream site in river basins with >= 5 sites)"))
  print(space)
  print(sites_sigmoran)
  print(space)
  
  ################################################################################
  # Full Domain:  Hydrologic channel distance weighting for Moran's I test
  
  for (i in 1:nstas) {
    for (k in 1:nstas) {
      if(sdistance[i,k]==0) {
        sdistance[i,k] <- sdistance[k,i]           
      }
    }
  }
  
  distance <- sdistance
  xmoran.dists.inv <- ifelse(!distance==0,eval(parse(text=MoranDistanceWeightFunc)),0)
  diag(xmoran.dists.inv) <- 0
  
  # convert w to a row standardised general weights object
  lw <- mat2listw(xmoran.dists.inv)
  lwW <- nb2listw(lw$neighbours, glist=lw$weights, style="W",zero.policy=TRUE)
  
  # mres[1:nstas] residuals
  morantest.obj <- moran.test(mres, lwW, alternative="two.sided",adjust.n=TRUE,na.action=na.exclude,zero.policy=TRUE)    # SPDEP
  
  #pmoran <- round(morantest.obj$p.value,digits=4)
  pmoran <- morantest.obj$p.value
  pmoran_dev <- morantest.obj$statistic
  
  print(outcharfun("FULL DOMAIN RESULTS (Hydrologic distance weighting within river basins)"))
  
  moranOut <- data.frame(pmoran,pmoran_dev)
  rownames(moranOut) <- c(" ") 
  colnames(moranOut) <- c(" Moran's P-Value"," Moran's Standard Deviate")
  print(moranOut)
  
  xtext <- paste("  Fraction of sites that are nested:  ",round(nest_sites,digits=3),sep="")
  print(outcharfun(xtext))
  print(space)
  
  
  ################################################################################
  # Moran's I for Euclidean distance within CLASS variable (e.g., HUC-2) and for full domain
  ibasin <- 0
  
  if(!is.na(classvar[1]) & (classvar[1] != "sitedata.demtarea.class")) {   # process only where class variable designated by user
    
    # cycle through regions:  CLASS 
    # Obtain CLASS region numbers
    mrbgrp <- table(class[,1])   # get labels
    xx <- as.data.frame(mrbgrp)  # convert table to dataframe...
    mrbgrp <- as.numeric(xx$Freq)
    xclass <- as.numeric(xx$Var1)   
    xclass <- as.numeric(levels(xx$Var1)[xx$Var1])  # convert factor levels to numeric values
    
    pmoran <- numeric(length(xclass)+1)
    pmoran_dev <- numeric(length(xclass)+1)
    ind <- rep(1:(length(pmoran)))
    cind <- character(length(pmoran))
    cindLabel <- classvar[1]
    
    for (j in 1:length(xclass)) {  
      
      ibasin <- ibasin + 1
      
      # transfer river basin sites info for Moran test          
      
      is <- 0
      xresids <- numeric(mrbgrp[j])
      xLat <- numeric(mrbgrp[j])
      xLon <- numeric(mrbgrp[j])
      ires <- numeric(mrbgrp[j])
      
      for (i in 1:numsites) { 
        if (class[i] == xclass[j]) {   
          is <- is+1
          ires[is] <- is
          xresids[is] <- mres[i]
          xLat[is] <- Lat[i]
          xLon[is] <- Lon[i]
        }
      }
      
      if(is >= 4) {  # only calculate for more than 4 sites
        xmoran <- data.frame(ires,xresids,xLat,xLon)
        xmoran.dists <- as.matrix(dist(cbind(xmoran$xLon, xmoran$xLat)),method = "euclidean")
        distance <- xmoran.dists
        xmoran.dists.inv <- eval(parse(text=MoranDistanceWeightFunc))
        diag(xmoran.dists.inv) <- 0
        
        cind[ibasin] <- as.character(xclass[j])
        
        # convert w to a row standardised general weights object
        lw <- mat2listw(xmoran.dists.inv)
        lwW <- nb2listw(lw$neighbours, glist=lw$weights, style="W")
        morantest.obj <- moran.test(xmoran$xresids, lwW, alternative="two.sided")    # SPDEP
        pmoran[ibasin] <- morantest.obj$p.value
        pmoran_dev[ibasin] <- morantest.obj$statistic
      }
      
    }  # end class loop
    
    ##########################################################################
  }  else {   # case of no designation of class variable
    pmoran <- numeric(1)
    pmoran_dev <- numeric(1)
    cind <- character(1)
    cindLabel <- "Total Area"
  } # end check for designation of class variables by user
  
  # Full spatial domain 
  
  ibasin <- ibasin + 1
  
  # transfer river basin sites info for Moran test          
  
  is <- 0
  xresids <- numeric(numsites)
  xLat <- numeric(numsites)
  xLon <- numeric(numsites)
  ires <- numeric(numsites)
  
  for (i in 1:numsites) { 
    is <- is+1
    ires[is] <- is
    xresids[is] <- mres[i]
    xLat[is] <- Lat[i]
    xLon[is] <- Lon[i]
  }
  
  xmoran <- data.frame(ires,xresids,xLat,xLon)
  xmoran.dists <- as.matrix(dist(cbind(xmoran$xLon, xmoran$xLat)),method = "euclidean")
  distance <- xmoran.dists
  xmoran.dists.inv <- eval(parse(text=MoranDistanceWeightFunc))
  diag(xmoran.dists.inv) <- 0
  
  cind[ibasin] <- "Total Area"
  
  # convert w to a row standardised general weights object
  lw <- mat2listw(xmoran.dists.inv)
  lwW <- nb2listw(lw$neighbours, glist=lw$weights, style="W")
  morantest.obj <- moran.test(xmoran$xresids, lwW, alternative="two.sided")    # SPDEP
  pmoran[ibasin] <- morantest.obj$p.value
  pmoran_dev[ibasin] <- morantest.obj$statistic
  
  ##########################################################################
  # Plot Moran's I by Class variable (e.g., HUC-2)
  pmoran <- ifelse(pmoran == 0.0,min(pmoran[pmoran > 0]),pmoran)  # apply minimum non-zero to zero values
  plot(factor(cind),pmoran,main="Moran's I P Value by CLASS Variable",ylab="Moran's P Value (Euclidean distance weighting)",
       xlab=cindLabel,las=2,pch=16,ylim=c(0,1))
  abline(h = 0.1, col = "red", lwd = 1) 
  plot(factor(cind),pmoran_dev,main="Moran's I Standard Deviate by CLASS Variable",ylab="Moran's Standard Deviate (Euclidean distance weighting)",
       xlab=cindLabel,las=2,pch=16)
  abline(h = 0, col = "red", lwd = 1) 
  
  # output moran's I p values to text file 
  
  if(!is.na(classvar[1]) & (classvar[1] != "sitedata.demtarea.class")) {   # process only where class variable designated by user
    nmrbout <- numeric(length(xclass)+1)
    nmrbout[1:length(mrbgrp)] <- mrbgrp[1:length(mrbgrp)]
    nmrbout[length(mrbgrp)+1] <- sum(mrbgrp)
  } else {
    nmrbout <- numeric(1)
    nmrbout[1] <- numsites
  }
  
  print(outcharfun("REGIONAL AND FULL DOMAIN RESULTS"))
  print(outcharfun(" Euclidean distance weighting (regional results reported for contiguous spatial class variable)"))
  print(space)
  class_sigmoran <- data.frame(cind,nmrbout,pmoran,pmoran_dev)
  colnames(class_sigmoran) <- c(cindLabel," Number Stations"," Moran's P-Value"," Moran's Standard Deviate")
  print(class_sigmoran)
  
  fileCSV<-paste(path_results,.Platform$file.sep,"estimate",.Platform$file.sep,"summaryCSV",.Platform$file.sep,sep="")
  fileout<-paste(fileCSV,"EuclideanMoransI.csv",sep="")
  fwrite(class_sigmoran,file=fileout,row.names=F,append=F,quote=F,showProgress = FALSE,
         dec = csv_decimalSeparator,sep=csv_columnSeparator,col.names = TRUE,na = "NA")
  
  
  sink(type="message")
  sink()
  
  dev.off()  # shuts down current graphics device
  graphics.off()  # shuts down all open graphics devices
  
  
}#end function





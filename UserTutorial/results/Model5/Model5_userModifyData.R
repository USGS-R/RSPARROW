#
# (name)_modifySubdata.R
#
###########################################################
# DATA MODIFICATIONS
###########################################################
# 1. Ensure that the case-sensitive required parameter names ('sparrowNames') 
#    in the betavalues Excel file and global network variables 
#    are defined in this section.

# 2. All new variables need to be stored in 'subdata' object
# 3. Ensure that all 'NAs' are replaced with a zero (NAs in tnoder.for is problematic, causing large RMSE)

###########################################################

  
  ##################################################################
  # set longitude to be consistent with map limits
  if (!is.na(lon_limit)){
    if(lon_limit[1] < 0 & lon_limit[2] < 0) {
      for (i in 1:length(lon)) {  # ensure that longitude is negative
        if(!is.na(lon[i])) {
          lon[i] <- -(abs(lon[i]))
        } 
      }
    }
  }
  ############################################################
  # Update calibration site index 'calsites' beyond default setting
  # 'calsites' initiated with '1' if missing for cases of 'depvar>0'

  # Also place any additional site filtering statements here...
  #  Example: exclude site loads with standard error greater than 50% 
  #  calsites <- ifelse(depvar > 0 & (depvar_se/depvar)*100 > 50,0,calsites)

  # calsites <- ifelse(depvar>0,1,0)   # set default for cases where load>0
  calsites <- Tagsite   # index defined in advanced for MRB3 model
  
  
  ############################################################
  # Define any contiguous classification variables here...

  # Define HUC2 and HUC4 values...
  chk <- character(1)
  for (i in 1:length(huc)) {
    chk <- huc[i]
    if (nchar(chk) == 7) {
      huc[i] <- paste('0',chk,sep="")
    }
  }
  huc2 <- substr(huc,1,2)
  huc8 <- substr(huc,1,8)  
  huc4 <- substr(huc,1,4)  
  
#  table(huc4)           
#  table(huc8)
#  table(huc2)
  
  huc2 <- as.numeric(huc2)
  huc4 <- as.numeric(huc4)
  huc8 <- as.numeric(huc8)
  

  ############################################################
  # Designate each reach as either target or non-target.  The model computes for each non-target 
  #       reach the fraction of flux leaving that reach that is eventually delivered to its nearest 
  #      downstream receiving water body (target reach). */
  target <- ifelse(termflag == 1 | termflag == 3, 1, 0)
  
  # Specify the condition for transfer of load from upstream node to downstream node 
  #    iftran <- ifelse(meanq > 0 | staid > 0,1,0)    #  changed 7-24-2014
  #    iftran <- ifelse(termflag == 3 | termflag == 1,0,iftran)  # no transport beyond stations or along coastal segments
  #    iftran <- ifelse(is.na(iftran),0,iftran)
  
  # Fix cases of NAs (staid, depvar, iftran, demtarea, demiarea)
  replaceNAs(named.list(staid,iftran,demtarea,demiarea))     # replace NAs with zeros
  
  #staid <- ifelse(is.na(staid),0,staid)
  #iftran <- ifelse(is.na(iftran),0,iftran)
  #demtarea <- ifelse(is.na(demtarea),0,demtarea)
  #demiarea <- ifelse(is.na(demiarea),0,demiarea)
  
  # Specify the condition for transfer of load from upstream node to downstream node 
  #    iftran <- ifelse(ifelse(termflag == 3 | termflag == 1,0,1) | meanq > 0 | staid > 0,1,0)
  
  # provide a minimum flow value 
  meanq <- ifelse(meanq <= 0.0,0.1,meanq)
  
  # Define the reach decay variables. 
  # rchdecay1 and rchdecay2 already defined
  # rchdecay1 <- rchdecay1
  # rchdecay2 <- rchdecay2
  
  #    rchtot <- ifelse(rchtot <= 0,rchtot==0.0,rchtot)
  #    rchdecay1 <- ifelse(meanq <= 500 & rchtype == 0,rchtot,0.0)
  #    rchdecay2 <- ifelse(meanq > 500 & meanq <= 10000 & rchtype == 0,rchtot,0.0)
  #    rchdecay3 <- ifelse(meanq > 10000 & rchtype == 0,rchtot,0.0)
  
  #    rchdecay1 <- ifelse(is.na(rchdecay1),0,rchdecay1)
  #    rchdecay2 <- ifelse(is.na(rchdecay2),0,rchdecay2)
  #    rchdecay3 <- ifelse(is.na(rchdecay3),0,rchdecay3)
  
  # Define the reservoir decay variable 
  #  iresload already defined
  
  # iresload <- iresload
  
  #    hload <- subdata$hload
  #    rhload <- ifelse(hload > 0,1.0/hload,0.0)
  #    resdecay <- ifelse(rchtype == 2,rhload,0.0)  # The decay variable RESDECAY is specified as the inverse of the areal 
  #  hydraulic load. - applicable only to reservoir reaches (reachtype = 2)
  
  # Check source and delivery variables for missing values
  
  # Check land use variables for NAs and replace with zeros
  agric <- crops + pasture
  shrubgrass <- shrub + grass
  names <- named.list(agric,forest,grass,shrub,barren,wetlands,urban,shrubgrass)
  replaceNAs(names)
  
  crops <- crops / 1000000
  pasture <- pasture / 1000000

  agric <- agric / 1000000
  forest <- forest / 1000000
  grass <- grass / 1000000
  shrub <- shrub / 1000000
  barren <- barren / 1000000
  wetlands <- wetlands / 1000000
  urban <- urban / 1000000
  shrubgrass <- shrubgrass / 1000000
  
  landUseArea <- agric+forest+grass+shrub+barren+wetlands+urban+shrubgrass 
  urban_percent <- urban / landUseArea *100
  agric_percent <- agric / landUseArea * 100
  crops_percent <- crops / landUseArea * 100
  pasture_percent <- pasture / landUseArea *100
  forest_percent <- forest / landUseArea * 100
  shrubgrass_percent <- shrubgrass / landUseArea * 100
  barren_percent <- barren / landUseArea * 100 
  
  # Apply load reduction factors to the scenario matrix for selected reaches
  #  in cases where the control setting: if_predict_scenarios<-"selected reaches".
  #  The "S_" prefix is required in these cases.
  #  The example applies scenario conditions to all reaches in HUC2 = 5 (Ohio basin)
 # S_point <- ifelse(huc2 == 5,0.5,1)      # point sources
#  S_ndep <- ifelse(huc2 == 5,0.25,1)     # deposition
#  S_FARM_N <- ifelse(huc2 == 5,1.15,1)     # farm fertilizer
#  scenario_sources <- c("urban","crops","pasture")  
  S_urban <- ifelse(huc2 == 7,0.5,1)      # point sources
  S_crops <- ifelse(huc2 == 7,0.25,1)     # deposition
  S_pasture <- ifelse(huc2 == 5,1.15,1)     # farm fertilizer
  
  
  #Site attributes to map
  meanload <- depvar
  meanyield <- depvar / demtarea
  meanconc <- depvar/meanq*ConcFactor
  meanloadSE <- depvar_se/depvar*100
  

  ######## weighted NLLS
  #pre_run_id <- "Model6"

  #nreaches <- length(fnode)
  #weight <- estimateWeightedErrors(file.output.list,run_id,pre_run_id,nreaches,calsites)
  
  
  #x <- abs( rnorm(length(FARM_N)) )
  #MANC_N <- FARM_N
  #####################################################################################



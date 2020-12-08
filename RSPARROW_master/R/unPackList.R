#'@title unPackList
#'@description creates individual variables from list objects and places them in the 
#'            `parent.frame()` environment \\cr \\cr
#'Executed By: \\itemize\{\\item batchMaps.R
#'             \\item batchRun.R
#'             \\item interactiveBatchRun.R
#'             \\item addVars.R
#'             \\item applyUserModify.R
#'             \\item calcClassLandusePercent.R
#'             \\item calcHeadflag.R
#'             \\item calcTermflag.R
#'             \\item checkAnyMissingSubdataVars.R
#'             \\item checkClassificationVars.R
#'             \\item checkDrainageareaErrors.R
#'             \\item checkMissingData1Vars.R
#'             \\item checkMissingSubdataVars.R
#'             \\item controlFileTasksModel.R
#'             \\item correlationMatrix.R
#'             \\item createDataMatrix.R
#'             \\item createDirs.R
#'             \\item createInitialDataDictionary.R
#'             \\item createInitialParameterControls.R
#'             \\item createMasterDataDictionary.R
#'             \\item createVerifyReachAttr.R
#'             \\item dataInputPrep.R
#'             \\item diagnosticMaps.R
#'             \\item diagnosticPlotsNLLS.R
#'             \\item diagnosticPlotsValidate.R
#'             \\item diagnosticSensitivity.R
#'             \\item diagnosticSpatialAutoCorr.R
#'             \\item estimate.R
#'             \\item estimateBootstraps.R
#'             \\item estimateFeval.R
#'             \\item estimateFevalNoadj.R
#'             \\item estimateNLLSmetrics.R
#'             \\item estimateNLLStable.R
#'             \\item estimateOptimize.R
#'             \\item estimateWeightedErrors.R
#'             \\item executeRSPARROW.R
#'             \\item generateInputLists.R
#'             \\item goShinyPlot.R
#'             \\item hydseq.R
#'             \\item hydseqTerm.R
#'             \\item importCSVcontrol.R
#'             \\item mapSiteAttributes.R
#'             \\item modelCompare.R
#'             \\item outputSettings.R
#'             \\item predict.R
#'             \\item predictBoot.R
#'             \\item predictBootsOutCSV.R
#'             \\item predictBootstraps.R
#'             \\item predictMaps.R
#'             \\item predictOutCSV.R
#'             \\item predictScenarios.R
#'             \\item predictScenariosOutCSV.R
#'             \\item predictScenariosPrep.R
#'             \\item predictSensitivity.R
#'             \\item predictSummaryOutCSV.R
#'             \\item read_dataDictionary.R
#'             \\item readData.R
#'             \\item readDesignMatrix.R
#'             \\item readParameters.R
#'             \\item runShiny.R
#'             \\item selectCalibrationSites.R
#'             \\item selectValidationSites.R
#'             \\item setMapDefaults.R
#'             \\item setNLLSWeights.R
#'             \\item setupMaps.R
#'             \\item shinyMap2.R
#'             \\item startModelRun.R
#'             \\item syncVarNames.R
#'             \\item testCosmetic.R
#'             \\item testSettings.R
#'             \\item validateMetrics.R
#'             \\item verifyDemtarea.R\} \\cr
#'@param lists list of variables to unpack into the `parent.frame()`
#'@param parentObj list of parent objects that `lists` are part of, if NA no parent object



unPackList<-function(lists,parentObj, env = parent.frame()){
  for (l in 1:length(lists)){
    sublist<-lists[[l]]
    
    
    if (length(parentObj[[l]])!=1){
      #variables from data frame
      parent<-parentObj[[l]]
      for (i in 1:length(sublist)) {
        dname <- paste0("parent$",sublist[i]) 
        x1name <- paste0(sublist[i])
        if((x1name %in% names(parent)) == TRUE) {
          assign(sublist[i],eval(parse(text=dname)),env = parent.frame())
        }
      }
      
    }else{
      #objects from list
      for(i in 1:length(sublist)){
        tempobj=sublist[[i]]
        assign(names(sublist)[[i]],tempobj,env = parent.frame())
        
      }
    }
    
    
  }
}

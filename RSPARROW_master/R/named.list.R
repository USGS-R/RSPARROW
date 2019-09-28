#'@title named.list
#'@description Supports the creation of a list object by specifying unquoted variable names in 
#'            the argument list. \\cr \\cr
#'Executed By: \\itemize\{\\item batchRun.R
#'             \\item applyUserModify.R
#'             \\item controlFileTasksModel.R
#'             \\item correlationMatrix.R
#'             \\item createDataMatrix.R
#'             \\item createRTables.R
#'             \\item diagnosticSensitivity.R
#'             \\item estimate.R
#'             \\item estimateBootstraps.R
#'             \\item estimateNLLSmetrics.R
#'             \\item estimateOptimize.R
#'             \\item estimateWeightedErrors.R
#'             \\item executeRSPARROW.R
#'             \\item findMinMaxLatLon.R
#'             \\item generateInputLists.R
#'             \\item getVarList.R
#'             \\item makePaths.R
#'             \\item mapBreaks.R
#'             \\item mapSiteAttributes.R
#'             \\item predict.R
#'             \\item predictBoot.R
#'             \\item predictBootstraps.R
#'             \\item predictScenarios.R
#'             \\item predictScenariosOutCSV.R
#'             \\item predictScenariosPrep.R
#'             \\item selectCalibrationSites.R
#'             \\item selectParmValues.R
#'             \\item selectValidationSites.R
#'             \\item setNLLSWeights.R
#'             \\item startModelRun.R
#'             \\item testCosmetic.R
#'             \\item testRedTbl.R
#'             \\item validateMetrics.R\} \\cr



named.list <- function(...) { 
  l <- setNames( list(...) , as.character( match.call()[-1]) ) 
  l
}

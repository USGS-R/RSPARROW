#'@title getYesNoSett
#'@description list of all yes/no control settings \\cr \\cr
#'Executed By: \\itemize\{\\item executeRSPARROW.R
#'             \\item outputSettings.R
#'             \\item runRsparrow.R
#'             \\item testSettings.R\} \\cr
#'@return `yesNoSettings` list of all yes/no control settings



getYesNoSett<-function(){
  yesNoSettings<-c("if_verify_demtarea",
                   "if_reverse_hydseq",
                   "if_mean_adjust_delivery_vars",
                   "if_estimate",
                   "ifHess",
                   "if_spatialAutoCorr",
                   "if_corrExplanVars",
                   "if_validate",
                   "if_boot_estimate",
                   "if_predict",
                   "if_boot_predict",
                   "batch_mode",
                   "if_create_binary_maps",
                   "create_initial_dataDictionary",
                   "create_initial_parameterControlFiles",
                   "if_userModifyData",
                   "edit_Parameters",
                   "edit_DesignMatrix",
                   "edit_dataDictionary",
                   "if_estimate_simulation",
                   "load_previousDataImport",
                   "enable_ShinyApp",
                   "if_verify_demtarea_maps",
                   "RSPARROW_errorOption",
                   "showPlotGrid")
  return(yesNoSettings)
}

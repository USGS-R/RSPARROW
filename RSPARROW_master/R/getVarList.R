#'@title getVarList
#'@description list of all fixed and required variables with explanations \\cr \\cr
#'Executed By: \\itemize\{\\item addVars.R
#'             \\item calcHeadflag.R
#'             \\item calcTermflag.R
#'             \\item checkAnyMissingSubdataVars.R
#'             \\item checkingMissingVars.R
#'             \\item checkMissingData1Vars.R
#'             \\item checkMissingSubdataVars.R
#'             \\item createDataMatrix.R
#'             \\item createInitialDataDictionary.R
#'             \\item createMasterDataDictionary.R
#'             \\item estimateBootstraps.R
#'             \\item estimateNLLSmetrics.R
#'             \\item hydseq.R
#'             \\item hydseqTerm.R
#'             \\item predict.R
#'             \\item predictBoot.R
#'             \\item predictBootsOutCSV.R
#'             \\item predictBootstraps.R
#'             \\item predictOutCSV.R
#'             \\item predictScenariosOutCSV.R
#'             \\item predictSensitivity.R
#'             \\item read_dataDictionary.R
#'             \\item readDesignMatrix.R
#'             \\item readParameters.R
#'             \\item replaceData1Names.R
#'             \\item syncVarNames.R
#'             \\item validateMetrics.R\} \\cr
#'Executes Routines: named.list.R \\cr
#'@return `varOUT` list of all fixed and required variables with explanations



getVarList<-function(){
  
  reqNames <- c("waterid","fnode","tnode","frac","iftran",
                "demiarea","hydseq","termflag",
                "rchtype","calsites")
  
  fixNames <- c("rchname", "demtarea", "length", "headflag", "meanq", "rchtot", 
                "hload", "staid", "station_id", "station_name", "lat", "lon", 
                "depvar", "depvar_se", "target")
  
  matrixlst <- c("waterid","staid","fnode","tnode","frac","iftran","target",
                 "demtarea","demiarea","depvar","hydseq","meanq","calsites")
  
  varList<-c(reqNames,fixNames)
  
  explanations<-structure(list(sparrowNames = structure(c(25L, 16L, 6L, 24L, 
                                                          7L, 11L, 3L, 2L, 13L, 10L, 15L, 17L, 9L, 23L, 22L, 8L, 18L, 19L, 
                                                          20L, 4L, 5L, 12L, 14L, 21L, 1L), .Label = c("calsites", "demiarea", 
                                                                                                      "demtarea", "depvar", "depvar_se", "fnode", "frac", "headflag", 
                                                                                                      "hload", "hydseq", "iftran", "lat", "length", "lon", "meanq", 
                                                                                                      "rchname", "rchtot", "rchtype", "staid", "station_id", "station_name", 
                                                                                                      "target", "termflag", "tnode", "waterid"), class = "factor"), 
                               explanation = structure(c(12L, 15L, 11L, 18L, 25L, 1L, 19L, 
                                                         13L, 14L, 6L, 7L, 17L, 3L, 16L, 24L, 5L, 20L, 10L, 2L, 8L, 
                                                         9L, 21L, 22L, 23L, 4L), .Label = c("\"if transport\" indicator (0=no; 1=yes; nontransport, shoreline segments should be set to 0)", 
                                                                                            "alphanumeric station ID", "areal hydraulic load for reservoirs (e.g., m/yr)", 
                                                                                            "Calibration site index", "headwater reach indicator (1=headwater reach; 0=other reach)", 
                                                                                            "hydrological sequence number (reach order number needed for sorting data1 file)", 
                                                                                            "mean annual streamflow ", "mean load response variable (e.g., kg)", 
                                                                                            "mean load response variable standard error (e.g., % of mean)", 
                                                                                            "numeric station ID (sequence number assigned by user; a new hydrological sequence number is assigned by code)", 
                                                                                            "reach from (upstream) node", "reach ID number", "reach incremental drainage area (e.g., km2)", 
                                                                                            "reach length (e.g., m, km)", "reach name", "reach terminal flag indicator (1=reach; 3=coastal reach)", 
                                                                                            "reach time of travel (e.g., days)", "reach to (downstream) node", 
                                                                                            "reach total drainage area (e.g., km2)", "reach type indicator (0=reach; 1=reservoir internal reach; 2=reservoir outlet reach; 3=coastal segment)", 
                                                                                            "station latitude", "station longitude", "station name", 
                                                                                            "terminal target reach (1=target; 0=non target) for computing load delivery", 
                                                                                            "transport fraction (1.0 indicates no diversion of water/mass)"
                                                         ), class = "factor")), .Names = c("sparrowNames", "explanation"
                                                         ), class = "data.frame", row.names = c(NA, -25L))
  
  varOUT <- named.list(reqNames,fixNames,varList,matrixlst,explanations) 
  return(varOUT)
}

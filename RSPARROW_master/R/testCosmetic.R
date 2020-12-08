#'@title testCosmetic
#'@description test for invalid user inputs in map settings rhandsontables in shiny app \\cr \\cr
#'Executed By: \\itemize\{\\item shinyMap2.R
#'             \\item shinyScenariosMod.R\} \\cr
#'Executes Routines: \\itemize\{\\item compileInput.R
#'             \\item convertHotTables.R
#'             \\item getNumSett.R
#'             \\item getSpecialSett.R
#'             \\item named.list.R
#'             \\item unPackList.R\} \\cr
#'@param input top level interactive user input in Shiny app
#'@param output shiny list object for session output
#'@param session active shiny session
#'@param DF rhandsontable for cosmetic mapping settings in the shiny app
#'@param mapType character string indicating type of map (prediction, siteAttr, scenario)
#'@return `out` named list with bad cosmetic mapping settings and rownumbers for invalid user 
#'            inputs to map settings tables in the shiny app



testCosmetic<-function(input, output, session, DF, mapType, scenario.input.list, mapping.input.list){
  #import namespace
  
  if (mapType!=""){
    if (mapType %in% c("Stream","Catchment")){
      n<-"nsStreamCatch"
    }else if (mapType=="Site Attributes"){
      n<-"nsSiteAttr"
    }else{
      n<-"nsScenarios"
    }
    
    
    #compile hot table
    if (n!="nsScenarios"){
      compiledInput<-callModule(compileInput,n)
    }else{
      compiledInput<-list()
      for (n in names(input)){
        if (!n %in% c("selectAll","clearAll","dropdown")){
          eval(parse(text = paste0("compiledInput$`",n,"`<-input$`",n,"`")))
        }}
    }
    
    compiledInput<-convertHotTables(compiledInput)
    
    
    
    
    #unpack settings
    unPackList(lists = list(scenario.input.list = scenario.input.list,
                            mapping.input.list = mapping.input.list),
               parentObj = list(NA, NA))
    unPackList(lists = list(compiledInput = compiledInput),
               parentObj = list(NA))
    
    #eval map colors
    if (length(which(names(compiledInput)=="predictionMapColors"))!=0){
      predictionMapColors<-eval(parse(text = predictionMapColors))
    }else if (length(which(names(compiledInput)=="siteAttrColors"))!=0){
      siteAttrColors<-eval(parse(text = siteAttrColors))
    }else if (length(which(names(compiledInput)=="scenarioMapColors"))!=0){
      
      scenarioMapColors<-eval(parse(text = scenarioMapColors))
      assign("compiledInput2",compiledInput,envir = .GlobalEnv)
    }
    
    assign("compiledInput_scenarioMapColors",scenarioMapColors,envir = .GlobalEnv)
    #test for bad settings
    badSettings<-as.data.frame(matrix(0,ncol=4,nrow=0))
    names(badSettings)<-c("Setting","CurrentValue","Type","Test")
    
    
    #check that all numeric settings are class= numeric
    numericSettings<-getNumSett()[which(getNumSett() %in% DF$setting)]
    for (s in numericSettings){
      setting<-eval(parse(text = s))
      fail<-paste0(" \nINVALID SETTING : ",s," should be a numeric class\n ")
      # fail<-paste0(s," should be a numeric")
      if (class(setting)=="numeric" | is.na(setting)){
      }else{
        badSet<-data.frame(Setting = s)
        badSet$CurrentValue<-capture.output(dput(setting))
        badSet$Type<-"numeric"
        badSet$Test<-fail
        badSettings<-rbind(badSettings,badSet)
      }
    }
    
    #check special requirements settings
    specialSettings<-as.data.frame(sapply(getSpecialSett(), "[", which(getSpecialSett()$name %in% DF$setting)))
    names(specialSettings)<-names(getSpecialSett())
    
    for (s in 1:length(specialSettings$name)){
      setting<-specialSettings$name[s]
      if (setting=="scenarioMapColors"){
        if (length(scenarioMapColors)==1){
          scenarioMapColors<-eval(parse(text = scenarioMapColors))
        }
      }
      if (setting %in% names(compiledInput)){
        test<-as.character(specialSettings$test[s])
        
        
        
        fail<-(paste0(" \nINVALID SETTING : ",setting," should be meet the required test \n",specialSettings$fail[s],"\n "))
        goodvalue<-eval(parse(text=test))
        goodvalue<-ifelse(is.na(goodvalue),FALSE,goodvalue)
        if (goodvalue){
        }else{
          badSet<-data.frame(Setting = setting)
          CurrentValue<-capture.output(dput(eval(parse(text = setting))))
          if (length(CurrentValue)!=1){
            CurrentValue<-paste(CurrentValue,collapse=",")
          }
          badSet$CurrentValue<-CurrentValue
          badSet$Type<-"special requirements"
          badSet$Test<-fail
          badSettings<-rbind(badSettings,badSet)
        }
      }
    } 
    
    #set rowNums if no cosmetic table
    if (nrow(badSettings)==0){
      rowNums <- -1
    }else{
      rowNums<-which(DF$setting %in% badSettings$Setting)
    }
    
    
    out<-named.list(badSettings,rowNums)
    
    return(out)
  }#if maptype
}

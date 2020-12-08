#'@title testSettings
#'@description test for invalid user inputs in shiny app \\cr \\cr
#'Executed By: executeRSPARROW.R \\cr
#'Executes Routines: \\itemize\{\\item getCharSett.R
#'             \\item getNumSett.R
#'             \\item getOptionSett.R
#'             \\item getShortSett.R
#'             \\item getSpecialSett.R
#'             \\item getYesNoSett.R
#'             \\item unPackList.R\} \\cr
#'@param settings named list of all control settings
#'@param saved binary value indicated whether the user has saved all changes to the 
#'       `sparrow_control.R` file



testSettings<-function(settings,saved){
  if (saved){
    unPackList(lists = list(settings = settings),
               parentObj = list(NA))   
    
    
    if (if_estimate_simulation=="yes"){
      if_validate<-"no"
      assign("if_validate",if_validate,envir = .GlobalEnv)
    }
    
    options(warn=-1)
    #convert all yes/no settings to lowercase and test that all are either yes or no
    #check that settings with specified options do not violate available options
    #check that settings that should have length = 1 have length = 1
    #check that all numeric settings are class= numeric
    
    badSettings<-as.data.frame(matrix(0,ncol=3,nrow=0))
    names(badSettings)<-c("Setting","CurrentValue","Type")
    
    #convert all yes/no settings to lowercase and test that all are either yes or no
    yesNoSettings<-getYesNoSett()
    for (s in yesNoSettings){
      setting<-get(s)
      setting<-tolower(setting)
      if (setting=="yes" |setting=="no"){
        assign(s,setting,envir = .GlobalEnv)
      }else{
        message(paste0(" \nINVALID SETTING : ",s," should have a value of 'yes' or 'no'\n "))
        badSet<-data.frame(Setting = s)
        badSet$CurrentValue<-capture.output(dput(setting))
        badSet$Type<-"yes/no"
        badSettings<-rbind(badSettings,badSet)
      }
    }
    
    #check that all character settings are characters
    charSettings<-getCharSett()
    for (s in charSettings){
      if (exists(s)){
        setting<-get(s)
        if (class(setting)=="character" | is.na(setting)){
        }else{
          message(paste0(" \nINVALID SETTING : ",s," should be a character class\n "))
          badSet<-data.frame(Setting = s)
          badSet$CurrentValue<-capture.output(dput(setting))
          badSet$Type<-"character"
          badSettings<-rbind(badSettings,badSet)
        }
      }
    }
    #check that settings with specified options do not violate available options
    optionSettings<-getOptionSett()
    for (s in optionSettings){
      setting<-s
      setting<-trimws(strsplit(setting,"=")[[1]][2])
      value<-get(trimws(strsplit(s,"=")[[1]][1]))
      badvalue<-value[which(!value %in% eval(parse(text=setting)) & !is.na(value))]
      if (length(badvalue)==0){
      }else{
        message(paste0(" \nINVALID SETTING : ",s," should be selected from ",setting,"\n "))
        badSet<-data.frame(Setting = trimws(strsplit(s,"=")[[1]][1]))
        badSet$CurrentValue<-capture.output(dput(value))
        badSet$Type<-"option"
        badSettings<-rbind(badSettings,badSet)
      }
    }
    
    #check that settings that should have length = 1 have length = 1
    shortSettings<-getShortSett()
    for (s in shortSettings){
      setting<-get(s)
      if (length(setting)==1){
      }else{
        message(paste0(" \nINVALID SETTING : ",s," should have only 1 element\n "))
        badSet<-data.frame(Setting = s)
        badSet$CurrentValue<-capture.output(dput(setting))
        badSet$Type<-"single element"
        badSettings<-rbind(badSettings,badSet)
      }
    }
    
    #check that all numeric settings are class= numeric
    numericSettings<-getNumSett()
    for (s in numericSettings){
      setting<-get(s)
      if (class(setting)=="numeric" | is.na(setting)){
      }else{
        message(paste0(" \nINVALID SETTING : ",s," should be a numeric class\n "))
        badSet<-data.frame(Setting = s)
        badSet$CurrentValue<-capture.output(dput(setting))
        badSet$Type<-"numeric"
        badSettings<-rbind(badSettings,badSet)
      }
    }
    
    #check special requirements settings
    specialSettings<-getSpecialSett()
    for (s in 1:length(specialSettings$name)){
      setting<-specialSettings$name[s]
      test<-specialSettings$test[s]
      fail<-specialSettings$fail[s]
      value<-
        goodvalue<-eval(parse(text=test))
      if (goodvalue){
      }else{
        message(paste0(" \nINVALID SETTING : ",setting," should be meet the required test \n",fail,"\n "))
        badSet<-data.frame(Setting = setting)
        CurrentValue<-capture.output(dput(get(setting)))
        if (length(CurrentValue)!=1){
          CurrentValue<-paste(CurrentValue,collapse=",")
        }
        badSet$CurrentValue<-CurrentValue
        badSet$Type<-"special requirements"
        badSettings<-rbind(badSettings,badSet)
      }
    }
    
    
    
    options(warn=0)
    
    return(badSettings)
    
    
  }#saved
}#end function

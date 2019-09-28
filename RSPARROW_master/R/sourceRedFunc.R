#'@title sourceRedFunc
#'@description create source change functions from rhandsontables in shiny app \\cr \\cr
#'Executed By: goShinyPlot.R \\cr
#'@param shinyInput compiled Shiny input selections
#'@return `out` named list of source change functions based on user input to rhandsontables in 
#'            shiny app



sourceRedFunc<-function(shinyInput){
  if (shinyInput$domain=="all reaches"){
    #get hottable input
    SourceRedAll<-shinyInput$`nsSourceRedALL-hot`
    SourceRedAll<-SourceRedAll[which(!is.na(as.character(SourceRedAll$Source))),]
    
    #set overall settings (non-table settings)
    scenario_factors<-as.numeric(as.character(SourceRedAll$PercentChange))
    scenario_factors<-ifelse(scenario_factors>0,100+scenario_factors,ifelse(scenario_factors==0,100,-1*scenario_factors))
    LanduseConversion<-as.character(SourceRedAll$LanduseConversion)
    LanduseConversion<-ifelse(LanduseConversion=="None",NA,LanduseConversion)
    ChangeCoef<-as.character(SourceRedAll$ChangeCoefficient)
    sourceCoef<-as.character(SourceRedAll$Source)
    
    #output list
    out<-list(scenario_factors = scenario_factors/100,
              scenario_sources = as.character(SourceRedAll$Source),
              landuseConversion = LanduseConversion,
              ChangeCoef = ChangeCoef,
              sourceCoef = sourceCoef)
    
    
  }else if (shinyInput$allSrc=="yes"){
    selectFuncs<-character(0)
    #get all sources and selections
    srcs<-shinyInput$`nsSourceRed-hot`
    
    sels<-shinyInput$`nsAllSources-hot`
    #remove empty rows
    
    #remove rows with no source don't include coefficinet changes
    srcs<-srcs[which(!is.na(srcs$Source)),]
    #remove rows with no selections
    sels<-sels[which(!is.na(sels$SelectionVariable)),]
    
    #loop through srcs
    for (s in 1:nrow(srcs)){
      
      selectFunc<-paste0("S_",as.character(srcs$Source)[s],"<-")
      
      #start ifFunc
      ifFunc<-paste0("ifelse(")
      
      #loop through selection
      totCrit<-character(0)
      
      #reach selection criteria
      for (i in 1:nrow(sels)){
        var<-as.character(sels$SelectionVariable[i])
        mn<-as.numeric(as.character(sels$Min[i]))
        mx<-as.numeric(as.character(sels$Max[i]))
        #equals can be numeric or character
        eq<-ifelse(suppressWarnings(!is.na(as.numeric(as.character(sels$Equals[i])))),
                   as.numeric(as.character(sels$Equals[i])),
                   as.character(sels$Equals[i]))
        lk<-as.character(sels$Like[i])
        seps<-ifelse(as.character(sels$Separator[i])!="",ifelse(as.character(sels$Separator[i])=="OR","|","&"),"")
        
        criteria<-ifelse(!is.na(eq) & class(eq)=="character",paste0(var,"=='",eq,"'"),#equals character
                         ifelse(!is.na(eq) & class(eq)!="character",paste0(var,"==",eq),#equals numeric
                                ifelse(!is.na(lk),paste0("regexpr('",lk,"',",var,")>0"),#like
                                       ifelse(!is.na(mn)& is.na(mx),paste0(var,">=",mn),#min only
                                              ifelse(!is.na(mx) & is.na(mn),paste0(var,"<=",mx),#max only
                                                     paste0("(",mn,"<=",var," & ",var,"<=", mx,")")))))) #min and max
        
        #compile ifelse statments into 1 
        if (nrow(sels)==1){
          totCrit<-criteria
        }else if (i!=nrow(sels)){
          totCrit<-paste0(totCrit,criteria," ",seps," ")
        }else{
          totCrit<-paste0(totCrit,criteria)
        }
        
        #build ifelse
        if (i==nrow(sels)){#end function
          PercentChange<-as.numeric(as.character(srcs$PercentChange[s]))
          PercentChange<-ifelse(PercentChange>0,100+PercentChange,ifelse(PercentChange==0,100,-1*PercentChange))
          
          ifFunc<-paste0(ifFunc,totCrit,",",PercentChange/100,",NA)")
        }
      }
      selectFunc<-paste0(selectFunc,ifFunc)
      selectFuncs<-c(selectFuncs,selectFunc)
      
      
      
    }
    #address landuse conversion (non-table settins)
    LanduseConversion<-as.character(srcs$LanduseConversion)
    LanduseConversion<-ifelse(LanduseConversion=="None",NA,LanduseConversion)
    ChangeCoef<-as.character(srcs$ChangeCoefficient)
    sourceCoef<-as.character(srcs$Source)
    
    #set overall settings (non-table settings)
    scenario_factors<-as.numeric(as.character(srcs$PercentChange))
    scenario_factors<-ifelse(scenario_factors>0,100+scenario_factors,ifelse(scenario_factors==0,100,-1*scenario_factors))
    
    
    out<-list(scenario_sources = as.character(srcs$Source),
              selectFuncs = selectFuncs,
              landuseConversion = LanduseConversion,
              ChangeCoef = ChangeCoef,
              sourceCoef = sourceCoef,
              scenario_factors = scenario_factors/100)
    
  }else{#complex select reaches
    selectFuncs<-character(0)
    lcFuncs<-character(0)
    cfFuncs<-character(0)
    
    #get shiny hottable inputs
    selects<-shinyInput$`nsAllSourcesNO-hot`
    #remove rows with no source
    selects<-selects[which(!is.na(selects$Source)),]
    
    #loop through sources
    for (s in unique(selects$Source)){
      
      src<-selects[which(selects$Source==as.character(s)),]
      if (nrow(src)!=0){
        #set S_ functions
        selectFunc<-paste0("S_",as.character(s),"<-")
        lcFunc<-paste0("S_",as.character(s),"_LC <-")
        cfFunc<-paste0("S_",as.character(s),"_CF <-")
        
        ifFuncs<-character(0)
        ifFuncs_LC<-character(0)
        ifFuncs_CF<-character(0)
        
        uniques<-src[!duplicated(src[c("PercentChange","LanduseConversion","ChangeCoefficient")]),]
        
        #loop through unique "PercentChange","LanduseConversion","ChangeCoefficient"
        for (r in 1:length(uniques$PercentChange)){
          #start ifFunc
          
          ifFunc<-paste0("ifelse(") #reach criteria
          ifFunc_LC<-paste0("ifelse(") #landuseConversion
          ifFunc_CF<-paste0("ifelse(") #change coefficient
          
          #loop through selection
          totCrit<-character(0)
          
          #reduction factors
          reds<-src[which(src$PercentChange==as.character(uniques$PercentChange[r]) & 
                            src$LanduseConversion==as.character(uniques$LanduseConversion[r])
                          & src$ChangeCoefficient==as.character(uniques$ChangeCoefficient[r])),]
          
          
          
          for (i in 1:nrow(reds)){#for each selection variable
            var<-as.character(reds$SelectionVariable[i])
            mn<-as.numeric(as.character(reds$Min[i]))
            mx<-as.numeric(as.character(reds$Max[i]))
            #equals can be numeric or character
            eq<-ifelse(suppressWarnings(!is.na(as.numeric(as.character(reds$Equals[i])))),
                       as.numeric(as.character(reds$Equals[i])),
                       as.character(reds$Equals[i]))
            lk<-as.character(reds$Like[i])
            LU<-as.character(reds$LanduseConversion[i])
            LU<-ifelse(LU=="None",NA,LU)
            
            seps<-ifelse(as.character(reds$Separator[i])!="",ifelse(as.character(reds$Separator[i])=="OR","|","&"),"")
            
            criteria<-ifelse(!is.na(eq) & class(eq)=="character",paste0(var,"=='",eq,"'"),#equals character
                             ifelse(!is.na(eq) & class(eq)!="character",paste0(var,"==",eq),#equals numeric
                                    ifelse(!is.na(lk),paste0("regexpr('",lk,"',",var,")>0"),#like
                                           ifelse(!is.na(mn)& is.na(mx),paste0(var,">=",mn),#min only
                                                  ifelse(!is.na(mx) & is.na(mn),paste0(var,"<=",mx),#max only
                                                         paste0("(",mn,"<=",var," & ",var,"<=", mx,")")))))) #min and max
            
            if (nrow(reds)==1){
              totCrit<-criteria
            }else if (i!=nrow(reds)){
              totCrit<-paste0(totCrit,criteria," ",seps," ")
            }else{
              totCrit<-paste0(totCrit,criteria)
            }
            
            #build ifelse
            if (i==nrow(reds)){#end function
              ifFunc_orig<-ifFunc
              
              PercentChange<-as.numeric(as.character(uniques$PercentChange[r]))
              PercentChange<-ifelse(PercentChange>0,100+PercentChange,ifelse(PercentChange==0,100,-1*PercentChange))
              
              if (as.character(reds$ChangeCoefficient[i])=="no"){
                ifFunc<-paste0(ifFunc,totCrit,",",PercentChange/100)
                if (!is.na(as.character(LU))){
                  ifFunc_LC<-paste0(ifFunc_orig,totCrit,",'",as.character(LU),"'")
                  
                }
              }else{
                ifFunc_CF<-paste0(ifFunc_orig,totCrit,",1")
                
              }
              
              
              
              
            }
            
          }
          if(length(uniques$PercentChange)==1){
            if (as.character(reds$ChangeCoefficient[i])=="no"){
              ifFuncs<-paste0(ifFuncs,ifFunc)
              ifFuncs_LC<-paste0(ifFuncs_LC,ifFunc_LC)
            }else{
              ifFuncs_CF<-paste0(ifFuncs_CF,ifFunc_CF)
              
            }         
            
            
          }else if ((length(ifFuncs)!=0 & as.character(reds$ChangeCoefficient[i])=="no") | (length(ifFuncs_CF)!=0 & as.character(reds$ChangeCoefficient[i])=="yes")){
            if (as.character(reds$ChangeCoefficient[i])=="no"){
              ifFuncs<-paste0(ifFuncs,",",ifFunc)
              
              ifFuncs_LC<-paste0(ifFuncs_LC,",",ifFunc_LC)
            }else{
              ifFuncs_CF<-paste0(ifFuncs_CF,",",ifFunc_CF)
            }
            
          }else{
            
            if (as.character(reds$ChangeCoefficient[i])=="no"){
              ifFuncs<-ifFunc
              
              ifFuncs_LC<-ifFunc_LC
              
            }else{
              ifFuncs_CF<-ifFunc_CF 
            }
            
          }
        }
        #wrap up ifelse statments
        if (length(uniques[which(uniques$ChangeCoefficient=="no"),]$PercentChange)!=0){
          selectFunc<-paste0(selectFunc,ifFuncs,",NA",paste0(rep(")",length(uniques[which(uniques$ChangeCoefficient=="no"),]$PercentChange)),collapse = ""))
          selectFuncs<-c(selectFuncs,selectFunc)
          
          lcFunc<-paste0(lcFunc,ifFuncs_LC,",NA",paste0(rep(")",length(uniques[which(uniques$ChangeCoefficient=="no"),]$PercentChange)),collapse = ""))
        }
        
        cfFunc<-paste0(cfFunc,ifFuncs_CF,",NA",paste0(rep(")",length(uniques[which(uniques$ChangeCoefficient=="yes"),]$PercentChange)),collapse = ""))
        
        
        if (length(src[which(src$LanduseConversion!="None"),]$LanduseConversion)!=0){
          
          
          
          lcFuncs<-c(lcFuncs,lcFunc)
        }
        if (length(src[which(src$ChangeCoefficient!="no"),]$ChangeCoefficient)!=0){
          
          
          cfFuncs<-c(cfFuncs,cfFunc)
        }
        
      }
      
    }#end for source
    scenario_factors<-as.numeric(as.character(selects$PercentChange))
    scenario_factors<-ifelse(scenario_factors>0,100+scenario_factors,ifelse(scenario_factors==0,100,-1*scenario_factors))
    sourceCoef<-as.character(selects$Source)
    out<-list(scenario_sources = as.character(unique(selects$Source)),
              selectFuncs = selectFuncs,
              lcFuncs = lcFuncs,
              cfFuncs = cfFuncs,
              ChangeCoef = as.character(selects$ChangeCoefficient),
              sourceCoef = sourceCoef,
              scenario_factors = scenario_factors/100)
    
  }
  print(out)
  out<-c(shinyInput,out)
  
  
  return(out)
}

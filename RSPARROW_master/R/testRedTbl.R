#'@title testRedTbl
#'@description test for invalid user inputs in source reduction scenario rhandsontables in 
#'            shiny app \\cr \\cr
#'Executed By: shinyMap2.R \\cr
#'Executes Routines: \\itemize\{\\item convertHotTables.R
#'             \\item named.list.R\} \\cr
#'@param input top level interactive user input in Shiny app
#'@param output shiny list object for session output
#'@param session active shiny session
#'@param DF rhandsontable for cosmetic mapping settings in the shiny app
#'@return `out` named list with bad source change scenario settings and rownumbers for invalid 
#'            user inputs to rhandsontables in the shiny app



testRedTbl<-function(input, output, session, DF){
  #convert input to list
  compiledInput<-list()
  for (n in names(input)){
    if (!n %in% c("selectAll","clearAll","dropdown")){
      eval(parse(text = paste0("compiledInput$`",n,"`<-input$`",n,"`")))
    }}
  
  #convert hot tables to data.frame
  compiledInput<-convertHotTables(compiledInput)
  if (any(regexpr("nsScenarios",names(compiledInput))>0)){
    compiledInput<- compiledInput[which(!startsWith(names(compiledInput),"ns") | regexpr("Scenarios",names(compiledInput))>0)]
    names(compiledInput)<-gsub("nsScenarios-","",names(compiledInput))
    
  }
  
  rowNums<-data.frame(row = numeric(0), col = numeric(0))
  errMsg<-NA
  
  
  #test nsSourceRedALL      
  if (compiledInput$domain=="all reaches"){
    SourceRedALL<-compiledInput$`nsSourceRedALL-hot`
  }else if (compiledInput$allSrc=="yes"){
    SourceRedALL<-compiledInput$`nsSourceRed-hot`
  }else if (compiledInput$allSrc=="no"){
    SourceRedALL<-compiledInput$`nsAllSourcesNO-hot`
  }else{
    SourceRedALL<-NULL
    #SourceRedALL<-compiledInput$
  }
  SourceRedALLcomplete<-SourceRedALL
  if (!is.null(SourceRedALL)!=0 & length(which(names(DF)=="Source"))!=0){
    for (r in 1:nrow(SourceRedALLcomplete)){
      SourceRedALL<-SourceRedALLcomplete[r,]
      if (any(!is.na(SourceRedALL$Source))){
        SourceRedALL<-SourceRedALL[which(!is.na(SourceRedALL$Source)),]
        #check dup sources (whole table test)          
        dups<-which(duplicated(SourceRedALLcomplete[c("Source","ChangeCoefficient")]))
        if (length(which(names(SourceRedALL)=="SelectionVariable"))==0 & length(dups)!=0){
          rowNum<-data.frame(row = dups,
                             col =  rep(which(names(SourceRedALL)=="Source"),length(dups)))
          rowNums<-rbind(rowNums,rowNum) 
          errMsg<-"Duplicate 'Source' variables found, define only 1 reduction factor per 'Source'\nor select 'no' in 'Apply same reach selection criteria to all selected sources (yes/no)' to run a Source Change Scenario with multiple 'Percent Change' factors per 'Source'."
          
        }else{
          
          if (any(!is.na(SourceRedALL$PercentChange))){
            SourceRedALL<-SourceRedALL[which(!is.na(SourceRedALL$PercentChange)),]
            if (any(is.na(suppressWarnings(as.numeric(as.character(SourceRedALL$PercentChange)))))){
              rowNum<-data.frame(row = r,
                                 col =  which(names(SourceRedALL)=="PercentChange"))
              rowNums<-rbind(rowNums,rowNum) 
              errMsg<-"'PercentChange' must be numeric.  \nPositive values indicate increase in 'Source', \nnegative values indicate decrease in 'Source', and \nzero indicates no change in 'Source'"
            }
            
            if (any(SourceRedALL$LanduseConversion!="None")){
              if (length(SourceRedALL$LanduseConversion[which(SourceRedALL$LanduseConversion %in% SourceRedALLcomplete$Source)])!=0){
                rowNum<-data.frame(row = r, 
                                   col = which(names(SourceRedALL)=="LanduseConversion"))
                rowNums<-rbind(rowNums,rowNum)
                errMsg<-"'LanduseConversion' selections cannot be 'Source' variables.  Please select different 'LanduseConversions'"
              }else if (length(which(names(SourceRedALL)=="SelectionVariable"))!=0 & SourceRedALLcomplete$ChangeCoefficient=="no"){

                #test multiple landusechange per source-PercentChange-selectionVar combo
                multiLand<-SourceRedALLcomplete[which(SourceRedALLcomplete$LanduseConversion!="None" & SourceRedALLcomplete$ChangeCoefficient=="no"),]
                multiLand$Min<-ifelse(is.na(multiLand$Min),'NA',multiLand$Min)
                multiLand$Max<-ifelse(is.na(multiLand$Max),'NA',multiLand$Max)
                multiLand$Equals<-ifelse(is.na(multiLand$Equals),'NA',multiLand$Equals)
                multiLand$Like<-ifelse(is.na(multiLand$Like),'NA',multiLand$Like)
                
                multiLand<-aggregate(multiLand$LanduseConversion, 
                                     by = list(multiLand$Source, multiLand$SelectionVariable,
                                               multiLand$Min,multiLand$Max,multiLand$Equals,multiLand$Like),function(x) length(unique(x)))
                
                for (s in unique(SourceRedALLcomplete$Source)){
                  noneType<-SourceRedALLcomplete[which(SourceRedALLcomplete$Source==as.character(s) & 
                                                         SourceRedALLcomplete$LanduseConversion=="None" &
                                                         SourceRedALLcomplete$ChangeCoefficient=="no"),]
                  landType<-SourceRedALLcomplete[which(SourceRedALLcomplete$Source==as.character(s) 
                                                       & SourceRedALLcomplete$LanduseConversion!="None" &
                                                         SourceRedALLcomplete$ChangeCoefficient=="no"),]
                  if (nrow(noneType)!=0 & nrow(landType)!=0){
                    rowNum<-data.frame(row =which(SourceRedALLcomplete$Source==as.character(s) & 
                                                    SourceRedALLcomplete$LanduseConversion=="None" &
                                                    SourceRedALLcomplete$ChangeCoefficient=="no"), 
                                       col = rep(which(names(SourceRedALL)=="LanduseConversion"),nrow(noneType)))
                    rowNums<-rbind(rowNums,rowNum)
                    errMsg<-"Source' variables cannot be both Landuse type and not Landuse type (LanduseConversion = 'None').  Please select different 'LanduseConversions'"
                    
                  }
                }#for each unique source
                
                # if (nrow(multiLand)!=nrow(SourceRedALLcomplete)){
                if (length(which(multiLand$x>1))!=0){#multiple landuses found
                  
                  rowNum<-data.frame(row = which(multiLand$x>1), 
                                     col = rep(which(names(SourceRedALL)=="LanduseConversion"),length(which(multiLand$x>1))))
                  
                  rowNums<-rbind(rowNums,rowNum)
                  errMsg<-"Only 1 'LanduseConversion' selection is valid for each 'Source'-'PercentChange'-'SelectionVariable' combination.  Please select different 'LanduseConversions'"
                  #test if landuseConversion both NA and not NA
                }
                
              }#all src=="no"
              if (SourceRedALLcomplete$ChangeCoefficient=="yes"){
                rowNum<-data.frame(row = SourceRedALLcomplete$ChangeCoefficient=="yes", 
                                   col = rep(which(names(SourceRedALL)=="LanduseConversion"),1))
                
                rowNums<-rbind(rowNums,rowNum)
                errMsg<-"'LanduseConversion' selection is invalid for Coefficient Change Scenario, Remove Landuse conversion selection or do not change via coefficient"
                
              }
            }#no landuseconversion
          }else{#no PercentChanges
            rowNum<-data.frame(row = r, 
                               col = which(names(SourceRedALL)=="PercentChange"))
            rowNums<-rbind(rowNums,rowNum)
            errMsg<-"Please select a 'PercentChange' for all 'Source' variables in the Source Change Scenario"
          }
        }#duplicate sources
      }else{#no sources
        rowNum<-data.frame(row = which(is.na(SourceRedALLcomplete$Source)),
                           col=which(names(SourceRedALL)=="Source"))
        
        rowNums<-rbind(rowNums,rowNum)
        if (any(!is.na(SourceRedALLcomplete$Source))){
          errMsg<-NA
        }else{
          errMsg<-"Please select at least 1 'Source' variable to run a Source Change Scenario"
        }
      }
    }#for each row
  }
  
  
  #test selections
  if (compiledInput$domain=="selected reaches"){
    if (compiledInput$allSrc=="yes"){
      selections<-compiledInput$`nsAllSources-hot`
    }else if (compiledInput$allSrc=="no"){
      selections<-compiledInput$`nsAllSourcesNO-hot`
    }else{
      selections<-NULL
    }
  }else{
    selections<-NULL
  }
  
  selectionscomplete<-selections
  if (!is.null(selections) & length(which(names(DF)=="SelectionVariable"))!=0){
    for (r in 1:nrow(selectionscomplete)){
      selections<-selectionscomplete[r,]
      if (any(!is.na(selections$SelectionVariable))){
        selections<-selections[which(!is.na(selections$SelectionVariable)),]
        if (any(!is.na(selections[,which(names(selections) %in% c("Min","Max","Equals","Like"))]))){
          if (length(which(names(selections)=="Source"))==0 & r>1){#allsrc==yes
            if (is.na(selectionscomplete$Separator[r-1])){#must have separator
              rowNum<-data.frame(row = r-1,  
                                 col=which(names(selectionscomplete)=="Separator"))
              rowNums<-rbind(rowNums,rowNum) 
              #whole table test
              if (is.na(errMsg)){ 
                errMsg<-"Please select a criteria 'Separator' for the 'SelectionVariable' variable\n to run a Source Change Scenario with 'selected reaches'"
              }
            }
          }else if (r>1){#allSrc==yes separator test
            if (is.na(selectionscomplete$Separator[r-1]) & selectionscomplete$Source[r-1]==selections$Source & selectionscomplete$PercentChange[r-1]==selections$PercentChange){
              rowNum<-data.frame(row = r-1,  
                                 col=which(names(selectionscomplete)=="Separator"))
              rowNums<-rbind(rowNums,rowNum) 
              #whole table test
              if (is.na(errMsg)){ 
                errMsg<-"Please select a criteria 'Separator' for each new row to run a Source Change Scenario with 'selected reaches'"
              }
            }
          }else if (r>1){#all Src=="no"
            uniques<-selectionscomplete[!duplicated(selectionscomplete[c("Source","PercentChange","LanduseConversion")]),]
            for (u in 1:nrow(uniques)){
              selectionSub<-selectionscomplete[which(selectionscomplete$Source==as.character(uniques$Source)[u] &
                                                       selectionscomplete$PercentChange==as.character(uniques$PercentChange)[u] &
                                                       selectionscomplete$LanduseConversion==as.character(uniques$LanduseConversion)[u]),]
              if (nrow(selectionSub)>1){
                for (rw in as.numeric(as.character(rownames(selectionSub)[1:(length(rownames(selectionSub))-1)]))){
                  if (is.na(selectionSub$Separator)){
                    rowNum<-data.frame(row = rw, 
                                       col=which(names(selections)=="Separator"))
                    rowNums<-rbind(rowNums,rowNum) 
                  }
                  if (is.na(errMsg)){ 
                    errMsg<-"Please select a criteria 'Separator' for each unique 'Source'-'PercentChange'-'LanduseConversion' combination\n to run a Source Change Scenario with 'selected reaches'"
                  }
                }
              }
              
            }# for u in uniques
          }#end all srcs no 
          
        }else{#no criteria
          
          for (c in c("Min","Max","Equals","Like")){
            rowNum<-data.frame(row = r, 
                               col=which(names(selections)==c))
            rowNums<-rbind(rowNums,rowNum) 
          }
          if (is.na(errMsg)){ 
            errMsg<-"Please select at least 1 criteria ('Min','Max','Equals', or 'Like') for the 'SelectionVariable' variable/n to run a Source Change Scenario with 'selected reaches'"
          }
          
        }
      }else{#no selection var
        
        rowNum<-data.frame(row = r,  
                           col=which(names(selectionscomplete)=="SelectionVariable"))
        rowNums<-rbind(rowNums,rowNum)
        if (any(!is.na(selectionscomplete$SelectionVariable))){
          errMsg<-NA
        }else if (is.na(errMsg)){
          errMsg<-"Please select at least 1 'SelectionVariable' variable to run a Source Change Scenario with 'selected reaches'"
        }
        
      }
    }#for each row
    
  }
  
  out<-named.list(rowNums,errMsg)
  
  return(out)
}#end Function

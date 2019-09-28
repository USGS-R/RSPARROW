#'@title convertHotTables
#'@description Converts rhandsontable hottables from Shiny into regular dataframes and 
#'            cosmetic mapping settings to regular list objects in the shiny input list \\cr \\cr
#'Executed By: \\itemize\{\\item goShinyPlot.R
#'             \\item testCosmetic.R
#'             \\item testRedTbl.R\} \\cr
#'@param shinyInput compiled Shiny input selections
#'@return `outshinyInput`  the compiled Shiny Input list with hottables as dataframes and 



convertHotTables<-function(shinyInput){
  
  #get all hottables
  hot<-shinyInput[which(regexpr("hot",names(shinyInput))>0)]
  #remove hottables from shinyInput list
  outshinyInput<-shinyInput[which(regexpr("hot",names(shinyInput))<0)]
  
  #convert hottables to regular list objects
  for (t in names(hot)){
    hottable<-eval(parse(text  = paste0("hot$`",t,"`$data")))
    null.remove <- function(lst) {
      lapply(lst, function(x) {x <- ifelse(is.null(x),NA,x); x})
    }
    miss.remove <- function(lst) {
      lapply(lst, function(x) {x <- ifelse(x=="",NA,x); x})
    }
    hottable <- lapply(hottable, null.remove)
    hottable<- lapply(hottable, miss.remove)
    #for reach selection and source reduction hottables, make into dataframes
    if (regexpr("Cosmetic",t)<0){
      if (length(hottable)>1){ #for multi row hottables
        hottable<-as.data.frame(t(matrix(unlist(hottable),
                                         ncol = length(hottable))))
      }else{#for single row hottables
        hottable<-as.data.frame(matrix(unlist(hottable),
                                       nrow = length(hottable)))
      }
      NAMES<-unlist(eval(parse(text  = paste0("hot$`",t,"`$params$colHeaders"))))
      names(hottable)<-NAMES
      
      eval(parse(text = paste0("outshinyInput$`",t,"`<-hottable")))
      
      
    }else{#for cosmetic mapping settings make each setting a list object
      hottable<-as.data.frame(t(matrix(unlist(hottable),
                                       ncol = length(hottable))))
      
      names(hottable)<-c("setting","settingValue")
      
      for (i in 1:nrow(hottable)){
        if (suppressWarnings(!is.na(as.numeric(as.character(hottable$settingValue[i]))))){
          eval(parse(text = paste0("outshinyInput$",as.character(hottable$setting[i]),"<-as.numeric(as.character(hottable$settingValue[i]))"))) 
        }else{
          eval(parse(text = paste0("outshinyInput$",as.character(hottable$setting[i]),"<-as.character(hottable$settingValue[i])"))) 
        }
      }
    }
    
    
    
  }
  
  return(outshinyInput)
}

#'@title shinyScenarios
#'@description shiny ui for source change scenario mapping \\cr \\cr
#'Executed By: shinyMap2.R \\cr
#'Executes Routines: \\itemize\{\\item dropFunc.R
#'             \\item handsOnUI.R\} \\cr
#'@param id Shiny namespace designation
#'@param input top level interactive user input in Shiny app
#'@param choices data.frame output of function createInteractiveChoices.R



shinyScenarios<-function(id, input, choices){
  #set namespace
  ns<-NS(id)
  
  #set choices for mapping variables
  choices<-choices[which(!choices$category %in% c("Data Dictionary Variable","Prediction Uncertainties") & regexpr("Monitoring-adjusted",choices$definition)<0),]
  ratioChoices<-data.frame(category = c("Relative Change in Load","Relative Change in Load"),
                           variable = c("ratio_total","ratio_inc"),
                           definition = c("Ratio of the changed total load to the baseline (unchanged) total load",
                                          "Ratio of the changed incremental load to the baseline (unchanged) incremental load"))
  choices$category<-ifelse(choices$category=="Load Predictions","Load Predictions for Changed Sources",
                           ifelse(choices$category=="Yield Predictions","Yield Predictions for Changed Sources",choices$category))
  choices<-rbind(choices,ratioChoices)
  
  #start UI
  conditionalPanel(
    condition = "input.mapType == 'Source Change Scenarios'",
    
    ##output map type
    conditionalPanel(
      condition = "input.batch=='Batch'",
      checkboxGroupInput(ns("outCheck"), "Select Output Map Type", 
                         c("Stream","Catchment"),
                         inline=TRUE)),
    conditionalPanel(
      condition = "input.batch!='Batch'",
      selectInput(ns("outType"),"Select output map type",c("Stream","Catchment"))),
    
    #scenario_name 
    fluidRow(
      column(width=9,textInput(ns("scenarioName"), label = "", 
                               scenario_name)),
      column(width=3,checkboxGroupInput(ns("overwriteScenario"), "","Overwrite",
                                        inline=TRUE))
    ),
    
    #select_scenarioReachAreas
    h4("Select Target Reach Watersheds"),
    h6(HTML("<b>\"default\"</b> = run scenario for watersheds above the original outlet reaches (i.e., based on the user-defined terminal reaches for the network)")), 
    h6(HTML("<b>\"waterid1\"</b> or <b>\"waterid1, waterid2, ...\"</b> = run scenario for watersheds above a single or mulitple outlet reach(es), based on the 'waterid' system variable")),
    h6(HTML("<b>\"import\"</b> = run scenario for watersheds above flagged outlet reaches, imported from ~/scenarios/flag_TargetReachWatersheds.csv (with flag = 1 ) ")),
    fluidRow(
      column(width=9,textInput(ns("target"),NULL,"default")),
      column(width=3,actionButton(ns("openTarget"),"OpenFile"))),
    
    selectInput(ns("domain"), "Select reaches for applying the scenario (within targeted watersheds)", c("all reaches","selected reaches")),
    
    conditionalPanel(
      condition = paste0("input['",ns("domain"),"'] == 'selected reaches'"),
      selectInput(ns("allSrc"),"Apply same reach selection criteria to all selected sources (yes/no)",selected = "",c("","yes","no")),
      
      
      conditionalPanel(
        condition = paste0("input['",ns("allSrc"),"']=='yes'"),
        h4("Select Sources and Percent Change Factors"),
        h6("Right click on Row to insert above/below or remove row"),
        handsOnUI(ns("nsSourceRed"),input),
        h4("Reach Selection Criteria"),
        h5("(Reach Selection Criteria applys to all sources)"),
        
        handsOnUI(ns("nsAllSources"),input)
        
      )
      ,#end conditional apply to all sources
      conditionalPanel(
        condition = paste0("input['",ns("allSrc"),"']=='no'"),
        h4("Reach Selection Criteria"),
        h6("Right click on Row to insert above/below or remove row"),
        handsOnUI(ns("nsAllSourcesNO"),input)
        
      )#end conditional apply to all sourcesNO
    ),#end conditional selected reaches
    conditionalPanel(
      condition = paste0("input['",ns("domain"),"'] == 'all reaches'"),
      h4("Select Sources and Percent Change (+/-) Factors"),
      h6("Right click on Row to insert above/below or remove row"),
      handsOnUI(ns("nsSourceRedALL"),input)
    ),
    
    conditionalPanel(
      condition = "input.batch == 'Batch'",
      h5(HTML("<strong>Select Mapping Variables</strong>")),  
      #batch mapping variables
      lapply(1:length(as.character(unique(choices$category))), function(c) {
        category<-as.character(unique(choices$category))[c]
        if (category!="Prediction Uncertainties"){
          nsName<-paste0("nsScen",tolower(str_split(category," ")[[1]][1]),"Drop")
        }else{
          nsName<-"nsuncertaintyDrop"
        }
        dropFunc(nsName,category,choices)
      })
    ),
    
    
    #interactive mapping variable
    conditionalPanel(
      condition = "input.batch != 'Batch'",
      selectInput(ns("mapCategory"), "Mapping Variable Type", c("",as.character(unique(choices$category)))),
      selectInput(ns("var"), "Mapping Variable", c("",as.character(choices$variable))),
      textOutput(ns("definition"))
    ),
    
    #horizontal line
    h5(HTML('<hr style="color: #000000;background-color: #000000; height: 2px"/>')),
    
    h4("Mapping Settings"),
    handsOnUI(ns("nsCosmetic"),input)
    
  )#end wrap all conditional
}

#'@title plotlyLayout
#'@description initiates plotly plot and sets up layout for plot area and axes \\cr \\cr
#'Executed By: \\itemize\{\\item checkDrainageareaErrors.Rmd
#'                        \\item diagnosticClassLandChild.Rmd
#'                        \\item diagnosticClassvarChild.Rmd
#'                        \\item diagnosticContiguousChild.Rmd
#'                        \\item diagnosticCorrChild.Rmd
#'                        \\item diagnosticPlotsNLLS.Rmd
#'                        \\item diagnosticSensitivity.Rmd
#'                        \\item diagnosticSensParamChild.Rmd
#'                        \\item diagnosticSpatialAutoCorr.Rmd} \\cr
#'Executes Routines: named.list.R \\cr
#'@param x data to plot on x-axis
#'@param y data to plot on y-axis
#'@param log character string indicating which axes are to be plotted in log space
#'@param nticks number of tickmarks on axis
#'@param digits number of decimal places to include in axis labels
#'@param xTitle character string for x-axis title
#'@param xZeroLine TRUE/FALSE indicating whether zeroline should be included on x-axis
#'@param xLabs vector of labels for x-axis
#'@param xminTick numeric value indicating lowest value on x-axis
#'@param yTitle character string for y-axis title
#'@param yZeroLine TRUE/FALSE indicating whether zeroline should be included on y-axis
#'@param ymax maximum numeric value to include in data plotted on y-axis
#'@param ymin minimum numeric value to include in data plotted on y-axis
#'@param ymaxTick maximum value for y-axis labels
#'@param plotTitle character string for title of plot
#'@param legend TRUE/FALSE indicated whether legend should be included
#'@param showPlotGrid yes/no setting controlling whether gridlines are displayed
#'@return `p` plotly plot object with custom layout


plotlyLayout<-function(x, y, log, nTicks, digits, 
                       xTitle, xZeroLine,xLabs = NA, xminTick = NA,
                       yTitle, yZeroLine,ymax = NA,ymin = NA, ymaxTick = NA,
                       plotTitle,legend,showPlotGrid){
  showPlotGrid<-ifelse(showPlotGrid=="yes","TRUE","FALSE")
  nTicksOrig<-nTicks
  #format tickmarks
  for (a in c("x","y")){
    nTicks<-nTicksOrig
    aData<-eval(parse(text = a))
    
    if (a=="y"){
      if (!is.na(ymax)){
        aData<-c(aData,ymax)
      }
      if (!is.na(ymin)){
        aData<-c(aData,ymin)
      }
      
    }
    if ((!is.na(ymaxTick) & a=="y") | (!is.na(xminTick) & a=="x")){
      nTicks<-nTicks-1
    }
    
    if (!is.na(aData)){
      #test if scientific notation is needed
      if (min(aData, na.rm = TRUE)>1000 | max(aData,na.rm = TRUE)>100000){
        sciNote<-TRUE
      }else{
        sciNote<-FALSE
      }
      
      if (regexpr(a,log)>0){
        ticks<-axisTicks(log10(range(aData, na.rm = TRUE)), log = TRUE, nint = nTicks)
        if (sciNote){
          ticksLab<-formatC(ticks, format = "e", digits = digits) 
        }else{
          ticksLab<-as.character(ticks)
        }
        
        type<-"type = 'log',"
      }else{
        ticks<-axisTicks(range(aData, na.rm = TRUE), log = FALSE, n = nTicks) 
        ticksLab<-as.character(ticks)
        type<-""
      }
      
      if (!is.na(xminTick)){
        ticks<-c(xminTick,ticks)
        ticksLab<-as.character(ticks)
      }
      if (!is.na(ymaxTick)){
        ticks<-c(ticks,ymaxTick)
        ticksLab<-as.character(ticks)
      }
      
      strTitle<-eval(parse(text = paste0(a,"Title")))
      strZeroLine<-eval(parse(text = paste0(a,"ZeroLine")))
      
      
      #create axis attribute list
      eval(parse(text = paste0(a,"Axis.list <- list(",type,
                               "tickvals = ticks, 
                                                  ticktext = ticksLab,
                                                  showline = TRUE,
                                                  ticks = 'outside',
                                                  title = strTitle,
                                                  zeroline = strZeroLine,
                                                  showgrid =", showPlotGrid,",
                             yref = 'paper',y=0, titlefont = list(size = 11))"))) 
    }else{
      if (is.na(xLabs)){
        eval(parse(text = paste0(a,"Axis.list <- list(showticklabels = FALSE,
                                                     showgrid =", showPlotGrid,",
                                                     showline = TRUE)"))) 
      }else{
        strTitle<-eval(parse(text = paste0(a,"Title")))
        strZeroLine<-eval(parse(text = paste0(a,"ZeroLine")))
        eval(parse(text = paste0(a,"Axis.list <- list(type = 'category',
                                                  categoryoder = 'category ascending',
                                                  categoryarray = xLabs,
                                                  showline = TRUE,
                                                  ticks = 'outside',
                                                  title = strTitle,
                                                  zeroline = strZeroLine,
                                                  showgrid =", showPlotGrid,",
                             yref = 'paper',y=0, titlefont = list(size = 11))"))) 
      }
      
    }
  }
  
  a <- list(
    text = paste0("<b>",plotTitle,"</b>"),
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1.05,
    showarrow = FALSE,
    font = list(size = 14)
  )
  
  
  
  #initiate plotly plot
  p<-plot_ly()
  
  
  if (exists("xAxis.list")){  
    p <- p %>% layout(xaxis = xAxis.list,
                      yaxis = yAxis.list,
                      showlegend = legend,
                      annotations = a,
                      margin = list(t=100))
  }else{
    p <- p %>% layout(
      yaxis = yAxis.list,
      showlegend = legend,
      annotations = a,
      margin = list(t=100))
  }
  
  return(p)
}
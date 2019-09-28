library(shiny)
library(visNetwork)

ui <- shinyUI(fluidPage(
  visNetworkOutput("net")
))

server <- shinyServer(function(input, output) {
  
  output$net <- renderVisNetwork({
    set.seed(125)
    nodes <- data.frame(id = 1:15, label = paste("Label", 1:15), title = paste("Label", 1:15),
                        group = sample(LETTERS[1:3], 15, replace = TRUE))
    
    edges <- data.frame(from = trunc(runif(15)*(15-1))+1,
                        to = trunc(runif(15)*(15-1))+1)
    
    # using your own css
    # visNetwork(nodes, edges) %>% 
    #   visInteraction(tooltipStyle = "position: fixed;visibility:hidden;text-decoration: underline;")
    
    # default css + text-decoration
    visNetwork(nodes, edges) %>% 
      visOptions(manipulation = TRUE)
    
  })
  
  observe({
    print(input$net_graphChange)
  })
})

# Run the application 
shinyApp(ui = ui, server = server)


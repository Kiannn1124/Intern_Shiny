library(shiny)
library(ggplot2)

ui=fluidPage(tags$h3("R Shiny Assignment"),
             sidebarPanel(theme = "united",
             fileInput("file", "Upload Your CSV file here"),
             sliderInput("TF", "Tail Factor", value=1.1, min=0, max=10, step=0.1)),
             tabsetPanel(tabPanel("Cumulative Paid Claims ($) - Table",tableOutput("table")),
             tabPanel("Cumulative Paid Claims ($) - Graph",plotOutput("plot"))))

server=function(input, output, session){
  output$plot=renderPlot({
    
  })
}
  


shinyApp(ui, server)

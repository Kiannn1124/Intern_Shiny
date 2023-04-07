library(shiny)
library(ggplot2)

ui=fluidPage(sidebarPanel(theme = "united",
             fileInput("file", "Upload Your CSV file here"),
             sliderInput("TF", "Tail Factor", value=1.1, min=0, max=10, step=0.1)),
             mainPanel(tableOutput("table"),
             plotOutput("plot")))

server=function(input, output, session){
  output$
}
  


shinyApp(ui, server)

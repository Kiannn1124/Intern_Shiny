library(shiny)
library(ggplot2)

ui=fluidPage(tags$h3("R Shiny Assignment"),
             sidebarPanel(theme = "united",
             fileInput("file", "Upload Your CSV file here"),
             sliderInput("TF", "Tail Factor", value=1.1, min=1, max=3, step=0.1)),
             tabsetPanel(tabPanel("Cumulative Paid Claims ($) - Table",tableOutput("mytable")),
             tabPanel("Cumulative Paid Claims ($) - Graph",plotOutput("myplot"))))

server=function(input, output, session){
   data_hehe=reactive({
     req(input$file)
     read.csv(input$file$datapath)
   })
   calculation_hehe=reactive({
     req(data_hehe())
     Loss_Year=data_hehe()[c(1,4,6),1]
     Dev1=data_hehe()[c(1,4,6),3]
     Dev2=c(sum(data_hehe()[1:2,3]), sum(data_hehe()[4:5,3]), ((sum(data_hehe()[4:5,3]))/(sum(data_hehe()[1:2,3])))*data_hehe()[6,3])
     Dev3=c(sum(data_hehe()[1:3],3), (sum(data_hehe()[1:3],3))/(sum(data_hehe()[1:2,3]))*sum(data_hehe()[4:5,3]), (sum(data_hehe()[1:3],3))/(sum(data_hehe()[1:2,3]))*((sum(data_hehe()[4:5,3]))/(sum(data_hehe()[1:2,3])))*data_hehe()[6,3])
     Dev4=c(sum(data_hehe()[1:3],3)*input$TF, (sum(data_hehe()[1:3],3))/(sum(data_hehe()[1:2,3]))*sum(data_hehe()[4:5,3])*input$TF, (sum(data_hehe()[1:3],3))/(sum(data_hehe()[1:2,3]))*((sum(data_hehe()[4:5,3]))/(sum(data_hehe()[1:2,3])))*data_hehe()[6,3]*input$TF)
     table_hehe=data.frame(Loss_Year, Dev1, Dev2, Dev3, Dev4)
     
     return(table_hehe)
   })
   output$mytable=renderTable({
     calculation_hehe()
   })
}
  


shinyApp(ui, server)











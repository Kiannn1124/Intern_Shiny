library(shiny)
library(ggplot2)
library(reshape2)

ui=fluidPage(tags$h3("R Shiny Assignment"),
             sidebarPanel(theme = "united",
             fileInput("file", "Upload Your CSV file here"),
             sliderInput("TF", "Tail Factor", value=1.1, min=1, max=3, step=0.1)),
             tabsetPanel(tabPanel("Claims Paid Data - User Input", tableOutput("myinput")),
             tabPanel("Cumulative Paid Claims ($) - Table",tableOutput("mytable")),
             tabPanel("Cumulative Paid Claims ($) - Graph",plotOutput("myplot"))))

server=function(input, output){
   data_hehe=reactive({
     req(input$file)
     read.csv(input$file$datapath, header=TRUE, stringsAsFactors = FALSE)
   })
   base_year=reactive({
     min(data_hehe()$Loss_Year)
   })
   tail_f=reactive({input$TF})
   
   calculation_hehe=reactive({
     
     result_table_hehe=data.frame(Loss_Year=unique(data_hehe()$Loss_Year), stringsAsFactors = FALSE)
     
     for (i in 1:nrow(result_table_hehe)){
       lossyear=result_table_hehe$Loss_Year[i]
       
       Dev1=sum(data_hehe()$Amount_of_Claims_Paid[data_hehe()$Loss_Year == lossyear & data_hehe()$Development_Year == 1])
       Dev2=sum(data_hehe()$Amount_of_Claims_Paid[data_hehe()$Loss_Year == lossyear & data_hehe()$Development_Year %in% c(1,2)])
       Dev3=sum(data_hehe()$Amount_of_Claims_Paid[data_hehe()$Loss_Year == lossyear & data_hehe()$Development_Year %in% c(1,2,3)])
       Dev4=sum(data_hehe()$Amount_of_Claims_Paid[data_hehe()$Loss_Year == lossyear & data_hehe()$Development_Year %in% c(1,2,3,4)])
      result_table_hehe[i, c("Dev1", "Dev2", "Dev3", "Dev4")]=c(Dev1, Dev2, Dev3, Dev4)
     }
     result_table_hehe$Dev2[result_table_hehe$Loss_Year == base_year() + 2] = 
       (sum(result_table_hehe$Dev2[result_table_hehe$Loss_Year %in% c(base_year(), base_year() + 1)]) / 
          sum(result_table_hehe$Dev1[result_table_hehe$Loss_Year %in% c(base_year(), base_year() + 1)])) *
       result_table_hehe$Dev1[result_table_hehe$Loss_Year == base_year() + 2]
     
     result_table_hehe$Dev3[result_table_hehe$Loss_Year == base_year() + 1] =
       (sum(result_table_hehe$Dev3[result_table_hehe$Loss_Year == base_year()]) / 
          sum(result_table_hehe$Dev2[result_table_hehe$Loss_Year == base_year()])) *
       result_table_hehe$Dev2[result_table_hehe$Loss_Year == base_year() + 1]
     
     result_table_hehe$Dev3[result_table_hehe$Loss_Year == base_year() + 2] = 
       (sum(result_table_hehe$Dev3[result_table_hehe$Loss_Year == base_year() + 1]) / 
          sum(result_table_hehe$Dev2[result_table_hehe$Loss_Year == base_year() + 1])) *
       result_table_hehe$Dev2[result_table_hehe$Loss_Year == base_year() + 2]
     
  
       result_table_hehe$Dev4[result_table_hehe$Loss_Year == base_year()] = result_table_hehe$Dev3[result_table_hehe$Loss_Year == base_year()] * tail_f()
       result_table_hehe$Dev4[result_table_hehe$Loss_Year == base_year() + 1] = result_table_hehe$Dev3[result_table_hehe$Loss_Year == base_year() + 1] * tail_f()
       result_table_hehe$Dev4[result_table_hehe$Loss_Year == base_year() + 2] = result_table_hehe$Dev3[result_table_hehe$Loss_Year == base_year() + 2] * tail_f()
       
       return(result_table_hehe)
     
     
   })
   
   output$mytable=renderTable({
    calculation_hehe_rounded = format(round(calculation_hehe(),digits=0), scientific = FALSE)
    return(calculation_hehe_rounded)
   })
   output$myplot=renderPlot({

       data_long = melt(calculation_hehe(), id.vars = "Loss_Year",
                         variable.name = "Development_Year", 
                         value.name = "Cumulative_Paid_Claim")
       
   
       ggplot(data_long, aes(x = Development_Year, y = Cumulative_Paid_Claim, 
                             group = Loss_Year, color = Loss_Year)) +
         geom_line() + 
         geom_point(size = 2)+
         labs(title = "Cumulative Paid Claims ($) - Graph",
              x = "Development Year",
              y = "Cumulative Paid Claim") +
         theme_bw()
     })
     
  
   output$myinput=renderTable({
     data_hehe()
   })
}


shinyApp(ui, server)










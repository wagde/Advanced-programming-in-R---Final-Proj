library(shiny)
source("helpers.R")

ui<-fluidPage(titlePanel( "The R Olympic Medal Viewer") ,selectInput(inputId = "Country",label = "Country",
      choices = c("Australia", "China", "Spain" ,"France","United Kingdom" ,"Germany" ,
        "South Korea"  ,"Netherlands" ,"Russia" ,"United States")),sliderInput(inputId = "year",
label = "Starting year",value=1896,min=1896,max=2008  ),sliderInput(inputId = "year1",
   label = "endyear year",value=2008,min=1896,max=2008),checkboxGroupInput(inputId = "aa",label = "Medals",
    choices = c("TOTAL","GOLD","SILVER","BRONZE"),selected =c("TOTAL","GOLD","SILVER","BRONZE") ),
      plotOutput("ploto")  )
 

server<-function(input,output){
  output$ploto <- renderPlot({ PLOT.MEDAL.2(input$Country,input$year,input$year1,input$aa)
  })
}
shinyApp(ui=ui,server = server)

 


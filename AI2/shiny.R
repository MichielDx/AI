shiny = function(){
  library(shiny)
  library("ggplot2")
  library("scales")
  
  if(!exists("main", mode="function")) source("C:/Users/Michiel/AI/AI2/program.R")
  if(!exists("secondsToTimestamp", mode="function")) source("C:/Users/Michiel/AI/AI2/program.R")
  measurements <- main()
  
  ui <- fluidPage(
    headerPanel(""),
      fluidRow(
        h3("Temperature k-means clustering", align = "center"),
        column(12,numericInput('kmeansClusters', 'K-means clusters', 0,min = 0, max = 9), align="center"),
        column(12,plotOutput('kmeans'))
      ),
    fluidRow(
      h3("Temperature canopy clustering", align = "center"),
      column(12,numericInput('canopyClusters', 'Canopy clusters', 0,min = 0, max = 9),align="center"),
      column(12,plotOutput('canopy'))
      ),
    fluidRow(
        h3("Temperature X-means clustering", align = "center"),
        plotOutput('xmeans')
      )    ,
    fluidRow(
      h3("Temperature self organizing map clustering", align = "center"),
      plotOutput('kohonen')
    )
    
  )
  server <- function(input, output, session) {
      output$kmeans <- renderPlot({
        km<-kmean(measurements, input$kmeansClusters)
        Timestamp <- measurements$Timestamp
        Temperature <- measurements$Temperature
        km$cluster <- as.factor(km$class_ids)
        temp<- ggplot(measurements, aes(Timestamp, Temperature, color = km$cluster)) + geom_point() + scale_x_datetime(labels = date_format("%H:%M:%S"))
        print(temp)
      })
      
      output$canopy <- renderPlot({
        can<-canopy(measurements, input$canopyClusters)
        Timestamp <- measurements$Timestamp
        Temperature <- measurements$Temperature
        can$cluster <- as.factor(can$class_ids)
        temp<- ggplot(measurements, aes(Timestamp, Temperature, color = can$cluster)) + geom_point()
        print(temp)
      })
      
      output$xmeans <- renderPlot({
        xm<-xmeans(measurements)
        Timestamp <- measurements$Timestamp
        Temperature <- measurements$Temperature
        xm$cluster <- as.factor(xm$class_ids)
        temp<- ggplot(measurements, aes(Timestamp, Temperature, color = xm$cluster)) + geom_point()
        print(temp)
      })
      
      output$kohonen <- renderPlot({
        koh<-kohonen(measurements)
        Timestamp <- measurements$Timestamp
        Temperature <- measurements$Temperature
        koh$cluster <- as.factor(koh$class_ids)
        temp<- ggplot(measurements, aes(Timestamp, Temperature, color = koh$cluster)) + geom_point()
        print(temp)
      })
  }
  shinyApp(ui, server)
}

shiny()
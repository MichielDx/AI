shiny = function(tree){
  library(shiny)
  library(leaflet)
  
  r_colors <- rgb(t(col2rgb(colors()) / 255))
  names(r_colors) <- colors()
  
  ui <- fluidPage(
    titlePanel(NULL),
    sidebarLayout(
      sidebarPanel(
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        tags$hr(),
        checkboxInput('header', 'Header', FALSE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '')
      ),
      mainPanel(
        tableOutput('contents'),
        leafletOutput("mymap")
      )),
    sidebarLayout(
      sidebarPanel(
        h4("Determine the route closest to your route by using the decision tree."),
        actionButton("submit","Go!"),
        tags$style(type='text/css', "#submit { vertical-align: middle; height: 50px; width: 100%; font-size: 30px;}"),
        h4(textOutput("name"))
      ),
      mainPanel(
        leafletOutput("treemap")
      )
    )
  )
  
  
  
  server <- function(input, output, session) {
    wd <- getwd()
    points <- data.frame()
    name <- ""
    output$mymap <- renderLeaflet({
      inFile <- input$file1
      if (is.null(inFile)){
        return(leaflet() %>%
                 addTiles())
      }
      points<-read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                       quote=input$quote)
      points <<- points
      colnames(points) <- c("lat","lon")
      lon <- c(points$lon[1],points$lon[nrow(points)])
      lat <- c(points$lat[1],points$lat[nrow(points)])
      leaflet() %>%addTiles()%>%
        addPolylines(lng = points$lon, lat=points$lat) %>% addMarkers(lon,lat, popup = as.character(c("Start","End")))
    })
    
    output$treemap <- renderLeaflet({
      leaflet() %>% addTiles()
    })
    observeEvent(input$submit, {
      points <- interpolateRoute(points)
      points <- calculateValues(points)
      points <- transformFrame(points,"")
      prediction <- predict(tree,points)
      output$name <- renderText({
        paste("The most similiar route is ",prediction[[1]])
      })
      route <- read.csv(paste(wd,"/",prediction[1],".csv",sep=""),header=FALSE)
      lon <- c(route$V2[1],route$V2[nrow(route)])
      lat <- c(route$V1[1],route$V1[nrow(route)])
      output$treemap <- renderLeaflet({
        leaflet() %>% addTiles()%>%
        addPolylines(lng = route$V2, lat=route$V1) %>% addMarkers(lon,lat, popup = as.character(c("Start","End")))
      })
      })
    
    
  }
  shinyApp(ui, server)
}
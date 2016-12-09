readFile <- function(file){
  route <- read.csv(file,header=FALSE)
  return(route)
}

interpolateRoute <- function (route){
  longitude<-approx(route$V1,n=100,method = "linear")$y
  latitude<-approx(route$V2,n=100,method = "linear")$y
  route2<-data.frame(longitude,latitude)
  return(route2)
}

interpolateTestRoute <- function (route){
  longitude<-approx(route$lon,n=100,method = "linear")$y
  latitude<-approx(route$lat,n=100,method = "linear")$y
  route2<-data.frame(longitude,latitude)
  return(route2)
}

calculateDistance = function(lon2,lat2,lon1,lat1){
  toRadians = function(deg) {(deg * pi) / (180)}
  R = 6371e3;
  sigma1 = toRadians(lat1);
  sigma2 = toRadians(lat2);
  delta1 = toRadians(lat2-lat1);
  delta2 = toRadians(lon2-lon1);
  a = sin(delta1/2) * sin(delta1/2) +  cos(sigma1) * cos(sigma2) *  sin(delta2/2) * sin(delta2/2);
  c = 2 * atan2(sqrt(a), sqrt(1-a));
  d = R * c
  return(d)
}

calculateBearings = function(lon2,lat2,lon1,lat1){
  toDegrees = function(rad) {(rad *180) / (pi)}
  y = sin(lon2-lon1) * cos(lat2);
  x = cos(lat1)* sin(lat2) - sin(lat1)* cos(lat2)* cos(lon2-lon1);
  bearing =  toDegrees(atan2(y, x));
  return(bearing)
}

calculateValues <- function(route){
  size<-nrow(route)
  for(i in 1:size){
    route$distance[i]<-calculateDistance(route$longitude[i], route$latitude[i], route$longitude[i+1], route$latitude[i+1])
    route$bearing[i] <- calculateBearings(route$longitude[i], route$latitude[i], route$longitude[i+1], route$latitude[i+1])
  }
  route$distance[size]=0
  route$bearing[size]=0
  return(route)
}

getName <- function(file){
  name <- strsplit(file,"\\\\")
  name <- as.list(name[[1]])
  name <- name[length(name)]
  name <- strsplit(name[[1]],'\\.')
  name <- as.list(name[[1]])[[1]]
  return(name)
}

dataset <- function(file){
  temp <- readFile(file)
  temp<-interpolateRoute(temp)
  return(temp)
}

values <- function (route){
  temp<-calculateValues(route)
  return(temp)
}

transformFrame = function(dataframe,fileName){
  values = dataframe[1,]
  for (i in 2:nrow(dataframe)) {
    vector = dataframe[i,]
    values = append(values,vector)
  }
  values = append(values,fileName)
  x = data.frame(values)
  colnames(x)[ncol(x)] <- "result"
  return(x)
}

addJitter = function(route){
  temp <- as.data.frame(route)
    for(i in 1:nrow(temp)){
         temp$longitude[i] = temp$longitude[i] + (runif(1,-0.01,0.01))
         temp$latitude[i] = temp$latitude[i]  + (runif(1,-0.01,0.01))
         temp$distance[i] = temp$distance[i] + (runif(1,-0.01,0.01))
         temp$bearing[i] = temp$bearing[i] + (runif(1,-0.01,0.01))
    }
    return(temp)
}

calculateJitter = function(valuesResult, jitteredRoute, file, results, amount){
  for (i in 1:amount){
    jitteredRoute <- addJitter(valuesResult)
    
    jitteredRoute <- transformFrame(jitteredRoute, getName(file))
    results <- rbind(results, jitteredRoute)
    
  }
  return(results)
}

init = function(){
  list.of.packages <- c("foreign", "RWeka","partykit","plotKML")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(foreign)
  library("RWeka")
  library("partykit")
  library("plotKML")
}

makeArff = function(){
  results <- data.frame()
  jitteredRoute <- data.frame(c)
  wd <- getwd()
  files = c(paste(wd,"\\Jaar3\\AI\\routes\\GerardHomeWork.csv",sep=""),paste(wd,"\\Jaar3\\AI\\routes\\GerardWorkHome.csv",sep=""),paste(wd,"\\Jaar3\\AI\\routes\\JosHomeWork.csv",sep=""),paste(wd,"\\Jaar3\\AI\\routes\\JosWorkHome.csv",sep=""),paste(wd,"\\Jaar3\\AI\\routes\\PietWorkHome.csv",sep=""))
  for(j in files){
    interpolatedRoute <- dataset(j)
    valuesResult<-data.frame()
    valuesResult <- values(interpolatedRoute)
    results <- rbind(results, transformFrame(valuesResult,getName(j)))
    
    results<- calculateJitter(valuesResult, jitteredRoute, j, results, 50)
  }
  write.arff(results,paste(wd,"\\Jaar3\\AI\\routes\\temp.arff",sep=""))
}

buildTree = function(){
  wd <- getwd()
  data <- read.arff(paste(wd,"\\Jaar3\\AI\\routes\\temp.arff",sep=""))
  data[,'train'] <- ifelse(runif(nrow(data))<0.8,1,0)
  testData = data[data$train==0,]
  trainData = data[data$train==1,]
  testData$result <- NULL
  testData$train<-NULL
  trainData$train<-NULL
  j48 <<- J48(result~.,data = trainData)
  return(j48)
}

main = function(){
  init()
  makeArff()
  tree <- buildTree()
  shiny(tree)
}

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
             tags$style(type='text/css', "#submit { vertical-align: middle; height: 50px; width: 100%; font-size: 30px;}")
             ),
      mainPanel(
        leafletOutput("treemap")
      )
    )
  )
  
  
  
  server <- function(input, output, session) {
    points <- data.frame()
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
      points <- values(points)
      points <- transformFrame(points,"")
      prediction <- predict(j48,points)
      route <- read.csv(paste(wd,"\\Jaar3\\AI\\routes\\",prediction[1],".csv",sep=""),header=FALSE)
      lon <- c(route$V2[1],route$V2[nrow(route)])
      lat <- c(route$V1[1],route$V1[nrow(route)])
      leafletProxy("treemap") %>%
        addPolylines(lng = route$V2, lat=route$V1) %>% addMarkers(lon,lat, popup = as.character(c("Start","End"))) %>% setView(lng=route$V2[nrow(route)/2],lat=route$V1[nrow(route)/2], zoom=15)
      
    })
  }
  shinyApp(ui, server)
}

main()

csv = read.csv(paste(wd,"\\Jaar3\\AI\\routes\\PietWorkHome.csv",sep=""),header=FALSE)
csv <- interpolateRoute(csv)
csv <- values(csv)
csv <- transformFrame(csv,getName(paste(wd,"\\Jaar3\\AI\\routes\\GerardWorkHome.csv",sep="")))
csv$result <- NULL

prediction <- predict(j48, csv)
print(prediction)
# 
# library(rpart)
# fit <- rpart(result ~ .,data = results)
# 
library("plotKML")
testRoute <- readGPX(paste(wd,"\\Jaar3\\AI\\test_routes\\JosHomeWork.gpx",sep=""))
testRoute <- testRoute$tracks[[1]]$`Edestraat 21 to Siervogelstraat 2`
testRoute$name <- NULL
colnames(testRoute) <- c("V1","V2")
testRouteTest <- interpolateRoute(testRoute)
testRoute <- values(testRoute)
testRoute <- transformFrame(testRoute,"")
testRoute$result <- NULL
# 
# write.arff(testRoute,paste(wd,"\\Jaar3\\AI\\routes\\testRoute.arff",sep=""))
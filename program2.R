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

calculateDistance = function(long2,lat2,long1,lat1){
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d)
}

calculateBearings = function(lon2,lat2,lon1,lat1){
  toDegrees = function(rad) {(rad *180) / (pi)}
  y = sin(lon2-lon1) * cos(lat2)
  x = cos(lat1)* sin(lat2) - sin(lat1)* cos(lat2)* cos(lon2-lon1)
  bearing =  toDegrees(atan2(y, x))
  return(bearing)
}

calculateValues <- function(route){
  size<-nrow(route)
  for(i in 2:size){
    route$distance[i]<-calculateDistance(route$longitude[i], route$latitude[i], route$longitude[i-1], route$latitude[i-1])
    route$bearing[i] <- calculateBearings(route$longitude[i], route$latitude[i], route$longitude[i-1], route$latitude[i-1])
  }
  route$distance[1]=0
  route$bearing[1]=0
  return(route)
}

getName <- function(file){
  name <- strsplit(file,"/")
  name <- as.list(name[[1]])
  name <- name[length(name)]
  name <- strsplit(name[[1]],'.',fixed=TRUE)
  name <- as.list(name[[1]])[[1]]
  return(name)
}

dataset <- function(file){
  temp <- readFile(file)
  temp<-interpolateRoute(temp)
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

addJitter = function(interpolatedRoute){
  temp <- as.data.frame(interpolatedRoute)
  temp<- temp[1:nrow(temp),1:ncol(temp)] + runif(nrow(temp),-0.01,0.01)
  return(temp)
}

calculateJitterXTimes = function(interpolate,file){
  jittered <- addJitter(interpolate)
  return(transformFrame(calculateValues(jittered),getName(file)))
}

calculateJitter = function(interpolatedRoute, file, amount){
  result <- data.frame()
  interpolate <- as.data.frame(interpolatedRoute)
  result<-do.call("rbind",lapply(1:amount, function(x) calculateJitterXTimes(interpolate,file)))

  return(result)
}

init = function(){
  list.of.packages <- c("foreign", "RWeka","partykit","plotKML","shiny","leaflet")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(foreign)
  library("RWeka")
  library("partykit")
  library("plotKML")
  library(data.table)
}

makeDataFrame = function(file,results){
  wd <- getwd()
  file <- paste(wd,"/",file,sep="")
  interpolatedRoute <- dataset(file)
  results <- rbind(results,transformFrame(calculateValues(interpolatedRoute),getName(file)))
  results <- rbind(results,calculateJitter(interpolatedRoute,file,50))
}

makeArff = function(){
  results<-data.frame()
  jitteredRoute <- data.frame(c)
  setwd("C:/Users/Michiel/AI/routes")
  files <- list.files(pattern="*.csv", path=getwd())
  results <- do.call("rbind", lapply(as.list(files),function(x) makeDataFrame(x,results)))
  write.arff(results,paste(getwd(),"/temp.arff",sep=""))
}

buildTree = function(){
  wd <- getwd()
  data <- read.arff(paste(wd,"/temp.arff",sep=""))
  data[,'train'] <- ifelse(runif(nrow(data))<0.70,1,0)
  testData = data[data$train==0,]
  trainData = data[data$train==1,]
  testData$train<-NULL
  checkSet <- as.data.frame(testData)
  testData$result <- NULL
  trainData$train<-NULL
  j48 <<- J48(result~.,data = trainData)
  j <- 0
  for(i in 1:nrow(testData)){
    if(predict(j48, testData[i,])[1] == checkSet[i,"result"]){
      j = j + 1
    }
  }
  print(j)
  plot(j48)
  return(j48)
}

main = function(){
  init()
  makeArff()
  tree <- buildTree()
  if(!exists("shiny", mode="function")) source("C:/Users/Michiel/AI/shiny.R")
  shiny(tree)
}

main()
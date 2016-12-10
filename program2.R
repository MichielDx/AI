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
  y = sin(lon2-lon1) * cos(lat2);
  x = cos(lat1)* sin(lat2) - sin(lat1)* cos(lat2)* cos(lon2-lon1);
  bearing =  toDegrees(atan2(y, x));
  return(bearing)
}

calculateValues <- function(route){
  size<-nrow(route)
  for(i in 1:size){
    route$distance[i]<-calculateDistance(route$longitude[i+1], route$latitude[i+1], route$longitude[i], route$latitude[i])
    route$bearing[i] <- calculateBearings(route$longitude[i+1], route$latitude[i+1], route$longitude[i], route$latitude[i])
  }
  route$distance[size]=0
  route$bearing[size]=0
  return(route)
}

getName <- function(file){
  name <- strsplit(file,"/")
  name <- as.list(name[[1]])
  name <- name[length(name)]
  #print(name)
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
    for(i in 1:nrow(temp)){
         temp$longitude[i] = temp$longitude[i] + (runif(1,-0.01,0.01))
         temp$latitude[i] = temp$latitude[i]  + (runif(1,-0.01,0.01))
    }
    return(temp)
}

calculateJitter = function(interpolatedRoute, file, amount){
  result <- data.frame()
  interpolate <- as.data.frame(interpolatedRoute)
  for (i in 1:amount){
    jittered <- addJitter(interpolate)
    result <- rbind(result, transformFrame(calculateValues(jittered),getName(file)))
  }
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

makeArff = function(){
  results <- data.frame()
  jitteredRoute <- data.frame(c)
  setwd("C:/Users/Michiel/AI/routes")
  wd <- getwd()
  files <- list.files(pattern="*.csv", path=wd)
  for(j in files){
    f <- paste(wd,"/",j,sep="")
    interpolatedRoute <- dataset(f)
    results <- rbind(results,transformFrame(calculateValues(interpolatedRoute),getName(f)))
    results <- rbind(results,calculateJitter(interpolatedRoute,f,15))
  }
  write.arff(results,paste(wd,"/temp.arff",sep=""))
}

buildTree = function(){
  wd <- getwd()
  data <- read.arff(paste(wd,"/temp.arff",sep=""))
  data[,'train'] <- ifelse(runif(nrow(data))<0.70,1,0)
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
  if(!exists("shiny", mode="function")) source("C:/Users/Michiel/AI/shiny.R")
  shiny(tree)
}

main()
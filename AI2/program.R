readFile <- function(file){
  measurement <- read.table(file,header=FALSE, sep=",")
  return(measurement)
}

correctFaultTimeBlock <- function(measurements){
  measurements[,2] = measurements[,2] + 7200
  return(measurements)
}

kmean <- function(measurements, clusters){
  temp <- as.data.frame(measurements)
  measurements$Timestamp <- NULL
  if(clusters == 0){
    km    <- SimpleKMeans(measurements)
  } else {
    km    <- SimpleKMeans(measurements, Weka_control(N = clusters))
  }
  measurements$Timestamp <- temp$Timestamp
  return(km)
}

canopy <- function(measurements, clusters){
  Canopy <- make_Weka_clusterer("weka.clusterers.Canopy")
  temp <- as.data.frame(measurements)
  measurements$Timestamp <- NULL
  if(clusters == 0){
    can <- Canopy(measurements)
  } else {
    can <- Canopy(measurements, Weka_control(N = clusters))
  }
  measurements$Timestamp <- temp$Timestamp
  return(can)
}

xmeans <- function(measurements){
  WPM('load-packages','XMeans')
  XMeansCluster <- make_Weka_clusterer("weka.clusterers.XMeans")  
  temp <- as.data.frame(measurements)
  measurements$Timestamp <- NULL
  xm <- XMeansCluster(measurements,c("-L", 4))
  measurements$Timestamp <- temp$Timestamp
  return(xm)
}

kohonen <- function(measurements){
  WPM('load-packages','SelfOrganizingMap')
  SelfOrganizingMap <- make_Weka_clusterer("weka.clusterers.SelfOrganizingMap")  
  temp <- as.data.frame(measurements)
  measurements$Timestamp <- NULL
  koh <- SelfOrganizingMap(measurements, c("-H",4, "-W", 3))
  measurements$Timestamp <- temp$Timestamp
  return(koh)
}

init = function(){
  list.of.packages <- c("foreign","RWeka","shiny","ggplot2","lubridate")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(foreign)
  library("RWeka")
  library("lubridate")
}

getMeasurements = function(){
  measurements <- readFile("C:/Users/Michiel/AI/AI2/temperature data.txt")
  measurements<- correctFaultTimeBlock(measurements)
  measurements<- secondsToTimestamp(measurements)
  measurements$V1 <- NULL
  colnames(measurements) <- c("Timestamp","Temperature")
  return(measurements)
}

secondsToTimestamp = function(measurements){
  measurements[,2] = as.POSIXct(seconds_to_period(measurements[,2]),origin='1960-01-01')
  return(measurements)
}

main = function(){
  init()
  measurements <- getMeasurements()
  return(measurements)
}
readFile <- function(file){
  measurement <- read.table(file,header=FALSE, sep=",")
  return(measurement)
}

correctFaultTimeBlock <- function(measurements){
  measurements[,2] = measurements[,2] + 7200
  return(measurements)
}

kmean <- function(measurements, clusters){
  if(clusters == 0){
    km    <- SimpleKMeans(measurements)
  } else {
    km    <- SimpleKMeans(measurements, Weka_control(N = clusters))
  }
  return(km)
}

canopy <- function(measurements, clusters){
  Canopy <- make_Weka_clusterer("weka.clusterers.Canopy")
  if(clusters == 0){
    can <- Canopy(measurements)
  } else {
    can <- Canopy(measurements, Weka_control(N = clusters))
  }
  return(can)
}

xmeans <- function(measurements){
  XMeansCluster <- make_Weka_clusterer("weka.clusterers.XMeans")
    xm <- XMeansCluster(measurements)
  return(xm)
}

kohonen <- function(measurements){
  SelfOrganizingMap <- make_Weka_clusterer("weka.clusterers.SelfOrganizingMap")
  koh <- SelfOrganizingMap(measurements)
  return(koh)
}

init = function(){
  list.of.packages <- c("foreign","RWeka","shiny","ggplot2")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(foreign)
  library("RWeka")
}

init()

measurements <- readFile("C:/Users/Michiel/AI/AI2/temperature data.txt")
measurements<- correctFaultTimeBlock(measurements)
colnames(measurements) <- c("V1", "Timestamp","Temperature")
measurements$V1 <- NULL
write.arff(measurements,paste("C:/Users/Michiel/AI/AI2/temp.arff"))
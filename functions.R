library(tidyverse)
library(dplyr)

trajectory_step <- function(data){
  x <- data$x
  y <- data$y
  direction <- data$direction
  colorOfIndividual <- data$colorOfIndividual
  lifeCount <- data$lifeCount 
  isLocked <- data$isLocked
  n <- length(x)
  
  for(i in 1:n){
    if(!isLocked[i]){
      x[i] <- x[i] + steplength*sin(direction[i])
      y[i] <- y[i] + steplength*cos(direction[i])
    }
    if(lifeCount[i] > 1){
      lifeCount[i] <- lifeCount[i] - 1
    }
    else if(lifeCount[i] == 1){
      deathCoin = runif(1,0,1)
      mortality <- 0.5*sum(colorOfIndividual == "red")/(n - sum(colorOfIndividual == "brown"))
      if(deathCoin < mortality){
        colorOfIndividual[i] <- "brown"
        x[i] <- 100
        y[i] <- 100
        lifeCount[i] <- 0
      }
      else{
        colorOfIndividual[i] <- "green"
        lifeCount[i] <- 0
      }
    }
  }
  
  M <- distance(cbind(x,y), method = "euclidean")
  
  edgeWidth <- radius*2
  
  edgeInd <- which(x <= edgeWidth | x >= (upperlimit - edgeWidth) | y <= edgeWidth | y >= (upperlimit - edgeWidth))
  direction[edgeInd] <- 
    (y[edgeInd] <= edgeWidth | y[edgeInd] >= (upperlimit - edgeWidth))*
    ((direction[edgeInd] <= pi)*(pi - direction[edgeInd]) + (direction[edgeInd] > pi)*(3*pi - direction[edgeInd])) + 
    (x[edgeInd] <= edgeWidth | x[edgeInd] >= (upperlimit - edgeWidth))*(2*pi - direction[edgeInd]) 
  
  collisionInd <- which(M <= 2*radius)
  
  for(i in collisionInd){
    yInd <- i %% n
    if (yInd == 0) yInd <- n
    xInd <- (i - yInd)/n + 1
    if(!(xInd == yInd)){
      direction[xInd] <- 
        (direction[xInd] + pi/2) %% (2*pi)
      #(atan((y[yInd] - y[xInd])/(x[yInd] - x[xInd])) + pi) %% (2*pi)
      if(colorOfIndividual[xInd] == "red" & colorOfIndividual[yInd] == "blue"){
        colorOfIndividual[yInd] <- "red"
        lifeCount[yInd] <- rpois(1,daysOfDisease)
      }
      else if(colorOfIndividual[yInd] == "red" & colorOfIndividual[xInd] == "blue"){
        colorOfIndividual[xInd] <- "red"
        lifeCount[xInd] <- rpois(1,daysOfDisease)
      }
    }
  }
  direction[is.na(direction)] <- pi
  data$x <- x
  data$y <- y
  data$direction <- direction
  data$colorOfIndividual <- colorOfIndividual
  data$lifeCount <- lifeCount
  data$isLocked <- isLocked
  return(data)
}
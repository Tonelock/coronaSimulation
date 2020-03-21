library(ggplot2)
library(gganimate)
library(philentropy)
library(ggplot2)
theme_set(theme_bw())

source("C:/Users/alexander.lammerich/Documents/R/coronaSimulation/functions.R")

# set necessary parameters for simualtion
n <- 500
upperlimit = 1
radius = 0.01
steplength <- 0.015
x <- runif(n, radius, upperlimit - radius)
y <- runif(n, radius, upperlimit - radius)
direction <- runif(n,0,2*pi)
colorOfIndividual <- rep("blue",n)
numberOfInfectedStart <- 1
daysOfDisease <- 56
mortality <- 0.9
rateOfLocked <- 0.5
timeDays <- 200
infected <- sample(1:n,numberOfInfectedStart)
colorOfIndividual[infected] <- "red"
lifeCount <- rep(0,n)
lifeCount[infected] <- rpois(1,daysOfDisease)
locked <- sample(1:n,ceiling(rateOfLocked*n))
isLocked <- rep(FALSE,n)
isLocked[locked] <- TRUE
isLocked[infected] <- FALSE

# build data frame
data <- data.frame(x = x,
                   y = y,
                   direction = direction,
                   colorOfIndividual = colorOfIndividual,
                   lifeCount = lifeCount,
                   isLocked = isLocked)
levels(data$colorOfIndividual) <- c(levels(data$colorOfIndividual),"green","brown")

# perform simulation
for(i in 1:timeDays){
  deathCount <- paste0(
    "Disease: ", sum(data$colorOfIndividual == "red"),
    " Recovered: ", sum(data$colorOfIndividual == "green"),
    " Dead: ", sum(data$colorOfIndividual == "brown"))
  plot(data$x,data$y,xlim = c(0,upperlimit), ylim = c(0,upperlimit), xlab = "x", ylab = "y", col = data$colorOfIndividual, main = deathCount)
  #ggplot(data, aes(x = data$x, y = data$y)) +
  #  geom_point()
  data <- trajectory_step(data)
}






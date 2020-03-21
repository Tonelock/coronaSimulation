
incrementSteps <- function(data){
  data[isLocked == FALSE,] <- data[isLocked == FALSE,] %>%
    mutate(x = x + steplength*sin(direction)) %>%
    mutate(y = y + steplength*cos(direction))
  
  data[lifeCount > 1] <- mutate(lifeCount - 1)

  return(data)
}

decreaseDaysOfDisease <- function(data){
  
}

data <- incrementSteps(data)

library(ggplot2)
library(dplyr)

datx = c(0, 3, 6, 7, 15, 12, 14, 9, 7, 0)
daty = c(1, 4, 5, 3, 0, 4, 10, 6, 9, 10)

P=250
n=0.8
pm=0.2
Tmax=1000

#Function calculating distance from one city to another
calculateCityToCity <- function(citya, cityb){
  a = abs(citya[1] - cityb[1])
  b = abs(citya[2] - cityb[2])
  return(sqrt((a^2)+(b^2)))
}

#Function creating a distance matrix for all the cities
calculateDistanceMatrix <- function(cities){
  output <- matrix(nrow=length(cities[1,]), ncol = length(cities[1,]))
  for(i in 1:length(cities[1,])){
    for(j in 1:length(cities[1,])){
      output[i,j] = calculateCityToCity(cities[,i], cities[,j])
    }
  }
  return(output)
}

#Function that creates the initial population
initialPopulation <- function(cities, pSize){
  outData <- replicate(pSize, sample(1:length(cities[1,]), replace = FALSE))
  print(outData)
  return(outData)
}

populationEvaluation <- function(population, distansMatrix){
  output <- numeric(length(population[1,]))
  for(i in 1:length(output)){
    output[i] <- distansMatrix[population[1,i], population[length(population[,1]),i]]
    for(j in 1:(length(population[,1])-1)){
      output[i] <- output[i] + distansMatrix[population[j,i],population[j+1,i]]
    }
  }
  return(output)
  #print(output)
}

crossingFunction <- function(P1, P2){
  O <- numeric(length(P1))
  O[1] <- P1[1]
  x <- P2[1]
  while(isFALSE(is.element(x, O))){
    pos <- match(x, P1)
    O[pos] <- x
    x <- P2[pos]
  }
  for(i in 1:length(O)){
    if(O[i] == 0){
      O[i] = P2[i]
    }
  }
  return(O)
}

cities <- matrix(c(datx, daty), nrow=2, ncol = 10, byrow = TRUE)
print("Initial city coordinates:")
print(cities)

distansMatrixx <- calculateDistanceMatrix(cities)
print("Distance matrix:")
print(distansMatrixx)

population <- initialPopulation(cities, P)

print("Initial population: ")
head(population)


for(x in 1:Tmax){
  costMatrix <- populationEvaluation(population, distansMatrixx)
  fitnesMatrix <- -costMatrix + max(costMatrix)
  #print(fitnesMatrixOrdered)
  fitnesSum <- sum(fitnesMatrix)
  #print(fitnesSum)
  #Generating offspring
  offspring = matrix(0, nrow = length(population[,1]), ncol = as.integer(P * n))
  parentPool = matrix(0, nrow = length(population[,1]), ncol = as.integer(P * n))
  for(i in seq(1, as.integer((P * n)), by=1)){
    sumP1 = 0
    P1prob = runif(1, min=0, max=fitnesSum)
    P1 = 0
    for(j in 1:length(fitnesMatrix)){
      sumP1 = sumP1 + fitnesMatrix[j]
      if(sumP1>=P1prob){
        P1 = j
        break
      }
    }
    parentPool[,i] <- population[,P1]
  }
  for(i in seq(1, as.integer((P * n)), by=2)){
    singleParents <- sample(1:length(parentPool[1,]), 2, replace = FALSE)
    
    O1 = crossingFunction(parentPool[,singleParents[1]], parentPool[,singleParents[2]])
    offspring[,i] <- O1
    O2 = crossingFunction(parentPool[,singleParents[2]], parentPool[,singleParents[1]])
    offspring[,i+1] <- O1
  }
  #print(offspring)
  offspringMutated <- offspring
  #mutating offspring
  for(i in 1:length(offspringMutated[1,])){
    chance <- runif(1)
    if(chance < pm){
      swap <- sample(1:length(offspringMutated[,1]), 2, replace = FALSE)
      temp <- offspringMutated[ swap[1], i]
      offspringMutated[ swap[1], i] <- offspringMutated[ swap[2], i]
      offspringMutated[ swap[2], i] <- temp
    }
  }
  #print(offspringMutated)
  offspringCost <- populationEvaluation(offspringMutated, distansMatrixx)
  offspringWithCost <- rbind(offspringMutated, offspringCost)
  parrentsWithCost <- rbind(population, costMatrix)
  combinedPopulations <- cbind(offspringWithCost, parrentsWithCost)
  population <- combinedPopulations[1:(length(combinedPopulations[,1])-1),order(combinedPopulations[length(combinedPopulations[,1]),])]
  population <- population[, 1:P]
}

finalCost <- populationEvaluation(population, distansMatrixx)
result <- rbind(population, finalCost)
resultOrdered <- result[, order(result[length(result[,1]),])]
citiesOrdered <- data.frame(x = cities[1, resultOrdered[1:(length(resultOrdered[,1])-1), 1]], y=cities[2, resultOrdered[1:(length(resultOrdered[,1])-1), 1]])
citiesOrdered <- rbind(citiesOrdered, citiesOrdered[1,])
p <- ggplot(citiesOrdered, aes(x, y)) + geom_point(size = 5, color = "darkgrey") + geom_path()
p
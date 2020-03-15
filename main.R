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
    output[i] <- distansMatrix[population[1,i], population[-1,i]]
    for(j in 1:(length(population[,1])-1)){
      output[i] <- output[i] + distansMatrix[population[j,i],population[j+1,i]]
    }
  }
  print(output)
}

cities = matrix(c(datx, daty), nrow=2, ncol = 10, byrow = TRUE)
print("Initial city coordinates:")
print(dat)

distanceMatrix <- calculateDistanceMatrix(dat)
print("Distance matrix:")
print(distanceMatrix)

population <- initialPopulation(cities, P)
print("Initial population: ")
head(population)

populationEvaluation(population, distanceMatrix)

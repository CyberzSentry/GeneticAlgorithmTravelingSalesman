datx = c(0, 3, 6, 7, 15, 12, 14, 9, 7, 0)
daty = c(1, 4, 5, 3, 0, 4, 10, 6, 9, 10)

P=250
n=0.8
pm=0.2
Tmax=1000

calculateDistance <- function(citya, cityb){
  a = abs(citya[0] - cityb[0])
  b = abs(citya[1] - cityb[1])
  return(sqrt((a^2)+(b^2)))
}

initialPopulation <- function(cities, pSize){
  outData <- list()
  for(i in 1:pSize){
    outData[[i]] <- cities[,sample(1:10)]
  }
  return(outData)
}

dat = matrix(c(datx, daty), nrow=2, ncol = 10, byrow = TRUE)
print(dat)
print(calculateDistance(dat[,1], dat[,2]))
#population <- initialPopulation(dat, P)



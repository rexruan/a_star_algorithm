# File:         rex.r 
# Description:  A* algorithms 
#               for Project 1, Artificial Intelligence 2019, UU
# Author:       Rex Ruan

# Install the package
# install.packages("DeliveryMan_1.1.0.tar.gz", repos = NULL, type="source")

# Load the library
library("DeliveryMan")

myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  # What is our goal?
  if(carInfo$load == 0) {
    node <- nextPickup(trafficMatrix,carInfo,packageMatrix)
    if(node[1]==1) {
    carInfo$nextMove <- 5
    return(carInfo)
    } else {
      carInfo$nextMove <- move(c(node[4:5]),c(node[2:3]))
      return(carInfo)
    }
  } else {
    carInfo$mem$goal <- packageMatrix[carInfo$load, c(3,4)]
  } 
  # How do we get there?
  carInfo$nextMove <- nextMove(trafficMatrix,
                               carInfo,
                               packageMatrix)
  return(carInfo)
}

h <- function(origin, target) {
  return (abs(target[1]-origin[1]) + abs(target[2]-origin[2]))
}

# compute traffic cost between two neighbor nodes
costTraffic <- function(origin, neighbor, trafficMatrix) {
  if (origin[1] == neighbor[1]) {
    return (trafficMatrix$vroads[origin[1],min(c(origin[2],neighbor[2]))])
  } else {
    return (trafficMatrix$hroads[min(c(origin[1],neighbor[1])),origin[2]])
  }
}

# generate the neighbor nodes
neighbors <- function(origin) {
  if(origin[1]>1 && origin[1]<10) {
    x = c(origin[1]-1, origin[1]+1)
  } else if(origin[1]==1) {
    x = 2
  } else {
    x = 9
  }
  if(origin[2]>1 && origin[2]<10) {
    y = c(origin[2]-1, origin[2]+1)
  } else if(origin[2]==1) {
    y = 2
  } else {
    y = 9
  }
  if(length(x)==1 && length(y)==1) {
    return (list(c(x,origin[2]), c(origin[1],y)))
  } else if(length(x)==2 && length(y) == 1) {
    return (list(c(x[1],origin[2]),c(x[2],origin[2]),c(origin[1],y)))
  } else if(length(x)==1 && length(y) == 2) {
    return (list(c(x, origin[2]), c(origin[1],y[1]), c(origin[1],y[2])))
  } else {
    return (list(c(x[1],origin[2]),c(x[2],origin[2]),c(origin[1],y[1]),c(origin[1],y[2])))
  }
}

# compare if two nodes are same
compare <- function(a,b) {
  if(a[1]==b[1] && a[2]==b[2]){
    return (1)
  } else{
    return (0)
  }
}

# check if one node in a list
findIndexInRows <- function(node,m) {
  if(length(m)==2){
    if(compare(node,m)==1) {
      return(1)
    }
  } else {
      for(i in 1:(length(m)/2)){
        if(compare(m[i,],node)==1){
          return (i)
        }
      }
    }
  return(0)
}

#A star search
aStarSearch <- function(origin, target, trafficMatrix) {
 
  ##frontiers <- list(list(c(origin),0, list(c(origin)))) #location; traffic cost; path
  # current location, traffic cost, heuristic cost, total cost, initial move, previous move 
  frontiers <- matrix(data=c(origin,0,h(origin,target),999,NA,NA,TRUE),nrow = 1)
  #colnames(frontiers) <- c('cx','cy','cost','h','total','mx','my','visited')
  current <- 1
  while(min(frontiers[,4])>0 || frontiers[which.min(frontiers[,4]),8]==FALSE) {
    #newFrontiers <- list()
    newNeighborList <- list()
    unVisitedNeighborList <- list()
    for (neighbor in neighbors(frontiers[current,1:2])) {
      index = findIndexInRows(neighbor,frontiers[,1:2])
      if (index == 0) {
        newNeighborList[[length(newNeighborList)+1]] <- c(neighbor)
      } else {
        if(frontiers[index,8]==FALSE) {
          unVisitedNeighborList[[length(unVisitedNeighborList)+1]] <- c(index)  
        }
      }
    }
    
    
        for(index in unVisitedNeighborList) {
          newCost <- frontiers[current,3]+costTraffic(c(frontiers[current,1:2]),c(frontiers[index,1:2]),trafficMatrix)
          if(frontiers[index,3] > newCost){
            frontiers[index,3] <- newCost
            frontiers[index,6:7] <- frontiers[current,6:7]
            frontiers[index,5] <- frontiers[index,3] + frontiers[index,4]
          }
        }
        for(neigh in newNeighborList){
          cost <- costTraffic(neigh,frontiers[current,1:2],trafficMatrix) + frontiers[current,3]
          heuristic=h(neigh,target)
          total <- cost + heuristic 
          if(current==1){
            mx <- neigh[1]
            my <- neigh[2] 
          } else {
            mx <- frontiers[current,6]
            my <- frontiers[current,7]
          }
          frontiers <- rbind(frontiers,c(neigh,cost,heuristic,total,mx,my,FALSE))
        }
    current <- which.min(frontiers[,5])
    frontiers[current,8] <- TRUE
    frontiers[current,5] <- 999
  }
  index = which.min(frontiers[,4])
  return(c(frontiers[index,]))
}

# Find the nearest pickup location for an undelivered package
nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
  packageUndone <- list()
  for(i in 1:5) {
    if(packageMatrix[i,5]==0){
      packageUndone[[length(packageUndone)+1]] <- packageMatrix[i,1:2]
    }
  }
  origin <- c(carInfo$x, carInfo$y)
  distance <- c()
  goNext <- matrix(ncol=2,nrow=0)
  for(goal in packageUndone){
    if(compare(goal,origin)==1){
      return(c(1,goal))
    }
    r = aStarSearch(origin,goal,trafficMatrix)
    distance <- append(distance, r[3])
    goNext <- rbind(goNext,r[6:7])
  }
  
  return (c(0,goNext[which.min(distance),],origin))
  if(FALSE){distanceVector = abs((packageMatrix[,1] - carInfo$x)) +
    ((packageMatrix[,2] - carInfo$y))
  distanceVector[packageMatrix[,5] != 0] = Inf
  return(packageMatrix[which.min(distanceVector), c(1,2)])}
}

# Find the move to get to carInfo$mem$goal
nextMove <- function(trafficMatrix, carInfo, packageMatrix) {
  target <- carInfo$mem$goal
  origin <- c(carInfo$x,carInfo$y)
  if(compare(target,origin)==1){
    return(5)
  } else {
  nextStep <- aStarSearch(origin,target,trafficMatrix)[6:7]
  }
  move(origin,nextStep)
}

move <- function(origin,nextStep) {
  if(compare(origin,nextStep)==1){
    return(5)
  }
  if(origin[1] == nextStep[1]) {
    if(origin[2] > nextStep[2]) {
      return(2)
    } else {
      return(8)
    }
  } else {
    if(origin[1] > nextStep[1]) {
      return(4)
    } else {
      return(6)
    }
  }
}
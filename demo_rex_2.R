# File:         demo.r 
# Description:  Naive demo-solution given at classroom session
#               for Project 1, Artificial Intelligence 2019, UU
# Author:       Fredrik Nilsson

# Install the package
# install.packages("DeliveryMan_1.1.0.tar.gz", repos = NULL, type="source")

# Load the library
library("DeliveryMan")

# Read documentation
# ?runDeliveryMan
# ?testDM

myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  # What is our goal?
  if(carInfo$load == 0) {
    carInfo$mem$goal <- nextPickup(trafficMatrix, 
                                   carInfo, 
                                   packageMatrix)
  } else {
    carInfo$mem$goal <- packageMatrix[carInfo$load, c(3,4)]
  }
  
  # How do we get there?
  carInfo$nextMove <- nextMove(trafficMatrix,
                               carInfo,
                               packageMatrix)
  return(carInfo)
}

v2n <- function(node) {
  return (node[1]*100+node[2])
}

n2v <- function(num) {
  return (c(num %/% 100, num %% 100))
}
h <- function(origin, target) {
  origin <- n2v(origin)
  target <- n2v(target)
  return (2*(abs(target[1]-origin[1]) + abs(target[2]-origin[2])))
}

# compute traffic cost between two neighbor nodes
costTraffic <- function(origin, neighbor, trafficMatrix) {
  origin <- n2v(origin)
  neighbor <- n2v(neighbor)
  if (origin[1] == neighbor[1]) {
    return (trafficMatrix$vroads[origin[1],min(c(origin[2],neighbor[2]))])
  } else {
    return (trafficMatrix$hroads[min(c(origin[1],neighbor[1])),origin[2]])
  }
}

# generate the neighbor nodes
neighbors <- function(origin) {
 
  origin <- n2v(origin)
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
findIndexInRows <- function(node,l) {
  for(i in 1:length(l)) {
    if(l[[i]] == node) {
      return(i)
    }
  }
  return(0)
}



#A star search
aStarSearch <- function(origin, target, trafficMatrix) {
 
  ##frontiers <- list(list(c(origin),0, list(c(origin)))) #location; traffic cost; path
  # current location, traffic cost, heuristic cost, total cost, initial move, previous move 
  frontiers <- matrix(data=c(origin,0,h(origin,target),999,NA,TRUE),nrow = 1)
  #colnames(frontiers) <- c('cxy','cost','h','total','mxy,'visited')
  current <- 1
  visited <- list(origin)
  while(min(frontiers[,3])>0 || frontiers[which.min(frontiers[,3]),6]==FALSE) {
    #newFrontiers <- list()
    newNeighborList <- list()
    unVisitedNeighborList <- list()
    for (neighbor in neighbors(frontiers[current,1])) {
      neighbor <- v2n(neighbor)
      index = findIndexInRows(neighbor,frontiers[,1])
      if (index == 0) {
        if(findIndexInRows(neighbor,visited)==0){
        newNeighborList[[length(newNeighborList)+1]] <- neighbor}
      } else {
        if(frontiers[index,6]==FALSE) {
          unVisitedNeighborList[[length(unVisitedNeighborList)+1]] <- index  
        }
      }
    }
    
    
        for(index in unVisitedNeighborList) {
          newCost <- frontiers[current,2]+costTraffic(c(frontiers[current,1]),c(frontiers[index,1]),trafficMatrix)
          if(frontiers[index,2] > newCost){
            frontiers[index,2] <- newCost
            frontiers[index,5] <- frontiers[current,5]
            frontiers[index,4] <- frontiers[index,2] + frontiers[index,3]
          }
        }
        for(neigh in newNeighborList){
          cost <- costTraffic(neigh,frontiers[current,1],trafficMatrix) + frontiers[current,2]
          heuristic=h(neigh,target)
          total <- cost + heuristic 
          if(current==1){
            mxy <- neigh
            
          } else {
            mxy <- frontiers[current,5]
            
          }
          
          frontiers <- rbind(frontiers,c(neigh,cost,heuristic,total,mxy,FALSE))
        }
      
    visited[[length(visited)+1]] <- frontiers[current,1]
    frontiers <- frontiers[-current,]
    current <- which.min(frontiers[,4])
    
    frontiers[current,6] <- TRUE
    frontiers[current,4] <- 999
    
  }
  #print(frontiers)
  index = which.min(frontiers[,3])
  return(n2v(frontiers[index,5]))
  
}

# Find the nearest pickup location for an undelivered package
nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
  distanceVector = abs((packageMatrix[,1] - carInfo$x)) +
    ((packageMatrix[,2] - carInfo$y))
  distanceVector[packageMatrix[,5] != 0] = Inf
  return(packageMatrix[which.min(distanceVector), c(1,2)])
}

# Find the move to get to carInfo$mem$goal
nextMove <- function(trafficMatrix, carInfo, packageMatrix) {
  target <- carInfo$mem$goal
  origin <- c(carInfo$x,carInfo$y)
  if(compare(target,origin)==1){
    return(5)
  } else {
  nextStep <- aStarSearch(v2n(origin),v2n(target),trafficMatrix)
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
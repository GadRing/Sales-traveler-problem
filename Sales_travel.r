rm(list = ls())
library('shape')
library('pso')


###############################################################################################################
#####Sales Traveler Problem for 21 Lithuanian City#############################################################
#We start in some city and we have to go through all cities only one time and back to the same city from which we started.
#We want to find the shortest distance, which traveler should take.
#We try to get the geometrical figure, in which any edge does not intercept the other edge. We get some polygon(not convex).
#We need to iterate the code 5-10 times to get polygon with not intercepting edges.
#We can try different methods to minimize the distance between cities. 
#If we get the polygon with not intercepting edges, we can take every point on it and start the journey of traveler
#around the polygon.
#We can try to minimize the distance, which traveler should take, but we cannot prove that it is the shortest road.

#Global variable for number of cities in file coordinates2.csv
city_number <- 21

#Saving the data in variables from files distance_between_cities2 and coordinates2
data <- read.csv(file="C:/Users/proto/Documents/PSO/distance_between_cities2.csv")
cordinates <- read.csv(file="C:/Users/proto/Documents/PSO/coordinates2.csv", header=FALSE)

#Function creating empty matrix 21x2 and filling geographical coordinates from file coordinates2
City_cordinates_matrix <- function(city_number){
  
  cor_mat <- matrix(0, city_number,2)
  for(i in 1:length(cordinates[,2])){
    
    cor_mat[i, 1] <- cordinates[i,3]
    cor_mat[i, 2] <- cordinates[i,2]
    
  }
  return(cor_mat)
}
#Saving the output of function City_coordinates_matrix in variable cor_matrix
cor_matrix <- City_cordinates_matrix(city_number)


#Function filling the matrix of distances between cities from file distance_between_cities2.csv
dist_mat_cities_fill <- function(){
  
  distance <- matrix(0, city_number, city_number)
  for(i in 1:length(data[,1])){
    
    distance[data[i,1], data[i,2]] <- data[i,3]
    distance[data[i,2], data[i,1]] <- data[i,3]
  }
  return(distance)
}

#Saving the matrix of distances in variable distance_matrix
distance_matrix <- dist_mat_cities_fill()


#Creating the vector of 21 random numbers. This vector we need in function distance,
#which will be optimized
random_real_number_vec <- c(runif(city_number))

#Function which calculates distance between cities, which are taken at random 
#using vector random_real_number_vec
distance <- function(random_real_number_vec){
  
  sigma <- order(random_real_number_vec)
  dt <- 0
  
  for(i in 1:(city_number-1)){
    dt <- dt + distance_matrix[sigma[i],sigma[i+1]]
  }
  dt <- dt + distance_matrix[sigma[city_number],sigma[1]]
  
  return(dt)
}


#We use psoptim function, which minimizes the the function distance.
#Psoptim - Particle Swarm Optimizer.https://www.rdocumentation.org/packages/pso/versions/1.0.4/topics/psoptim
#We can use different methods to optimize the function distance.
optimized <- psoptim(random_real_number_vec,distance)


#Order random numbers from smallest to biggest, which is the path which traveler takes
Optimized_distance <- order(optimized$par)
#The lenght of distance, which traveler took
Length_Of_opt_dis <- optimized$value

#Printing Optimized length of distance
print(Length_Of_opt_dis)
#Printing the order of traveling city to city
print(Optimized_distance)
#Printing the names of cities, in optimized road
print(cordinates[Optimized_distance,1])
#Printing the file of Cities and their coordinates
print(cordinates)
#Ploting coordinates
plot(cor_matrix, main = "Shortest Distance Between Cities", xlab = "Longitude", ylab = "Latitude")
#Write the numbers on coordinates
text(cor_matrix[,1], cor_matrix[,2]-0.1, labels = seq(1, city_number, by = 1))

#Connect the points in plot
for(i in 1:(city_number-1)){
  segments(cor_matrix[Optimized_distance[i],1],cor_matrix[Optimized_distance[i],2], cor_matrix[Optimized_distance[i+1],1],cor_matrix[Optimized_distance[i+1],2])
}
segments(cor_matrix[Optimized_distance[city_number],1],cor_matrix[Optimized_distance[city_number],2], cor_matrix[Optimized_distance[1],1],cor_matrix[Optimized_distance[1],2])


## Put comments here that give an overall description of what your
## functions do

#Written By Pedro Forli

#I would like to apologize for the english mistakes i am about to make
#i'm from Brazil...


## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  ##### This function creates a special "matrix" object that can cache its inverse. #####
  
  
  #The i variable is used to store in memory a certain the inverted matrix
  i <- NULL
  
  #Set is just used to set the value of the matrix x to a given y and to
  #reset the inverted matrix stored in the cache
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #Get is used to get the matrix value x
  get <- function() x
  
  #Set inverted is used to set the inverted matrix stored in cache
  #to a given value
  set_inverted <- function(inverted) i <<- inverted
  
  #This function gets the inverted matrix stored
  get_inverted <- function() i
  
  #We return a list containing those special functions
  list(set = set, get = get,
       set_inverted = set_inverted ,
       get_inverted  = get_inverted )
}
}


## Write a short comment describing this function

##### This function computes the inverse of the special "matrix"          #####
##### returned by makeCacheMatrix above. If the inverse has already been  #####
##### calculated (and the matrix has not changed), then the cachesolve    #####
##### should retrieve the inverse from the cache.                         #####

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #First we get the inverted matrix stored in cache
  i <- x$get_inverted()
  
  #If the i variable is not NULL then we should return the inverted
  #matrix stored in cache instead of re-doing the invertion
  if(!is.null(i)) {
    return(i)
  }
  
  #If we didn't invert the matrix x yet we:
  
  #Get the actual matrix
  data <- x$get()
  
  #Invert it using the built-in solve function
  i <- solve(data, ...)
  
  #Then we set the inverted matrix in cache to be what we just obtained
  x$set_inverted(i)
  
  #And finally we return the inverted matrix
  i
}

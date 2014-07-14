# make function for matrix inversion 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL         # store inverse in this env 
  # set value of vector 
  set <- function(y) {
    x <<- y            
    inv <<- NULL
  }
  # get vector
  get <- function() x
  # set inverse
  setinv <- function(inv1) 
    inv <<- inv1            # check how to get this inverse
  # get inverse 
  getinv <- function() inv
  # return list of functions 
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
} # end function 

# retieve or calculate mean 
cacheSolve <- function(x, ...) {
  inv <- x$getinv()   # retrieve cached inverse
  
  if(!is.null(inv)) {    # if mean already cached 
    message("getting cached data")
    return(inv)
  }
  data <- x$get()         # retreve matrix 
  inv <- solve(data, ...) # calculate matix inversion  
  x$setinv(inv)           # set inveerse in cache
  inv                     # return inverse
} # end function 

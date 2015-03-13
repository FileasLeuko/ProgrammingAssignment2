## Dear reviewer, 
# thank you for taking the time to look into my humble
# contribution! 

####################################################
## The following function yields a vector, which is actually a list, 
## 

makeCacheMatrix <- function(x = numeric()) {
  sol <- NULL
  set <- function(y) {
    x <<- y
    sol <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) sol <<- solve
  getSolve <- function() sol
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}





#############################################
cacheSolve <- function(x, ...) {
  sol <- x$getSolve()
  if(!is.null(sol)) {
    message("getting cached data")
    return(sol)
  }
  data <- x$get()
  sol <- solve(data, ...)
  x$setSolve(sol)
  sol
}

# and for the fun of the game, a random cat content for some feel good factor
# http://www.gifbin.com/986145

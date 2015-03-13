## Dear reviewer, 
# thank you for taking the time to look into my humble
# contribution! I took most of the commenting from the 
# support forum, but I hope it will suffice for 
# satisfactory grading. 
# Best Filip
rm(list=ls())

####################################################
# The following function yields a vector, which is actually a list. 
# The mean function from the example is replaced with solve(). 

# To see how the function works, continue reading after the first
# block of code... 

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

####################################
# first create a 4x4 matrix, nonsingular, arbitrary content

x <- matrix(c(1:3, 8, 6:8, 13, 7, 12:14, 17:20), nrow = 4)
x

# apply makeCacheMatrix to that matrix
xl <- makeCacheMatrix(x)
xl

# the third element deserves a note: here inverse of the matrix
# gets stored in the parent environment, to be accessed at a 
# repeated call to the solver-function. 
xl[[3]]



#############################################
# here again, by running the code the workings become clear

cacheSolve <- function(x, ...) {
  sol <- x$getSolve()
    if(!is.null(sol)) {
      message("getting cached data")
      return(sol)
    }
    else message("calculating inverse")
  data <- x$get()
  sol <- solve(data, ...)
  x$setSolve(sol)
  sol
}

# now we use cacheSolve on xl, the element we just created.
xinverse <- cacheSolve(xl)
# gives you the message "calculating inverse"

# and since it has already been calculated, running 
# cacheSolve(xl) again gives "getting cached data"
xinverse2 <- cacheSolve(xl)


###########################################
# and for the fun of the game, a random cat content for some feel good factor
# http://www.gifbin.com/986145

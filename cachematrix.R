# This code is to create the ability to cache the inverses of matrices
#  'makeCacheMatrix' - creates the 'special' matrix that will cache the inverse
#  'cacheSolve' - will solve the inverse of a 'special' matrix


# Creates a special "matrix" object that can cache its inverse.
# 4 functions within this function:
#  1.  set the value of the matrix
#  2.  get the value of the matrix
#  3.  set the value of the inverse
#  4.  get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # value of inverse
  set <- function(y) {                    # set the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x                     # get the matrix
  setInv <- function(solve) i <<- solve   # set the inverse
  getInv <- function() i                  # get the inverse
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


# Computes the inverse of the special "matrix" returned by `makeCacheMatrix`
#  above. If the inverse has already been calculated (and the matrix has not
#  changed), then `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i) # ends function too
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
  ## Return a matrix that is the inverse of 'x'
}



###############################################################################
#####      Tests                                                           ####
###############################################################################
?matrix
m1<-matrix(c(1,2,3,4),2, 2)
m1
solve(m1)

sm1 <-makeCacheMatrix(m1)
cacheSolve(sm1)                 #Creating the cache
cacheSolve(sm1)                 #Printing from cache

bigm <- matrix(c(rep(1,11),2,rep(1,10),3,rep(1,10),4,rep(1,10),5,rep(1,10),
                 6,rep(1,10),7,rep(1,10),8,rep(1,10),9,rep(1,10),10  ), 10,10)
bigm
solve(bigm)

sbm <-makeCacheMatrix(bigm)
cacheSolve(sbm)                 #Creating the cache
cacheSolve(sbm)                 #Printing from cache
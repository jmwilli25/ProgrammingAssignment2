# Sample matrix x <- matrix(c(1,5,2,9),2,2), whose inverse is c(-9,2,5,-1)
# This source file calculates the inverse of a square invertible matrix and adds the
# solution to a cache. If the inverse of the matrix is called again after being
# initially calculated it is returned from cache rather than being calculated again.

# This function takes the provided matrix and will return the matrix, set the 
# inverse, and get the inverse if previously set.
makeCacheMatrix <- function(x = matrix()) {
  inverseMtx <- NULL
  set <- function(y) {
    x <<- y
    inverseMtx <<- NULL
  }
  getMatrix <- function() x
  getInverseMtx <- function() inverseMtx
  setInverseMtx <- function(y) inverseMtx <<- y
  list(set = set, getMatrix = getMatrix, getInverseMtx = getInverseMtx, setInverseMtx = setInverseMtx)
}

# This function calculates the inverse and sets it to the cache and returns the
# inverse or it returns the inverse from the cache if it was already set.
cacheSolve <- function(z, ...) {
  if(!is.null(z$getInverseMtx())) { # Returns inverse of matrix from cache if not null
    message("Getting cached data")
    inverseMtx <- z$getInverseMtx()
    return(inverseMtx)
  } else { # If inverse of matrix is not in cache this calculates, sets the inverse into the cache, then calls the inverse from the cache
    matrix <- z$getMatrix()
    inverseMtx <- solve(matrix)
    z$setInverseMtx(inverseMtx)
    return(z$getInverseMtx())
  }
}
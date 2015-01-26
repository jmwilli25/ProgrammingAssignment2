# x <- matrix(c(2,6,1,9),2,2)
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
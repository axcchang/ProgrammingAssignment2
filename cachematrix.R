## makeCacheMatrix and cacheSolve
## makeCacheMatrix caches a matrix and allow for the caching of the inverse of the matrix
## cacheSolve returns the inverse of the matrix. If the inverse matrix exists in cache, this
## is returned. If not, then the inverse is computed and returned.

## makeCacheMatrix allows the creation of a matrix and the
## caching of both the matrix and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ##initialize the inverse of x, i
  i <- NULL

  ##set function sets the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  ##get returns the matrix
  get <- function() x

  ##setinverse sets the local matrix to the inverse
  setinverse <- function(inverse) i <<- inverse

  ##getinverse returns the inverse matrix, i
  getinverse <- function() i

  list(set = set, get=get,
        setinverse=setinverse,
        getinverse=getinverse)

}


## cacheSolve returns a matrix that is the inverse of the
## provided matrix. If the provided matrix, x, doesn't have a
## cached inverse, the inverse matrix is calculated and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting inverse matrix")
    return(i)
  }
  localmatrix <- x$get()
  inv <- solve(x)
  x$setinverse(inv)
  inv
}

makeVector <- function(x = numeric()) {
  m <- NULL

  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
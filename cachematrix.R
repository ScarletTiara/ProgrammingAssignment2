## makeCacheMatrix & cacheSolve together act upon a matrix to get its inverse - 
## if it has already been called once, it'll give out the value of the previously 
## calculated inverse (the cache!) on a recall.


## makeCacheMatrix creates a matrix of any size and stores the inv of the first  
## matrix determined by the cacheSolve function.

makeCacheMatrix <- function(x = matrix(...)) {
  ## 'a' will become the variable containing the inv of the matrix as a cache.
  a <- NULL
  
  get <- function() x
  set <- function(q) {
    x <<- q
    a <<- NULL
  }
  
  getinv <- function() a
  setinv <- function(solve) a <<- solve
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve: either forms a new matrix or returns the cached value.

cacheSolve <- function(x, ...) 
  ## To return a matrix that is the inverse of 'x'
{ 
  ## Refers to cache if pre-existing.
  
  a <- x$getinv()
  
  if(!is.null(a)) {
    message("Displaying cached result...")
    return(a)
  }
  
  ##Solves matrix if not cached already, then creates a cache and returns it. 
  
  data <- x$get()
  a <- solve(data, ...)
  x$setinv(a)
  
  return(a)
}
## calculate the inverse matrix if once again for the same data 
## then gets result from cache

## create cache and set matrix to do inverse
## difference to example: change set function - if we assign the same matrix
## as last used then we not clear cached inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    usedM <- NULL
    
    set <- function(y){
      x <<- y
      if(!isTRUE(all.equal(y,getlastused()))){
        invM <<- NULL
      }
      
    }
    get <- function() x
    setinverse <- function(invmatrix, used){
      invM <<- invmatrix
      usedM <<- used
    } 
    getinverse <- function() invM
    getlastused <- function() usedM
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse,
         getlastused = getlastused)
}


## calculate or get result from buffor

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    current <- x$get()
    last <- x$getlastused()
    if(!is.null(x$getinverse()) && isTRUE(all.equal(current,last))){
        m <- x$getinverse()
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m, data)
    m
}
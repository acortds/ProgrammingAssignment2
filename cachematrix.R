## makeCacheMatrix function has the functions to calculate the inverse 
##    of a matrix and to store the solution into memory.
## Usage: variable <- makeCacheMatrix(matrix())
## Example:
## m1 <- matrix(rnorm(1:100), 10,10)
## cm1 <- makeCacheMatrix(m1)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

## cacheSolve function checks if the matrix has already been resolved before. 
##    If the solution is cached, then it will return the previously calculated 
##    values. If the solution is not cached, it will use the functions to  
##    calculate and store the results in memory.
## Usage: cacheSolve(makeCacheMatrix())
## Example:
## cacheSolve(cm1)
## sm1 <- cacheSolve(cm1)

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("using cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}

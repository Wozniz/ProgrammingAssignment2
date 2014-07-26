## Two functions create a special matrix (really a list) 
## and calculate the inverse of the matrix
## The inverse of the matrix is cached so the calculation can be done once and retrieved form memory


## The first functionn creates a list that contains 
## 1) a function to create a matrix,
## 2) a function to retrieve the matrix
## 3) a function to set the value of the inverse of the matrix
## 4) a function to retrieve the inverted matrix
## The input for makeCacheMatix is a square matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}



## Creat the inverse of the special matrix created above
## First check to see if the inverse has already been calculated and cached in getinverse
## If so, return the value of x$getinverse, if not, calculate the inverse and cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

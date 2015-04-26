makeCacheMatrix <- function(a = matrix()) {
    i <- NULL
    temp <- a[1]
    set <- function(a) {
        i <<- NULL
        temp <- a[1]
    }
    get <- function() a
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(of, ...) {
    i2 <- of$getinverse()
    if(!is.null(i2)) {
        message("getting cached data")
        return(i2)}
    else message("caching data")
    data <- of$get()
    i2 <- solve(data, ...)
    of$setinverse(i2)
    i2
}

## Test caching
test <- function() {
  X <- matrix(c(3,-2,-4,3),2,2)
  Mat <- makeCacheMatrix(X)
  for (n in 1:3) {
    Y <- cacheSolve(Mat)
    print(Y)
  }
}

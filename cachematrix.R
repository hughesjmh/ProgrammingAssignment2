## These function will allow for the creation of a matrix object that can 
#cache its inverse, and either ccompute the inverse of the matrix object 
#or retrieve the object from cache if it has already been computed

## create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) i <<- inverse
        getInv <- function() i
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## compute the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has 
#already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getInv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInv(i)
        i
}

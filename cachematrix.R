## makeCachMatrix will check if a matrix is stored in cache and return it if 
##       it is found.
## CachSolve will create a get an already computed inverse from cache 
##      if available. If it is not available, the inverse will be calculated 
##      and stored in cache.


## This function will return the cached inverse if it exist in cache. If the 
## does not exist it will return null.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse_value) m <<- inverse_value
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will attempt to get the inverse value from cache. And write it
## to cache if needed. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_value = x$getinverse()
        if (!is.null(inverse_value)){ ## check if inverse is in cache
                message("got inverse from cache")
                return(inverse_value)
                
                
        }
        
        ## we did not find the inverse...so it will be calculated and stored.
        data <- x$get() # create matrix for x
        inverse_value <- solve(data) # make inverse
        x$setinverse(inverse_value)
        inverse_value # return inverse
        
        
}

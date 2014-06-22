#### An interface to create a custom matrix that provide a cached for its inverse. 

## Given an optional matrix x, build another object representing
## the value of x with its inverse cached by a closure.
## The returned object provide four functions for setters and getters.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse<- function(inv) m <<- inv
        getinverse<- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns an matrix that is the inverse of the incoming matrix x 
cacheSolve <- function(x) {
        m <- x$getinverse()
        if(!is.null(m)) {
		# if the inverse is cached, just return it.
                message("getting cached inverse.")
                return(m)
        }
	# the inverse isn't cached, we have to calculate it.
        data <- x$get()
        m <- solve(data)
	# cache/rememeber the inverse for future look up.
        x$setinverse(m)
        m
}

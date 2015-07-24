# Solve function returns the inverse of a matrix, using a cache in makeCacheMatrix function.

# makeCacheMatrix takes a matrix as an argument and returns a list of functions.
# It sets and stores a matrix, then computes its inverse and caches it.

makeCacheMatrix <- function(x = matrix()) {
        #Initializes a cached matrix to NULL
        m <- NULL
        
        # Sets the value of a matrix x
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # Returns the value of the matrix x, stored in MakeCacheMatrix
        get <- function() x
        
        # stores the value of the input (inverse) in m  and returns this value
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        
        # Stores these 4 functions (set, get, setinverse, getinverse in a list)
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



# CacheSolve calculates and returns the inverse of a matrix x or returns the inverse value (m)
# stored in the cache function.

cacheSolve <- function(x, ...) {
        # Gets the cached value 
        m <- x$getinverse()
        
        # If there is a cached value, it returns it
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # Else, it gets the matrix, calculates its inverse and stores it in the cache
        data <- x$get()
        m <- solve(data) %*% data
        x$setinverse(m)
        
        # Returns the inverse
        m
}
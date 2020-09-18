########################### Week 3 Programming Assignment ###########################

## Put comments here that give an overall description of what your functions do
    # These functions will chance the inverse of a matrix. Matrix inversion can be a costly computation and there aresome benefits to chacing the inverse of a matrix rather than repeatedly compute it.

## Write a short comment describing this function
    #makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

# Create maxtrix (x)
makeCacheMatrix <- function(x = matrix()) 
{
    # Store inverse
    inv <- NULL
    # Set Matrix
    set <- function(y)
    {
        # <<- used to assign value to an object in a different environment than current environment
        x <<- y
        inv <<-NULL
    }
    # Retrieve x
    get <- function() x
    
    # Calculate inverse using solve(x) ["if X is a square invertible matrix, then solve(X) returns its inverse"]
    setInverse <- function() inv <<- solve(x)
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Write a short comment describing this function
    # cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
{
    # Return inverse of matrix 'x'
    inv <- x$getInverse()
    if(!is.null(inv))
    {
        message("getting cached matrix")
        return(inv)
    }
    data <- x$get()
    inv <-solve(matrix, ...)
    x$setInverse(inv)
    inv
}
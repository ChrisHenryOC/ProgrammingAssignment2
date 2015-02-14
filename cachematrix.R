## Computes and caches an inverse of a matrix.
## Requires that any matrix used be invertable.

## Functions:
##      makeCacheMatrix: Creates a function to store the inverse of a given matrix
##      cacheSolve: Uses solve() to calculate inverse of the cacheMatrix function passed in.
##                  If value previously calculated, returns cached inversion matrix

# makeCacheMatrix: Creates a function to store the inverse of a given matrix
#   Parameter x must be a matrix that can be inverted
makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL

    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() 
    {
        x
    }
    
    setmatrix <- function(data) 
    {
        inv <<- data
    }    

    getmatrix <- function() 
    {
        inv
    }
    
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## cacheSolve: Uses solve() to calculate inverse of the cacheMatrix function passed in.
##             If value previously calculated, returns cached inversion matrix
## Parameter x must be an object created with makeCacheMatrix above.
cacheSolve <- function(x, ...) 
{
    m <- x$getmatrix()
    
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}


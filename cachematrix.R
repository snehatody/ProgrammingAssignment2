#These functions caculate the inverse of a matrix and cache it for fture use

#This function creates the cache of the matrix
makeCacheMatrix <- function(x) {
	if(!is.matrix(x)) stop("x must be a matrix")
	if (nrow(x) != ncol(x)) stop("Non square matrix cannot be inversed")        
	i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function(inv) i <<- inv
        getInv <- function() i
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

#This function calculates the inverse of the matrix if it doesn't already exist in the cache
cacheSolve <- function(x, ...) {
        i <- x$getInv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInv(i)
        i
}


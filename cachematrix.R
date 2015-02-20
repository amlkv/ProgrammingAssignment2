## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly. These two functions do just that!

## This function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(mtx = matrix()) {

## The first step initialises imtx within the function as NULL.
        imtx <- NULL

## Now we define the four functions set, get, setinv and getinv.
        set <- function(vals) {
                mtx <<- vals
                imtx <<- NULL
        }
        get <- function() mtx
        setinv <- function(solve) imtx <<- solve
        getinv <- function() imtx        

## makeCacheMatrix will return a list of the four functions.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {

## If the inverse is already stored in imtx, the function will
## return it, accompanied by the message "getting cached data".
        imtx <- mtx$getinv()
        if(!is.null(imtx)) {
                message("getting cached data")
                return(imtx)
        }

## If the inverse isn't already stored, a solve() function is
## used to compute the inverse and store it in imtx.
        data <- mtx$get()
        imtx <- solve(data, ...)
        mtx$setinv(imtx)

## cacheSolve will return a matrix object, which contains the
## inverse of the matrix it got passed as data.
        imtx
}
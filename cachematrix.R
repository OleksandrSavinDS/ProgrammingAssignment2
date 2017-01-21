
## "makeCacheMatrix" is the special object, which can 
## store matrix and inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setcache <- function(cache) m <<- cache
    getcache <- function() m
    list(set = set, get = get, 
         setcache = setcache,
         getcache = getcache)
}


## "cacheSolve" function returns inverse matrix
## of x. First of all it checks is there precalculated
## inversed matrix and returns cache if exists
## In other case it calculate inversed matrix, caches
## and returns

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getcache()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    m <- solve(x$get())
    x$setcache(m)
    m
}
## Overall this function tries to save inverse of a matrix in cache
## If the matrix has inverse calculated already cache value will be returned
## Otherwise fresh inverse will be calculated and saved in cache

## This function creates a matrix with the cache functionality described above

makeCacheMatrix <- function(x = matrix()) {
    m<-matrix(data = NA)
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) m<<- solve
    getinverse<-function() m
    list(set=set, get=get,setinverse=setinverse, getinverse=getinverse)
}


## This function returns the cached value of inverse or calculates a fresh value

cacheSolve <- function(x, ...) {
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data, ...)
    x$setinverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
}

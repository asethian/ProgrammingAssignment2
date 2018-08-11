## Where x is a matrix, calculates the inverse of that matrix and stores it in cache 
## for retrieval if called thereafter.

## makeCacheMatrix stores a matrix and its inverse in a list and returns it to
## the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(solve) m<<- solve
        getinverse<-function()m
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## cacheSolve first attempts to retrieve the cached inverse matrix if previously
## called. Otherwise, it calculates the inverse matrix, caches it, and prints the result.

cacheSolve <- function(x, ...) {
        
        m<-x$getinverse()
        
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}

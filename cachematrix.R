## Solve the reverse of the matrix and get the reverse value from the cache

## Store a matrix and cache's its reverse
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    # set the value of the vector
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    # get the value of the vector
    get<-function() x
    # set the value of the reverse
    setsolve<-function(solve) m<<-solve
    # get the value of the reverse
    getsolve<-function() m
    list(set=set,get=get,
         setsolve=setsolve,
         getsolve=getsolve)
}


## calculate the reverse of the "vector" created with the above function.
cachesolve <- function(x, ...) {
    # first check to see if the reverse is already calculated.
    m<-x$getsolve()
    # if calculated, gets the reverse from the cache
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    # if not calculated, compute the reverse and set the value of 
    # the reverse in the cache via setSolve
    data<-x$get()
    m<solve(data,...)
    ## Return a matrix that is the inverse of 'x'
    x$setsolve(m)
    m
}

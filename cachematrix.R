## These functions cache and return inverese matrices for given square matrices

## This function returns R object wrapping given matrix to contain also an inverse matrix

makeCacheMatrix<-function(x=matrix()){
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get <- function() x
        setinverse<- function(inverse) i <<- inverse
        getinverse<- function() i
        list(set = set, get = get,
             setinverse=setinverse, 
             getinverse=getinverse)
}

## cacheSolve function computes and returns inverse matrix for given one if inverse matrix was not computed yet
## or returns cached inverse matrix 

cacheSolve<-function(x,...){
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        mi<-NULL
        
        set<-function(y){
                x<<-y
                mi<<-NULL
        }
        
        get<-function() x
        set_inverse<-function(inverse) mi<<-inverse
        get_inverse<-function() mi 
        
        list( set=set, get=get,
              set_inverse=set_inverse,
              get_inverse=get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        mi<-x$get_inverse()
        
        if (!is.null(mi)) {
                message("getting the cached data")
                
                return(mi)
                
        }
        
        data<-x$get()
        
        mi<-solve(data, ...)
        
        x$set_inverse(mi)
        
        mi
}

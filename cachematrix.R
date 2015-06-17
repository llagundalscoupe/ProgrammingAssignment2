## Put comments here that give an overall description of what your functions do 

        ## These functions create a special matrix like object that can cache it's inverse when available, and calculate the
        ## inverse of the matrix if the matrix has not been cached.


## Write a short comment describing this function

        ## The makeCacheMatrix function creates a special matrix object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        
        set<-function(y) {                
                x<<-y
                m<<-NULL
        }   
        get<-function()x 
        setinvm<-function(solve) m<<-solve
        getinvm<-function() m
        
        list(set=set,get=get,setinvm=setinvm, getinvm=getinvm)
}

## Write a short comment describing this function

        ## The cacheSolve function is used to create an inverse matrix, but first it checks to
        ## to see if the inverse matrix is already in cache. If the matrix exists in cache a message is printed and it returns m. 
        ## If the matrix is not in cache it proceeds to calculate the inverse matrix, set the inverse matrix, and return m.

cacheSolve <- function(x, ...) {      
        m<-x$getinvm() 
        if(!is.null(m)) {        
                message("getting cached data")
                return (m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinvm(m)
        m
}

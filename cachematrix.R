## Put comments here that give an overall description of what your
## functions do        

## Write a short comment describing this function

        ##This function creates a special matrix object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        ##sets the default values of the matrix in the makeCacheMatrix environment
        set<-function(y) {
                
                x<<-y
                m<<-NULL
        }
        ##gets the values of the matrix
        get<-function()x
        ##sets the values of the inverse matrix
        setinvm<-function(solve) m<<-solve
        ##gets the values of the inverse matrix
        getinvm<-function() m
        ##creates a list to store the above values
        list(set=set,get=get,setinvm=setinvm, getinvm=getinvm)
}


## Write a short comment describing this function
        ##This function is used to create an inverse matrix, but first it checks to
        ##to see if the inverse matrix is already in cache. If in cache it outputs a message and returns m, 
        ##and if not in cache it proceeds to calculate the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ##gets the cached value
        m<-x$getinvm() 
        ##checks the cached value to see if it should be used or proceed to calculate the inverse of the input matrix
        if(!is.null(m)) {        
                message("getting cached data")
                return (m)
        }
        ##If there was a NULL cached value the function proceeded and this calculates the inverse matrix, sets it, and then returns m
        data<-x$get()
        m<-solve(data,...)
        x$setinvm(m)
        m
}

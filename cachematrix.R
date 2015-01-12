## This is a collection of 2 functions - makeCacheMatrix() and cacheSolve(). The makeCacheMatrix() 
## 

## This function takes the initial matrix and stores it in a list. It also stores the inverse of the same matrix whenever setinverse() is called

makeCacheMatrix <- function(x = matrix()) {
        ##initialize a null varaible to store the inverse of the matrix passed
        m <- NULL
        
        ##function to set the value to the matrix passed. This matrix x has to be inversed
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ##This function will return the passed matrix to be inversed whenever called
        get <- function() x
        
        ##This function will set the inversed matrix m whenever called.
        setinverse <- function(solve) m <<- solve
        
        ##This function will get the inverse matrix m whenever called
        getinverse <- function() m
        
        ##This is the list which will essentially carry the cache of the intial and cached matrix.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function creates or fetches the inverse of the passed matrix (the argurment of function x) on multiple calls of the function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## Check if the received matrix is not null. It the returned matrix is not null, it means that the inverse was already created and cached.
        ## So no need to calculate the inverse again. Just return the value of m (i.e inverse of matrix) that we already have.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Now the control will come here for the firt time. Get the matrix passed for calcuating the inverse from the makeCacheVector() passed
        data <- x$get()
        
        ## Create the inverse of the data
        m <- solve(data, ...)
        
        ## Set the inverse to the list of makeCacheVector. This list will act as a cache on repeated calls to this function next time onwards
        x$setinverse(m)
        
        ##return m (inversed matrix) to print it on the console
        m
}

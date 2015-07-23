## These functions return the inverse of a matrix and also store their value
##in cache. This helps a lot while performing large Matrix Inverse Compuations.
## Since Matrix inversion is computation intensive, we can save  lot of time 
## and resources when we are doing it more than once.
## These functions are written only for square matrices as we use the inbuilt
## solve() for the inversion.

## makeCacheMatrix is a function to store a matrix and its cache
## It has the following list of functions associated
# 1.  setMatrix() to set the value of the matrix
# 2.  getMatrix() to get/return the value of the matrix
# 3.  setInverse() to set the inverse of a matrix
# 4.  getInverse() to get/return the inverse of a matrix
###############################################################
makeCacheMatrix <- function(x = matrix()) {
        #initializing the cache to null
        cache <- NULL
        ## set the value of the matrix
        set <- function(y){
                x <<- y
                cache <<- NULL
        }
        
        ##get the value of the matrix
        
        get <- function() x
        
        ##setting the inverse of a matrix
        
        setInverse <- function(inverse){
                ## setting the inverse
                cache <<- inverse
                
        }
        
        ## getting the inverse
        
        getInverse <- function() cache
        
        list(
                setMatrix = set,
                getMatrix = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
        

}


## This function is used to calculate the inverse of a matrix if the cached 
## matrix is NULL else it returns the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getInverse()
        if (!is.null(cache)) {
                message("getting cached data")
                return(cache)
                
        }
        data <- x$getMatrix()
        cache <- solve(data)
        x$setInverse(cache)
        cache
}

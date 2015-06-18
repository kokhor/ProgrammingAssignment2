## The purpose of the functions is to store some repeating processed data 
## in the memory (i.e. Cache) in order to avoid re-calculations, 
## and therefore effectively producing faster results.


## The two functions are 1) makeCacheMatrix and
##                       2) cacheSolve

## To execute these functions, users first have to load cachematrix.R, 
## then run makeCacheMatrix function and finally cacheSolve function.
## 
## e.g. at console >source('cachematrix.R')
##                 >a <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
##                 >cacheSolve(a)
## Output as below
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

makeCacheMatrix <- function(x = matrix()) {
 
        ## This function creates a special "matrix" object
        ## which stores or caches data of the inverse of the matrix
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvm <- function(invm) m <<- invm
        getinvm <- function() m
        list(set = set, get = get,
             setinvm = setinvm,
             getinvm = getinvm)                                   

}


cacheSolve <- function(x, ...) {
        ## Getting Inverse Matrix Cached Data or 
        ## calculate Inverse of the matrix if no Cached Data 
        
        ## Return a matrix that is the inverse of 'x'
       
        m <- x$getinvm()
        if(!is.null(m)) {
                message("getting inverse matrix cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvm(m)
        m
}

## makeCacheMatrix: Return a list of functions implementing a cached matrix.
## cacheSolve: Use the Solve funtion to compute the inverse of a cached matrix.

## makeCacheMatrix: Return a list of functions implementing a cached matrix.
##      The list of functions includes:
##      get(): Return the matrix
##      set(m): Set the matrix to m (Use to change the matrix)
##      getInverse(): return the inverse of the matrix
##      setInverse(i): set the inverse of the matrix to i (used by cacheSolve
##              and not meant to be used otherwise)

makeCacheMatrix <- function(mat = matrix()) {
        inv <- NULL
        
        ## Functions implementing the cached matrix interface
        set <- function(m) {
                mat <<- m
                inv <<- NULL
        }
        get <- function() mat
        setInverse <- function(i) inv <<- i
        getInverse <- function() inv
        
        ## Return the functions as a list with named values for easy access.
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## cacheSolve: Use the Solve funtion to compute the inverse of the matrix m. 
##      The inverse is cached to avoid recomputation.
##      Precondition: m must be an invertable square matrix.
##      A better design would be to place this functionality in getInverse
##              above so getInverse always returns the inverse (with solve
##              called if necissary).

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
        inv <- m$getInverse()
        if(!is.null(inv)) {
                ## To Do: remove (only for the exercise)
                message("returning cached inverse")
                return(inv)
        }
        mat <- m$get()
        inv <- solve(mat)
        m$setInverse(inv)
        inv
}

## cacheSolveTest: Minimal test of makeCacheMatrix and cacheSolve.
##      Create an cached matrix from an inverable matrix.
##      Print the cached matrix and its inverse (should be null since it has
##              not yet been computed).
##      Call cacheSolve on the cached matrix.
##      Print the inverse (not null now).
##      Call cacheSolve on the cached matrix. Should print message that the
##              cached inverse is being returned.
##      Print the inverse (not null now).

cacheSolveTest <- function() {
        m <- makeCacheMatrix(matrix(c(3,1,2,1),nrow=2,ncol=2))
        print("m: "); print(m)
        print("m$get(): "); print(m$get())
        print("m$getInverse(): "); print(m$getInverse())
        i <- cacheSolve(m)
        print("i: "); print(i)
        print("m$getInverse(): "); print(m$getInverse())
        i <- cacheSolve(m)
        print("i: "); print(i)
}

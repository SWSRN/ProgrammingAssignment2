## Put comments here that give an overall description of what your
## functions do
# These functions create an object definition (with methods) which 
# store a matrix and its inverse, so that the inverse only needs to be 
# calculated once. 

makeCacheMatrix <- function(matCached = matrix()) {
    # makeCacheSolve returns an "object" for a matrix marCached 
    # with function methods $set, $get, $getinv and $setinv.
    # Caches original matrix in a global matCached.
    # Caches's mat's inverse in a global variable invCached.
    
    invCached <- NULL
    set <- function(newmat) {  # defaults to this function if not specified
        matCached <<- newmat
        invCached <<- NULL
    }
    get <- function() {
        return(matCached)
    }
    setinv <- function(inv) {
        invCached <<- inv
    }
    getinv <- function() {
        # str(invCached)
        return(invCached)
    }
    return(list(  # return list of functions which are ab object's methods
        set = set, get = get, setinv = setinv, getinv = getinv))
}


## Write a short comment describing this function

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
    # Makes a R object of type makeCacheSolve for a matrix mat and
    # calculates and stores the inverse if not done already. 
    # Returns the inverse matrix.
    inv <- mat$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    } else {
        message("solving inverse")
        #        str(mat)
        data <- mat$get()
        # print (data)
        inv <- solve(data, ...)
        mat$setinv(inv)
        return(inv)
    }
}


test <- function(){
    # test function for cacheSolve and the object type makeCacheSolve.
    m <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
    message ("test: m=", m)
    #print (makeCacheSolve(m))
    mm <- makeCacheSolve(m)
    #    message ("test: m=", m)
    message ( "test after makeCacheSolve (methods): mm =", mm)
    message("test: mm$get()", mm$get())
    print(mm$get())
    
    message("test: mm$getinv()", mm$getinv())
    mm$getinv()
    
    message()
    # inverse not defined yet.
    #    minv <- cacheSolve(mm)
    #    message("test after cacheSolve minv=")
    #    minv
    
    message()
    message('Test another matrix')
    m2 <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
    print(m2)
    mm2 <- makeCacheSolve(m2)
    minv2 <- cacheSolve(mm2)
    print(minv2)
    
    message()
    message('Test another matrix')
    m3 <- matrix(c(1, 2, 1, 1), nrow = 2, ncol = 2, byrow = TRUE)
    print(m3)
    #    message("Call MakeCacheSolve")
    mm3 <- makeCacheSolve(m3)
    #    message("Call cacheSolve")
    minv3 <- cacheSolve(mm3)
    print(minv3)

}




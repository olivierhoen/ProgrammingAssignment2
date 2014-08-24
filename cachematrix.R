## caching the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL    #  m will store our 'inverse' and it's reset to NULL every 
  #    time makeCacheMatrix is called
  
  #  note these next three functions are not run when makeCacheMatrix is called.
  #   instead, they will be used by cacheSolve() to get values for x or for
  #   m (inverse) and for setting the inverse.  
  #  These are usually called object 'methods'
  
  set <- function(y) {    # takes an input matrix
    x <<- y         # saves the input matrix 
    m <<- NULL      # resets the inverse to NULL, basically what happens when a new object is generated.
  }
  
  get <- function() { x }   # this function returns the value of the original matrix
  
  setmatrix <- function(solve)  # this is called by cacheSolve() during the first cacheSolve()
  { m <<- solve }  #  access and it will store the value using superassignment
  
  getmatrix <- function() { m } # this will return the cached value to cachemSolve() on
  #  subsequent accesses
  
  list(set=set, get = get,          #  This list is returned with the newly created object.       
       setmatrix = setmatrix,  #   It lists all the functions ("methods") that are part of
       getmatrix = getmatrix)  #   the object.  If a function is not on the list then it cannot
  #   be accessed externally.
  
}
  

## `cacheSolve` function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()               # accesses the object 'x' and gets the value of the inverse
  if(!is.null(m)) {              # if inverse was already cached (not NULL) ...
    
    message("getting cached data")  # ... send this message to the console
    return(m)                       # ... and return the inverse ... "return" ends 
    #   the function cachesolve(), note
  }
  matrix <- x$get()        # we reach this code only if x$getmatrix() returned NULL
  m <- solve(matrix, ...)   # if m was NULL then we have to calculate the inverse
  x$setmatrix(m)           # store the calculated inverse value in x (see setmatrix() in makeCacheMatrix)
  m                      # return the inverse to the code that called this function
}

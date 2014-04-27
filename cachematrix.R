###############################################################################
##
##  Purpose:	This function creates a special "matrix" object that can cache its 
##				inverse.
##	
##	set :		Function to Set SET value of the matrix and set the inv to NULL
##				in the Global environment (parent) for later retrieval.
##	
##	get :		Function to return (Get) the value of the matrix.
##
##	setInverse:	Funtion to SET the value of x (matrix) to be available to the 
##				parent environment by ussing the <<- operator.
##
##	getInverse:	Function to return the inverse of the matrix i.e. the value in 
##				the Global environment.
##	
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
  # initialize the stored inverse value to NULL
  inv <- NULL
  
  # to set the value of the matrix
  set <- function(y) {
    x <<- y
    
    # Since we assigned a new value to the matrix (x <<- y) the value of the 
    # inverse must change also, thus we set it to NULL.
    
    inv <<- NULL
  }
  
  
  get <- function() {
    x
  }
  
  # to set the inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  # to get the inverse
  getInverse <- function( ) {
    inv
  }
  
  
  # return a list of all the above functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)    
}



###############################################################################
##
##	Purpose:This function computes the inverse of the special "matrix" returned 
##			by makeCacheMatrix above. If the inverse has already been 
##			calculated (and the matrix has not changed), then  cacheSolve  should
##			retrieve the inverse from the cache.Calculates the inverse of the 
##			"special" matrix created with "makeCacheMatrix" function. 
##	
##	Flow:	1. checks to see if the inverse has already been calculated. 
##			2. If so, gets the inverse from the cache instead of computing 
##			it.
##			3. Else calculates the inverse of the and sets the value of the
##			inverse in the cache via the setInverse function.
##
###############################################################################

cacheSolve <- function(x, ...) 
{
  # check if the inverse is already calculated (cached), if it ism return it
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Acquiring cached inverse")
    return(inv)
  }
  # If not cached, we get the matrix into data
  newMatrix <- x$get()
  
  # and compute the inverse
  inv <- solve(newMatrix, ...)
  
  # then cache the inverse
  x$setInverse(inv)
  # and return it as well
  return(inv)
}

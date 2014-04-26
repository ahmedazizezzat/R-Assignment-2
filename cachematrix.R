## overall Comments: 
#the code consists of two functions: The first function is used to create a special matrix
#It contains 4 embedded functions: 
## set: used to set the value of the matrix
## get: used to get the value of the matrix
## setinverse: used to set the value of the inverse matrix
## getinverse: used to get the value of the inverse matrix
## the second function is used to evaluate the inverse of the special matrix
#either by retrieving it from the cache or by computing it (if not in the cache) 
# and then saving it to the cache for further use. 

#the makeCacheMatrix is a function (with 4 embedded functions) that creates 
# a special invertible square matrix that can cache its inverse. It takes 
#x (the created matrix as its argument) and returns a list of 4 elements as an output.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function(){
    x
  }
  setinverse <- function(inverse){ 
    i <<- inverse
  }
  getinverse <- function(){ 
    i
  }
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) 
}
}


## the cacheSolve is a function that either computes or retrieves 
## the inverse of the matrix created by the "makeCacheMatrix" function according
## to whether the value of the inverse is stored in the cache or not

cacheSolve <- function(x, ...) {
  i <- x$getinverse()                 
  if(!is.null(i)) {           
    message("getting cached data")
    print(i)                 
  }
  data <- x$get()             
  i <- solve(data, ...)       
  x$setinverse(i)             
  print(i)                    
}


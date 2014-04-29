## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function makeCacheMatrix converts a matrix into  a special type of matrix
# in which it is possible to store the inverse of the original matrix
# as well as return the value of both the original and the inverse matrix

makeCacheMatrix <- function(x = matrix()){
   x_inverse   <- NULL;

   set         <- function(y){
      x           <<- y;
      x_inverse   <<- NULL;
   }
   get         <- function(){
      x;
   }
   setinverse  <- function(inverse){
      x_inverse   <<- inverse;
   }
   getinverse  <- function(){
      x_inverse;
   }

   list(
      set         = set,
      get         = get,
      setinverse  = setinverse,
      getinverse  = getinverse
   )
}

## Write a short comment describing this function

## The function cacheSolve receives a makeCacheMatrix argument. This function
# will compute the inverse of a matrix if the inverse of the argument is null
# or it will return the chached valuo of the argument.

cacheSolve  <- function(x, ...){
   ## Return a matrix that is the inverse of 'x'
   x_inverse <- x$getinverse();
   if(is.null(x_inverse)){
      print("Calculating inverse of matrix.");
      x_inverse <- solve(x$get());
      x$setinverse(x_inverse);
   }else{
      print("Cached matrix found.");
   }

   return (x_inverse);
}
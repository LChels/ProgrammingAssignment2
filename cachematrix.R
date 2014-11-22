## Hi. The functions below create a makeCacheMatrix 
## object which "binds" the matrix and its inverse together, such that
## we do not have to recalculate an inverse of the same matrix multiple times

## makeCacheMatrix initializes the object
## input: a (non-invertible) matrix x
## output: a makeCacheMatrix object with properties enabling
##         initialization and calls to the original matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
			#create variable for inverse in the scope of makeCacheMatrix
			#set to 'NULL' so that cacheSolve does not return a val on the 1st calculation
			inv <- NULL
			
			#sets the value of the matrix
            setX <- function(y) x <<- y

			#displays the matrix
            getX <- function() x
            
			#sets the valule of the inverse m to invVal
			setInv <- function(invVal) inv <<- invVal
            
			#displays the inverse
			getInv <- function() inv
			
			#returns a vector that stores the results of the set and get functions 
            list(setX = setX, getX = getX,
                 setInv = setInv,
                 getInv = getInv)

}


## cacheSolve outputs the inverse of the m<-round(matrix(runif(25,5,15),5,5),0)
m_obj<-makeCacheMatrix(m)
cacheSolve(m_obj)
cacheSolve(m_obj)matrix stored in the 
##      given makeCacheMatrix object by either outputting the stored inverse
##      or calculating the inverse if there is no stored inverse available
## input: a makeCacheMatrix object
## output: the inverse of the matrix x$getX and storing the inverse as x$getInv 

cacheSolve <- function(x, ...) {
		#let some value = the 'getInv' property of x
		someInv <- x$getInv()
		
		#if 'getInv' is not NULL, then return it as the cached inverse and exit function
		if(!is.null(someInv)){
			message("Inverse was already cached")
            return(someInv)
            }
		
		#if 'getInv' is NULL, then:
		###let data be the matrix
		dataM <- x$getX()
		###calculate the inverse, set someInv to it
		someInv <- solve(dataM, ...)
		###set it as x's inverse
		x$setInv(someInv)
		###and return in
		someInv
				
}

## EXAMPLE:
m<-round(matrix(runif(25,5,15),5,5),0)
m_obj<-makeCacheMatrix(m)
cacheSolve(m_obj)
cacheSolve(m_obj)

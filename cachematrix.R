## These functions are designed to store on cache the inverse matrix of a 
## determined matrix, so that we won't have to calculate it every time. I would like
## to credit the user Allan Cameron from stack overflow. He reviewed my first design
##for the assignment and helped with fixing it,so this is inspired by his own code.

##The function makeCacheMatrix works as an environment for the variable m,
##which is used as a control for the cache status, and as environment for the 
##unnamed function that stores the inverse matrix. It has to be executed before cache solve
##(you can't check if something is on cache if the cache hasn't been created)
makeCacheMatrix <- function(x = matrix()) {

	m<<-NULL		
	function(){		
		list(set=function()x<<-x,inverse=function() m<<-solve(x),cache=m)		
}

## Write a short comment describing this function

##This function takes the m defined in makeCacheMatrix and evaluates it
##if the unnamed function on makeCacheMatrix's hasnt been executed yet,it is going to calculate
##the inverse matrix and store it on the cache. Otherwise,it just returns the value
##already stored in the memory
cacheSolve <- function(y, ...) {
		## Return a matrix that is the inverse of 'x'
		y()$set()
		n<-y()$cache
		if (!is.null(n)){
			print("Loading from cache...s")
			print("The inverse matrix is:")
			return(n)
		}
		print("Calculating.../n")
		y()$inverse()
		print("The inverse matrix is:")
		y()$cache
}

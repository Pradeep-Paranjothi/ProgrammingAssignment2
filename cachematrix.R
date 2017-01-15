#----------------------------------------------------------------------------------------------------------------------------
#cacheSolve week3 assignment by Pradeep Paranjothi
#overall Objective: Caching the Inverse of a Matrix
#Date: 16-Jan-2017
#----------------------------------------------------------------------------------------------------------------------------
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#Initialize variable x to matrix data type, argument to the makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) { 
        #Initialize inverse matrix to NULL
        m <- NULL 
        
        set <- function(y) {
                #Assign the input argument to the x object in the parent environment
                x <<- y 
                #Assign the value of NULL to the m object in the parent environment
                m <<- NULL 
        }
        
        # Since the symbol x is not defined within get(), R retrieves it from the parent environment of makeCacheMatrix()
        get <- function() x 
        
        #Since m is defined in the parent environment and we need to access it after set_mat_inv() completes, the code uses the <<- form of the assignment operator to assign the input argument to the value of m in the parent environment.
        set_mat_inv <- function(inv) m <<- inv
        
        #defines the getter for the matrix inverse m. Just like the getter for x, R takes advantage of lexical scoping to find the correct symbol m to retrieve its value.
        get_mat_inv <- function() m
        
        #Assigns each of these functions(set,get,set_mat_inv,get_mat_inv) as an element within a list(), and returns it to the parent environment.
        list(set = set, get = get,
             set_mat_inv = set_mat_inv,
             get_mat_inv = get_mat_inv)
}

#cacheSolve:This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        #Retrieve the matrix inverse from the cache object
        m <- x$get_mat_inv() 
        
        #check if the value of the cache object is not null. If yes, return the value of the cache object, else proceed further
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)
        }
        
        #The below path would get executed only if the cache object value is null(i.e.first call to a matrix)
        #Retrieve input matrix x and assign it to the variable data
        data <- x$get() 
        #Use internal R function "solve" to inverse the matrix in the variable data and assign to m
        m <- solve(data, ...) 
        #Assign inverse matrix to m
        x$set_mat_inv(m)
        #Return Inverse Matrix of the input argument x
        m 
}

# Test Matrix Inverse
# c1=rbind(c(4,7), c(2,6)) 
# c1
# solve(c1)
# myMatrix_object <- makeCacheMatrix(c1)
# myMatrix_object$get()
# cacheSolve(myMatrix_object) 

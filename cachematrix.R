
## _____________________________________________________________________
## makeCacheMatrix - this function is passed a matrix and it creates an |
## object of type 'list', that stores two things - the original matrix  |
## value and what will be the cached value, which is initially set to   |
## 'NULL' and there are four functions -two to read the values of the   |
## two things we are storing and two functions to change them.          |
## 1.set the value of a matrix                                          |
## 2.get the value of a matrix                                          |        
## 3.set the value of the inverse                                       |
## 4.get the value of the inverse                                       |
## _____________________________________________________________________|


makeCacheMatrix <- function(x = matrix()) {     #input x will be a matrix
        inv_mat <- NULL                 #inv_mat is the value of matrix
                                        #inverted - is reset everytime
                                        #makeCacheMatrix is called
        
        
        # This code reinitializes the two object stored values
             set <- function(y) {       #takes an input matrix 
                         x <<- y        #saves the input matrix
                         inv_mat <<- NULL       #resets the inverse matrix 
                                                #to NULL
                     }
        
             get <- function() x        #this function returns the value
                                        #of the original Matrix
                                        
             setinverse <- function(inverse) inv_mat <<- inverse 
                #this is called by cacheSolve during the first cachesolve()
                                        
             getinverse <- function() inv_mat
                #this will return the cached value to cacheSolve
                #on subsequent accesses
        
             list(set = set, get = get,
                             setinverse = setinverse,
                             getinverse = getinverse)
                #This is accessed each time makeCacheMatrix is called,
                #or each time we make a new object. This is a list of the
                #internal functions ('methods') so a calling function
                #knows how to access those methods

}

## _____________________________________________________________________
## cacheSolve -  calculates the inverse of the                          |   
## special "matrix" created with the above function, however it         |        
## first checks if the inverse has already been calculated. If so,      |
## it gets the inverse from the cache and skips computation.            |
## Otherwise it calculates the inverse of the data and sets the         |
## value of the inverse in the cache via the setinverse function.       |        
## _____________________________________________________________________|

cacheSolve <- function(x, ...) {
        #The input x is an object created by makeCacheMatrix

        inv_mat <- x$getinverse()
                #accesses the object 'x' and gets the value of the inverse 
                if(!is.null(inv_mat)) {
                     #if inverse was already cached (not NULL)
                         
                        message("getting cached matrix")
                         #send a message to console
                         
                        return(inv_mat)
                         #return the inverse matrix..."return" ends
                     }
                data <- x$get()                 #this is in the case x$getinverse was 'NULL'
                                                #this function loads the new matrix to data
                
                inv_mat <- solve(data, ...)     #calculate the inverse matrix
                
                x$setinverse(inv_mat)           #store the calculated inverse matrix
                                                #in x (see setinverse in makeCacheMatrix)        
             
                inv_mat                         # return the inverse to the code that called
                                                # this function
}

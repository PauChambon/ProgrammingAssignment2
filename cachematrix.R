## This first unction creates a special "matrix object" that stores a matrix itself and its inverse.


makeCacheMatrix <- function(x = matrix()) {  ## creates the matrix object that takes in a matrix 'x' 
        inverse<-NULL ## erases any cached inverse mean that may exist in variable
        set<-function(y){  ## method/function saves a new matrix 'y' we may want to insert into matrix object 
                x<<-y  ## copies the matrix 'y' into variable x in parent scope 
                inverse<<-NULL ## erases old cached inverse from previous matrix in parent scope 
        }
        get<-function()x  ## method/function returns x stored in matrix object 
        setinverse<-function(solve) inverse<<-solve   ##stores incoming new inverse, calculated elsewhere 
        getinverse<-function()inverse ## method returns stored inverse
        list(set=set,               ## exposed methods for getting access to the matrix 'x' and to the inverse
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)  
}

##The next function returns the inverse of the matrix  if it exists,
## otherwise the function will calculate the inverse matrix using the matrix object 
## and return it. 

cacheSolve <- function(x, ...) {
        inverse<-x$getinverse()  ## use method to get inverse matrix from 'x' 
        if(!is.null(inverse)){   ## if 'x' already has a inverse, return it 
                message('getting cached data')
                return(inverse)
        }
        data<-x$get()   ## otherwise, get matrix from the object 
        inverse<-solve(data,...) ##calcuate the inverse matrix
        x$setinverse(inverse) ## store the inverse matrix in the object 'inverse'
        return(inverse) ## return the inverse matrix to the parent scope
}

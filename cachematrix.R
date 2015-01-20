## The functions below create both a matrix & it's inverse. They are cached using R's " <<-" operator.
## This is specially useful when calculating the inverse takes time, so calculating once and storing those matrices may save processor time.



## makeCacheMatrix receives a matrix and returns a list of functions to set & get the received matrix and its inverse.
makeCacheMatrix<- function(x = matrix()){
        inv<- NULL
        
        setmatrix<- function(y){ 
                x<<-y
                inv<<-NULL
        }
        
        getmatrix<- function() x
        
        getinverse<- function() inv
        
        setinverse<- function(inverse){
                inv<<-inverse
        }
        
        list(getmatrix = getmatrix, setmatrix = setmatrix, getinverse = getinverse, setinverse = setinverse)
        
}




## cacheSolve receives a matrix and returns its cached inverse. 
## If the inverse was not calculated yet, cacheSolve calculates, caches and returns the inverse.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        
        if(!is.null(inv)){
                print("Returned from cache")
                return(inv)
        }
        
        matrix<- x$getmatrix()
        inv<- solve(matrix)
        x$setinverse(inv)
        inv
        
}


## The following commands were used to test the functions
# > source("cachematrix.R")
# > x<- rbind(c(1,1), c(-1,2))
# > x
# [,1] [,2]
# [1,]    1    1
# [2,]   -1    2
# 
# > m<- makeCacheMatrix(x)
# 
# > cacheSolve(m)
# [,1]       [,2]
# [1,] 0.6666667 -0.3333333
# [2,] 0.3333333  0.3333333
# 
# > cacheSolve(m)
# [1] "Returned from cache"
# [,1]       [,2]
# [1,] 0.6666667 -0.3333333
# [2,] 0.3333333  0.3333333

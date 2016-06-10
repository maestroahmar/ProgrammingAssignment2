##The first function is meant for getting and setting the original matrix and 
## inverted matrix where as the second function is used to cache the given matrix or if already chached then retrive the
## inverted function with out actually calculating it again.



##This function is meant for getting and setting the original matrix and inverted matrix

makeCacheMatrix <- function(x = matrix())
{
inv_ori <- NULL
seto <- function(y)  x <<- y
geto <- function()   x
seti <- function(i)  inv_ori <<- i
geti <- function()  inv_ori
list(seto = seto, geto = geto, seti = seti, geti = geti)
}

## function is used to cache the given matrix or if already chached then retrive the
## inverted function with out actually calculating it again.
cacheSolve <- function(x, ...) {
        i <- x$geti()
        if(!is.null(i)) {
                message("getting cached Matrix")
                return(i)
        }
        data <- x$geto()
        i <- solve(data, ...)
        x$seti(i)
        i
}

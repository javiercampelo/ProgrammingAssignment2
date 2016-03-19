# makeCacheMatrix crea una lista que contiene:
# 1. establecer el valor de la matriz
# 2. obtener el valor de la matriz
# 3. establece el valor de inversa de la matriz
# 4. obtener el valor de la inversa de la matriz

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# La siguiente funcion devuelde la inversa de la matriz, comprobando previamente 
# se ha calculado previamente. Si se ha calculado, devuelve el resultado sin realizar 
# el calculo. En caso contrario, lo calcula.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("El dato está cacheado")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

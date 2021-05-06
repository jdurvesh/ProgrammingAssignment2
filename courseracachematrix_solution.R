makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

myVector<-makeVector(c(1:15))
cachemean(myVector)

myVector$set(c(1:10))

cachemean(myVector)


### 

make_matrix <- function(x=matrix())
{
 mat_inv<-NULL

 set_mat<-function(y)
 {
   x<<-y
   mat_inv<<-NULL
   
 }
 
 get_mat<-function() x 
 
 set_inv<-function(inv)
 {
   mat_inv<<-inv
 }
 
 get_inv<-function() {
   mat_inv
 }
 
 list(set=set_mat,
      get = get_mat,
      set_inv=set_inv,
      get_inv=get_inv)
 
}

cache_Matrix <- function(x,...)
{
  inv<-x$get_inv()
  if(!is.null(inv))
  {
    message("getting cached matrix")
    return(inv)
  }
  
  data<-x$get()
  inv<-solve(data,...)
  x$set_inv(inv)
  inv
  
}

m <- matrix(c(0, 1, 3, 0), nrow = 2, ncol = 2, byrow = TRUE)

new_matrix<-make_matrix(m)

cache_Matrix(new_matrix)

cache_Matrix(new_matrix)


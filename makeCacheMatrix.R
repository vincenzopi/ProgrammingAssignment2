#Matrix inversion is usually a costly computation and there may be some benefit
#to caching the inverse of a matrix rather than compute it repeatedly.

#Function makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix<- function(x = matrix()) {
  my_out <- NULL
  set <- function(y) {
    x <<- y
    my_out <<- NULL
  }
  get <- function() x
  #
  #Define set
  setInvMtr <- function(inv_matrix) my_out <<- inv_matrix
  #Define get
  getInvMtr <- function() my_out
  #
  list(set = set, get = get,
       setInvMtr = setInvMtr,
       getInvMtr = getInvMtr)
}

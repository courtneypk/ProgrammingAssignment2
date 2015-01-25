makeFibonacci <- function(x = NULL) {
  fv <- NULL

  set <- function(y) {
    x <<- y
    fv <<- NULL
  }

  get <- function() x

  setSeq <- function(seq) fv <<- seq
  
  getSeq <- function() fv
  
  list(set = set,
       get = get,
       setSeq = setSeq,
       getSeq = getSeq)
}

cacheFib <- function(x, ...) {
  seq <- x$getSeq()
  if(!is.null(seq)) {
    message("getting cached sequence")
    return(seq)
  }
  message("creating new sequence")
  len <- x$get()
  seq[1] <- 1
  seq[2] <- 2
  for(i in 3:len) {
    seq[i] <- seq[i-1] + seq[i-2]
  }
  x$setSeq(seq)
  seq
}

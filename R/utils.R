.IndexEnv <- new.env()
class(.IndexEnv) <- c("indexed_db", "environment")

indexenv <- function() .IndexEnv

localtime <- function(x)
  structure(x, class=c("POSIXt","POSIXct"), tzone=attr(x,'tzone'))

length.indexed <- function(x) {
  # indexed objects may have data and/or ordered slots, but need at least one
  # all need to be of equal length
  if(inherits(x,"indexed_list")) {
    sum(sapply(x,function(L) if(is.null(L$o)) length(L$d) else length(L$o)))
  }
  else {
    if(is.null(x$o))
      length(x$d)
    else length(x$o)
  }
}

print.indexed_db <- function(x, ...) {
  objects <- ls(x)
  if(length(objects) > 0) {
    NR <- length(get(objects[1],x))
    NC <- length(objects)
  } else {
    NR <- NC <- 0L
  }
  cat("Indexed Environment:",NR,"rows by",NC,"columns\n")
}

dimnames.indexed_db <- function(x) {
  list(NULL, ls(x))
}

`dimnames<-.indexed_db` <- function(x, value) {
  if(!is.list(value))
    stop("dimnames must be a list")
  attr(x, ".Dimnames") <- list(NULL, value[[2]])
}

dim.indexed_db <- function(x) {
  objects <- ls(x)
  if(length(objects) > 0) {
    NR <- length(get(objects[1],x))
    NC <- length(objects)
  } else {
    NR <- NC <- 0L
  }
  c(NR,NC)
}

.IndexEnv <- new.env()
class(.IndexEnv) <- c("indexed_db", "environment")

indexenv <- function() .IndexEnv

localtime <- function(x)
  structure(x, class=c("POSIXt","POSIXct"), tzone=attr(x,'tzone'))

length.indexed <- function(x) {
  if(inherits(x,"indexed_list"))
    sum(sapply(x,function(L) length(L$d)))
  else
    length(x$d)
}

print.indexed_db <- function(x, ...) {
  objects <- ls(x)
  NR <- length(get(objects[1],x))
  NC <- length(objects)
  cat("Indexed Environment:",NR,"rows by",NC,"columns\n")
}

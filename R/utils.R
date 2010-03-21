.IndexEnv <- new.env()
class(.IndexEnv) <- c("indexed_db", "environment")

indexenv <- function() .IndexEnv

localtime <- function(x)
  structure(x, class=c("POSIXt","POSIXct"), tzone=attr(x,'tzone'))

print.indexed_db <- function(x, ...) {
  objects <- ls(x)
  NR <- length(get(objects[1],x)$d)
  NC <- length(objects)
  cat("Indexed Environment:",NR,"rows by",NC,"columns\n")
}

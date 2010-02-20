.IndexEnv <- new.env()
class(.IndexEnv) <- c("indexed_db", "environment")

localtime <- function(x)
  structure(x, class=c("POSIXt","POSIXct"), tzone=attr(x,'tzone'))

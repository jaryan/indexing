.IndexEnv <- new.env()
class(.IndexEnv) <- c("indexed_db", "environment")

indexenv <- function() .IndexEnv

localtime <- function(x)
  structure(x, class=c("POSIXt","POSIXct"), tzone=attr(x,'tzone'))

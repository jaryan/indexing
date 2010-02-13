searchIndexGT <- function(column, x) {
  if(missing(column))
    stop("column must be specified")
  #.x <- get(".IndexEnv", .GlobalEnv)[[column]]
  .x <- .IndexEnv[[column]]
  binsearch <- xts:::binsearch
  if(!is.null(.x$l) && is.character(.x$l)) {
    x <- which(x==.x$l)
  } 
  i <- binsearch(x, .x$s, TRUE):length(.x$s)
  .x$o[i]
}


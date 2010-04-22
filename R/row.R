# methods to allow logical combining of searchIndex
# results

`|.rowid` <- function(e1,e2) {
  # use of bitmap indexing is very experimental. No clear idea of 
  # best way to implement this yet
  if(!is.null(attr(e1,"bitmap"))) {
    b1 <- attr(e1,"bitmap")
  }
  if(!is.null(attr(e2,"bitmap"))) {
    b2 <- b1 | attr(e2,"bitmap")
  }
  if(!is.null(attr(e2,"bitmap")) && !is.null(attr(e1,"bitmap"))) {
    message('using bitmaps')
    structure(as.which(b2), bitmap=b2, class="rowid")
  } else structure(unique(c(e1,e2)), class="rowid")
}

`&.rowid` <- function(e1,e2) {
  structure(intersect(e1,e2), class="rowid")
}

print.rowid <- function(x, ...) {
  nr <- length(unlist(x))
  if(nr == 0 || nr > 1)
    message(paste(nr,'hits'))
  else
    message('1 hit')
}


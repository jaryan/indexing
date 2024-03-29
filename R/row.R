# methods to allow logical combining of search_index
# results

as.rowid <- function(x) {
  UseMethod("as.rowid")
}

as.rowid.bitmap <- function(x) {
  .Call("which_bits", x)
}

as.rowid.rowid <- function(x) {
  x
}

as.rowid.list <- function(x) {
  structure(unlist(lapply(x, as.rowid)),class="rowid")
}

as.rowid.default <- function(x) {
  if(is.null(x)) {
    structure(list(), class="rowid")
  } else {
    structure(x, class="rowid")
  }
}

`&.rowid` <- function(e1,e2) {
  if(is.list(e1) || is.list(e2)) {
    if(!is.null(getOption("use.bit")) && getOption("use.bit")) {
      tmp <- lapply(1:length(e1), function(N) e1[[N]] & e2[[N]])
      return(tmp)
    } else {
      if(!is.list(e1))
        e1 <- rep(list(e1), length(e2))
      if(!is.list(e2))
        e2 <- rep(list(e2), length(e1))
      tmp <- vector("list", length(e1))
      tmp <- lapply(1:length(e1),
                      function(N) structure(intersect(e1[[N]],e2[[N]]),
                                            class="rowid"))
      structure(tmp, class="rowid")
    }
  } else {
    if(!is.null(getOption("use.bit")) && getOption("use.bit")) {
    if(length(e1) == 0 || length(e2) == 0)
      return(logical(0))
    if(!is.bitmap(e1)) {
      e1.bitmap <- bitmap_template(attr(e1, "length"))
      e1.bitmap[e1] <- TRUE
    } else e1.bitmap <- e1
    if(!is.bitmap(e2)) {
      e2.bitmap <- bitmap_template(attr(e1, "length"))
      e2.bitmap[e2] <- TRUE
    } else e2.bitmap <- e2
    .Call("bitmap_dlogical", e1.bitmap, e2.bitmap, 1L)
    } else {
    structure(intersect(e1,e2), length=attr(e1,"length"), class="rowid")
    }
  }
}
`|.rowid` <- function(e1,e2) {
  if(is.list(e1) || is.list(e2)) {
    if(!is.null(getOption("use.bit")) && getOption("use.bit")) {
      tmp <- lapply(1:length(e1), function(N) e1[[N]] | e2[[N]])
      return(tmp)
    } else {
      if(!is.list(e1))
        e1 <- rep(list(e1), length(e2))
      if(!is.list(e2))
        e2 <- rep(list(e2), length(e1))
      tmp <- vector("list", length(e1))
      tmp <- lapply(1:length(e1),
                      function(N) structure(unique(c(e1[[N]],e2[[N]])),
                                            class="rowid"))
      structure(tmp, class="rowid")
    }
  } else {
    if(!is.null(getOption("use.bit")) && getOption("use.bit")) {
    if(length(e1) == 0 || length(e2) == 0)
      return(logical(0))
    if(!is.bitmap(e1)) {
      e1.bitmap <- bitmap_template(attr(e1, "length"))
      e1.bitmap[e1] <- TRUE
    } else e1.bitmap <- e1
    if(!is.bitmap(e2)) {
      e2.bitmap <- bitmap_template(attr(e1, "length"))
      e2.bitmap[e2] <- TRUE
    } else e2.bitmap <- e2
    .Call("bitmap_dlogical", e1.bitmap, e2.bitmap, 2L)
    } else {
    structure(unique(c(e1,e2)), length=attr(e1,"length"), class="rowid")
    }
  }
}

print.rowid <- function(x, ...) {
  nr <- length(unlist(x))
  if(nr == 0 || nr > 1)
    message(paste(nr,'hits'))
  else
    message('1 hit')
}

..c.rowid <- function(...) {
  x <- list(...)
  if(length(x) > 2) stop("can only 'c' two rowid objects")
  if(length(x) == 1) return(unlist(x))
  if(is.list(x[[1]])) {
    return(lapply(1:length(unclass(x[[1]])), function(n) c(x[1],x[2])))
  } else {
    structure(c(x[[1]], x[[2]]), class="rowid")
  }
}

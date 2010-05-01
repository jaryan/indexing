# methods to allow logical combining of searchIndex
# results

`&.rowid` <- function(e1,e2) {
  if(is.list(e1) || is.list(e2)) {
    if(!is.list(e1))
      e1 <- rep(list(e1), length(e2))
    if(!is.list(e2))
      e2 <- rep(list(e2), length(e1))
    tmp <- vector("list", length(e1))
    #for(i in 1:length(e1))
    #  tmp[[i]] <- structure(intersect(e1[[i]],e2[[i]]),class="rowid")
    tmp <- lapply(1:length(e1),
                    function(N) structure(intersect(e1[[N]],e2[[N]]),
                                          class="rowid"))
    structure(tmp, class="rowid")
  } else {
    if(!is.null(getOption("use.bit")) && getOption("use.bit")) {
    rmap <- lmap <- bit(attr(e1,'length'))
    rmap[e1] <- TRUE
    lmap[e2] <- TRUE
    structure(as.which(rmap & lmap), class="rowid")
    } else {
    structure(intersect(e1,e2), class="rowid")
    }
  }
}
`|.rowid` <- function(e1,e2) {
  if(is.list(e1) || is.list(e2)) {
    if(!is.list(e1))
      e1 <- rep(list(e1), length(e2))
    if(!is.list(e2))
      e2 <- rep(list(e2), length(e1))
    tmp <- vector("list", length(e1))
    #for(i in 1:length(e1))
    #  tmp[[i]] <- structure(intersect(e1[[i]],e2[[i]]),class="rowid")
    tmp <- lapply(1:length(e1),
                    function(N) structure(unique(c(e1[[N]],e2[[N]])),
                                          class="rowid"))
    structure(tmp, class="rowid")
  } else {
    if(!is.null(getOption("use.bit")) && getOption("use.bit")) {
    rmap <- lmap <- bit(attr(e1,'length'))
    rmap[e1] <- TRUE
    lmap[e2] <- TRUE
    structure(as.which(rmap | lmap), class="rowid")
    } else {
    structure(unique(c(e1,e2)), class="rowid")
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


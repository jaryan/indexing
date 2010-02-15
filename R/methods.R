# methods to allow logical combining of searchIndex
# results

`|.rowid` <- function(e1,e2) {
  structure(unique(c(e1,e2)), class="rowid")
}

`&.rowid` <- function(e1,e2) {
  structure(intersect(e1,e2), class="rowid")
}

# method to allow R-style subsetting using
# unquoted boolean operations

`[.indexed_db` <- function(x, i, j, envir=.IndexEnv, ...) {
  i <- eval(match.call(`[.indexed_db`)$i, envir=x)
  if(!missing(j)) {
    # j should be the column list in .IndexEnv, $d the data
    #get(j, envir=envir)[["d"]][i]
    vars <- all.vars(match.call(`[.indexed_db`)$j)
    tmp.env <- new.env()
    for(v in 1:length(vars)) {
      assign(vars[v], get(vars[v], envir=envir)[['d']][i], tmp.env)
    }
    eval(match.call(`[.indexed_db`)$j, envir=tmp.env)
  }
  else i
}

`<.indexed` <- function(e1,e2) {
  if(inherits(e1, 'indexed'))
    searchIndex(deparse(substitute(e1)), e2, "<")
  else searchIndex(deparse(substitute(e2)), e1, ">")
}
`>.indexed` <- function(e1,e2) {
  if(inherits(e1, 'indexed'))
    searchIndex(deparse(substitute(e1)), e2, ">")
  else searchIndex(deparse(substitute(e2)), e1, "<")
}
`<=.indexed` <- function(e1,e2) {
  if(inherits(e1, 'indexed'))
    searchIndex(deparse(substitute(e1)), e2, "<=")
  else searchIndex(deparse(substitute(e2)), e1, ">=")
}
`>=.indexed` <- function(e1,e2) {
  if(inherits(e1, 'indexed'))
    searchIndex(deparse(substitute(e1)), e2, ">=")
  else searchIndex(deparse(substitute(e2)), e1, "<=")
}
`==.indexed` <- function(e1,e2) {
  e <- integer()
  if(inherits(e1, 'indexed')) {
    for(i in 1:length(e2)) {
      e <- c(e,searchIndex(deparse(substitute(e1)), e2[i], "="))
      e <- unique(e)
    }
  } else {
    for(i in 1:length(e2)) {
      e <- c(e,searchIndex(deparse(substitute(e1)), e2[i], "="))
      e <- unique(e)
    }
  }
  structure(e, class='rowid')
}

`!=.indexed` <- function(e1,e2) {
  e <- integer()
  if(inherits(e1, 'indexed')) {
    for(i in 1:length(e2)) {
      e <- c(e,searchIndex(deparse(substitute(e1)), e2[i], "!="))
      e <- unique(e)
    }
  } else {
    for(i in 1:length(e2)) {
      e <- c(e,searchIndex(deparse(substitute(e1)), e2[i], "!="))
      e <- unique(e)
    }
  }
  structure(e, class='rowid')
}

`%r%` <- function(e1, e2) {
  UseMethod("%r%") 
}
`%r%.indexed` <- function(e1, e2) {
  searchIndex(deparse(substitute(e1)), e2)
}


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

# method to allow R-style subsetting using
# unquoted boolean operations

# need to add a subset() or other method to create
# pre-compiled queries, to alleviate the very complicated
# scope/lookup that is taking place here.
#  i <- "strike == 15"
#  db[i] _should work_ (it doesn't now)
qsubset <- function(x, i, j, ...) {
  if(!inherits(x, "indexed_db"))
    stop("'qsubset' requires an indexed_db object")
  envir <- x
  i <- parse(text=i,srcfile=FALSE)
  i <- eval(i, envir=as.list(x,rev(sys.frames())), enclos=parent.frame())
  if(missing(j))
    return(structure(i, class="rowid"))
  if(!missing(j)) {
    j <- parse(text=j, srcfile=FALSE)
    vars <- all.vars(j)
    tmp.env <- new.env()
    for(v in 1:length(vars)) {
      VAR <- NULL
      if(exists(vars[v], envir=envir) && 
         inherits(get(vars[v],envir=envir),"indexed")) {
        VAR <- get(vars[v], envir=envir)[['d']][i]
      } else {
        # if not in 'database' look up frames until we find
        for(f in rev(sys.frames())) {
          if(exists(vars[v],envir=f)) {
            VAR <- get(vars[v], envir=f)
            break
          }
        }
        if(is.null(VAR))
          stop(paste("variable",vars[v],"not found")) 
      }
      assign(vars[v], 
             VAR,
             tmp.env)
             #get(vars[v],
             #    envir=envir)[['d']][i], tmp.env)
    }
    eval(j, envir=tmp.env)
  } else i
}

# could use something like this to run count automatically to get smallest subset
# from disk.  Expense of linear search on subset in-memory has some B/E point with
# AND/OR using interesect/c. bitmap indexing internal will better this though
# eval(eval(parse(text=paste("as.call(expression(`", FUN, "`,'",VAR,"',38,count=TRUE))",sep=""))))
# [1] 9988

subset.indexed_db <- `[.indexed_db` <- function(x, i, j, group, order, count=FALSE, ...) {
  if(!missing(group))
    return(match.call(`[.indexed_db`))
  mc_i <- match.call(`[.indexed_db`)$i
  envir <- x
  if(count) {
    # query optimizer; move large count results to smallest
    # table possible then rerun
    vars <- all.vars(mc_i)
    tmp.env <- new.env()
    for(v in 1:length(vars)) {
      assign(vars[v], 
             structure(get(vars[v], envir=envir)[['d']],count=TRUE),
             tmp.env)
    }
    return(eval(mc_i,
                envir=as.list(tmp.env,rev(sys.frames())),
                enclos=parent.frame()))
  }
  i <- eval(mc_i, envir=as.list(x,rev(sys.frames())), enclos=parent.frame())
  if(missing(j))
    return(structure(i, class="rowid"))
  if(!missing(j)) {
    mc_j <- match.call(`[.indexed_db`)$j
    if(is.character(mc_j)) 
      mc_j <- parse(text=mc_j)
    # j should be the column list in .IndexEnv, $d the data
    #get(j, envir=envir)[["d"]][i]
    vars <- all.vars(mc_j)
    tmp.env <- new.env()
#    if(is.bit(i))
#      i <- as.which(i)
    for(v in 1:length(vars)) {
      VAR <- NULL
      if(exists(vars[v], envir=envir) && 
         inherits(get(vars[v],envir=envir),"indexed")) {
        VAR <- get(vars[v], envir=envir)[['d']][i]
      } else {
        # if not in 'database' look up frames until we find
        for(f in rev(sys.frames())) {
          if(exists(vars[v],envir=f)) {
            VAR <- get(vars[v], envir=f)
            break
          }
        }
        if(is.null(VAR))
          stop(paste("variable",vars[v],"not found")) 
      }
      assign(vars[v], 
             VAR,
             tmp.env)
             #get(vars[v],
             #    envir=envir)[['d']][i], tmp.env)
    }
    eval(mc_j, envir=tmp.env)
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
    #bitmap <- bit(length(e1$d))
  } else {
    for(i in 1:length(e2)) {
      e <- c(e,searchIndex(deparse(substitute(e1)), e2[i], "="))
      e <- unique(e)
    }
    #bitmap <- bit(length(e1$d))
  }
  #bitmap[e] <- TRUE
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

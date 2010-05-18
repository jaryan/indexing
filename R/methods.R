# method to allow R-style subsetting using
# unquoted relational algebra

# could use something like this to run count automatically to get smallest subset
# from disk.  Expense of linear search on subset in-memory has some B/E point with
# AND/OR using interesect/c. bitmap indexing internal will better this though
# eval(eval(parse(text=paste("as.call(expression(`", FUN, "`,'",VAR,"',38,count=TRUE))",sep=""))))
# [1] 9988

orderBy <- function(x, order.by) {
  # internal use function to sort output columns
  # currently only supports ascending sort order
  order.by <- rev(order.by)
  decr <- grepl("-",order.by)
  order.by <- gsub("-","",order.by)
  if(NCOL(x) > 1) {
    temp.x <- x[order(x[[order.by[1]]],decreasing=decr[1]), ]
    if (length(order.by) > 1) {
        for (i in 2:length(order.by)) {
            temp.x <- temp.x[order(temp.x[[order.by[i]]],decreasing=decr[i]), ]
        }
    }
  } else {
    # univariate series
    temp.x <- x[order(x[[order.by[1]]],decreasing=decr[1])]
    if (length(order.by) > 1) {
        for (i in 2:length(order.by)) {
            temp.x <- temp.x[order(temp.x[[order.by[i]]],decreasing=decr[i])]
        }
  }
  }
  temp.x
}

# main subsetting function
subset.indexed_db <- 
   `[.indexed_db` <- 
function(x, i, j, group, order., limit, count=FALSE, ...) {
  if(!missing(group))
    return(match.call(`[.indexed_db`))
  mc_i <- match.call(`[.indexed_db`)$i

  # check for string query instead of call
  is_char <- try(is.character(mc_i_char <- eval(mc_i)),silent=TRUE)
  if(is.logical(is_char) && isTRUE(is_char) && !inherits(is_char,"try-error")) 
    mc_i <- parse(text=mc_i_char)

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
 
  # if db[condition] simply return rows, with print.rowid showing
  # the equivelent of count(*) in SQL

  if((length(i)==0 && nargs()==2) || nargs()==2)
    return(as.rowid(i))
    #return(structure(i, class="rowid"))

  if(TRUE) {
    if(!missing(j)) {
      mc_j <- match.call(`[.indexed_db`)$j
   
      # check if we need to parse a character version of 'j'
      is_char <- try(is.character(mc_j_char <- eval(mc_j)),silent=TRUE)
      if(is.logical(is_char) && isTRUE(is_char) && !inherits(is_char,"try-error")) 
        mc_j <- parse(text=mc_j_char)

    } else {
      # if db[condition,] return the matching rows as [.data.frame would
      mc_j <- parse(text=paste("data.frame(",
                               paste(ls(envir),collapse=","),
                               ")"),
                    srcfile=NULL) 
    }
    # j should be the column list in .IndexEnv, $d the data
    #get(j, envir=envir)[["d"]][i]
    vars <- all.vars(mc_j)
    tmp.env <- new.env()
#    if(is.bit(i))
#      i <- as.which(i)
    for(v in 1:length(vars)) {
      VAR <- NULL
      if(exists(vars[v], envir=envir, inherits=FALSE)) {
      #if(exists(vars[v], envir=envir) && 
      #   inherits(get(vars[v],envir=envir),"indexed")) {
        #VAR <- get(vars[v], envir=envir)[['d']][i]
        VAR <- get(vars[v], envir=envir)
        if(inherits(VAR,"indexed_list")) {
          # loop over list of indexed data to get subsets
          Var <- vector("list",length(unclass(VAR)))
          for(ii in 1:length(Var)) {
            Var[[ii]] <- VAR[[ii]]$d[as.rowid(i[[ii]])]
          }
          VAR <- unlist(Var)
        } else {
          VAR <- VAR[['d']][as.rowid(i)]
        }
      } else {
        # if not in 'database' look up frames until we find
        for(f in rev(sys.frames())) {
          if(exists(vars[v],envir=f)) {
            VAR <- get(vars[v], envir=f)[as.rowid(i)]
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
    res <- eval(mc_j, envir=tmp.env)
    if(!missing(order.))
      res <- orderBy(res,order.)

    if(!missing(limit)) {
      if(is.null(dim(res)))
        res <- res[1:limit]
      else res <- res[1:limit,]
    }
    res
  }
  else as.rowid(i)
}

`<.indexed` <- function(e1,e2) {
  if(inherits(e1, 'indexed'))
    search_index(deparse(substitute(e1)), e2, "<")
  else search_index(deparse(substitute(e2)), e1, ">")
}
`>.indexed` <- function(e1,e2) {
  if(inherits(e1, 'indexed'))
    search_index(deparse(substitute(e1)), e2, ">")
  else search_index(deparse(substitute(e2)), e1, "<")
}
`<=.indexed` <- function(e1,e2) {
  if(inherits(e1, 'indexed'))
    search_index(deparse(substitute(e1)), e2, "<=")
  else search_index(deparse(substitute(e2)), e1, ">=")
}
`>=.indexed` <- function(e1,e2) {
  if(inherits(e1, 'indexed'))
    search_index(deparse(substitute(e1)), e2, ">=")
  else search_index(deparse(substitute(e2)), e1, "<=")
}
`==.indexed` <- function(e1,e2) {
  e <- integer()
  if(inherits(e1, 'indexed')) {
    len <- length(e1)
    for(i in 1:length(e2)) {
      e <- c(e,search_index(deparse(substitute(e1)), e2[i], "=="))
    }
  } else {
    len <- length(e2)
    for(i in 1:length(e1)) {
      e <- c(e,search_index(deparse(substitute(e2)), e1[i], "=="))
    }
  }
  .Call("indexing_add_class", e, len, "rowid")
}

`!=.indexed` <- function(e1,e2) {
  e <- integer()
  if(inherits(e1, 'indexed')) {
    for(i in 1:length(e2)) {
      e <- c(e,search_index(deparse(substitute(e1)), e2[i], "!="))
      e <- unique(e)
    }
  } else {
    for(i in 1:length(e2)) {
      e <- c(e,search_index(deparse(substitute(e1)), e2[i], "!="))
      e <- unique(e)
    }
  }
  structure(e, class='rowid')
}

`%r%` <- function(e1, e2) {
  UseMethod("%r%") 
}

`%r%.indexed` <- function(e1, e2) {
  search_index(deparse(substitute(e1)), e2, type="%r%")
}

`%r%.numeric` <- function(e1,e2) {
  which(e1 > e2[1] & e1 < e2[2])
}

`%r%.mmap` <- function(e1,e2) {
  intersect(e1 > e2[1],e1 < e2[2])
}



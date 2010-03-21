searchIndex <-
function(column, x, type='=', SIZE=1e5, env=.IndexEnv, range=FALSE, count=FALSE) {
  if(missing(column))
    stop("column must be specified as character string")
  if(!is.character(column))
    column <- deparse(substitute(column))


  if(inherits(env,"indexed_db")) {
    .x <- env[[column]]
    if(inherits(.x,"indexed_list")) {
        rows <- lapply(.x,function(e) {
                          searchIndex(column=column,x=x,
                                      type=type,SIZE=SIZE,
                                      env=e,range=range,count=count)
                          }
                )
        # searchUnindex(column=column,x=x,type=type,env=.x)
        return(rows)
    }
  } else
  if(inherits(env,"indexed")) {
    .x <- env
  } else stop("'env' needs to be of class 'indexed_db' or 'indexed'")

  binsearch <- function (key, vec, start = TRUE) 
  {
    # code is originally from the xts package
    .Call("binsearch", as.double(key), vec, start, PKG="indexing")
  }

  if(length(x) == 2) { # range query
    if(missing(type))
      type <- c(">=","<=")
    if(length(type) != 2L) stop("length of 'type' and 'x' must match")
    i <- seq(searchIndex(column, x[1], type=type[1], SIZE, env, range=TRUE),
             searchIndex(column, x[2], type=type[2], SIZE, env, range=TRUE))
  } else { # non-range query
    if(!is.null(.x$l) && is.character(.x$l)) {
      x <- which(x==.x$l) 
    } 
    sparse_seq <- seq(1, length(.x$s), SIZE)
    if(sparse_seq[length(sparse_seq)] != length(.x$s))
      sparse_seq <- c(sparse_seq,length(.x$s))
  
    sparse_index <- .x$s[sparse_seq]
  
    ll <- binsearch(x, sparse_index,TRUE)
    if(ll != 1)
      ll <- ll-1L
    ul <- binsearch(x, sparse_index,FALSE)
    if(ul != length(sparse_index))
      ul <- ul+1L
  
    final_index <- seq(sparse_seq[ll],sparse_seq[ul])
    s_final_index <- .x$s[final_index]
  
    # avoid unnecessary binsearch for inequality
    if(type %in% c("=","==","!=","<",">="))
      x_start <- binsearch(x, s_final_index, TRUE)
  
    if(type %in% c("=","==","!=",">","<="))
      x_end   <- binsearch(x, s_final_index, FALSE)

    if(type %in% c("=", "==", "!=")) {
      i <- .Call("R_memcpy", final_index, x_start, x_end)
      if(type == "!=")
        i <- (1:length(.x$s))[-i]
    } else
    if(type == "<") {
      if(range)
        return(final_index[x_start]-1)
      i <- seq(1,final_index[x_start]-1)
    } else
    if(type == "<=") {
      if(range)
        return(final_index[x_end])
      i <- seq(1,final_index[x_end])
    } else
    if(type == ">") {
      if(range)
        return(final_index[x_end])
      i <- seq(final_index[x_end]+1, length(.x$s))
    } else
    if(type == ">=") {
      if(range)
        return(final_index[x_start])
      i <- seq(final_index[x_start], length(.x$s))
    } else {
      stop(paste('type:',sQuote(type),
           'not one of [==, !=, =, <, <=, >, >=]'))
    }
  }

  if(length(i)==2 && i[2] > length(.x$s))
    return(logical(0))
  if(length(i)==2 && i[2] < i[1])
    return(logical(0))

  if(count)
    return(length(i))
#  bitmap <- bit(length(.x$d))
#  bitmap[.x$o[i]] <- TRUE
  bitmap <- NULL
  structure(.x$o[i],bitmap=bitmap, len=length(.x$d), class="rowid")
}

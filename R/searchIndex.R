search_index <-
function(column, x, type='=', SIZE=1e5, env=.IndexEnv, 
         range=FALSE, count=FALSE, parallel=FALSE) {
  if(type=="=")
    type <- "=="

  if(!parallel)
    mclapply <- lapply

  if(missing(column))
    stop("column must be specified as character string")
  if(!is.character(column))
    column <- deparse(substitute(column))

  # search can occur on a indexed_db environment
  # or with an `indexed` element (list) in that environment
  if(inherits(env,"indexed_db")) {
    .x <- env[[column]]
    
    # element may be a list of horizontally partitioned
    # data per column.  If so, we iterate over the
    # elements as they are unique.  Return a list of rows
    # that match
    if(inherits(.x,"indexed_list")) {
      rows <- mclapply(1:length(unclass(.x)),function(e) {
                       search_index(column=column,x=x,
                                   type=type,SIZE=SIZE,
                                   env=structure(.x[[e]],
                                       class=class(.x[[e]])),
                                   range=range,count=count)
                       }
                )
        class(rows) <- "rowid"
        return(rows)
    }
  } else
  if(inherits(env,"indexed")) {
    .x <- env
  } else stop("'env' needs to be of class 'indexed_db' or 'indexed'")

  binsearch <- function (key, vec, start = TRUE) {
    .Call("binsearch", as.double(key), vec, start, PACKAGE="indexing")
  }

  if(is.null(.x$o)) {
### UNSORTED data 
  ###               linear scan             ###
    i <- eval(do.call(type,list(.x$d,x)))
  } else 
### SORTED data
  if(!is.null(.x$rle)) {
  ###               rle search              ###
    if(!is.null(.x$l) && 
       (is.character(.x$l) || ( is.mmap(.x$l) && 
       is.character(.x$l$storage.mode)))) {
      x <- x==.x$l
      if(length(x) > 0 && is.logical(x))
        x <- which(x) 
    } 
    #if(length(x) > 0) {
      #i <- do.call(seq, as.list(c(max(.x$rle[x-1],0)+1, .x$rle[x])))
      i <- do.call(seq, as.list(c(.x$rle[x-1]+1, .x$rle[x])))
    #} else i <- integer(0)
  } else {
  ###               bitmap search           ###
  ###               hash search           ###

  ###               binary search           ###
  if(length(x) == 2) {                  # range query
    if(missing(type) || type=="%r%")
      type <- c(">=","<=")
    if(length(type) != 2L) stop("length of 'type' and 'x' must match")
    i <- seq(search_index(column, x[1], type=type[1], SIZE, env, range=TRUE),
             search_index(column, x[2], type=type[2], SIZE, env, range=TRUE))
  } else {                              # non-range query
    if(!is.null(.x$l) && 
       (is.character(.x$l) || ( is.mmap(.x$l) && is.character(.x$l$storage.mode)))
      ) {
      x <- x==.x$l
      if(length(x) > 0 && is.logical(x))
        x <- which(x) 
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
      if(range)  # added max(na.omit to try and handle outside cases
        return(na.omit(final_index[x_start])-1)
      i <- seq(1,na.omit(final_index[x_start])-1)
      #  return(max(na.omit(final_index[x_start]),0)-1)
      #i <- seq(1,max(na.omit(final_index[x_start]),0)-1)
    } else
    if(type == "<=") {
      if(range)
        return(final_index[x_end])
      i <- seq(1,final_index[x_end])
    } else
    if(type == ">") {
      if(range)
        return(final_index[x_end]+1)
      i <- seq(final_index[x_end]+1, length(.x$s))
      #  return(max(final_index[x_end],0)+1)
      #i <- seq(max(final_index[x_end],0)+1, length(.x$s))
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
  }

  if(length(i)==2 && i[2] > length(.x))
    return(logical(0))
  if(length(i)==2 && i[2] < i[1])
    return(logical(0))

  if(count)
    return(length(i))
  if(!is.null(.x$o))
    i <- .x$o[i]
  i <- .Call("indexing_add_class", i, length(.x), "rowid")
  return(i)
  #structure(i,bitmap=bitmap, len=length(.x), class="rowid")
}

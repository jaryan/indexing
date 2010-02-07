searchIndex <-
function(column, x, SIZE=1e5) {
  # only applicable to equality currently
  if(missing(column))
    stop("column must be specified")
  .x <- get(".IndexEnv", .GlobalEnv)[[column]]
  binsearch <- xts:::binsearch
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
  #i <- seq(binsearch(x, s_final_index, FALSE), binsearch(x, s_final_index, TRUE))
  #i <- final_index[i]
  i <- .Call("R_memcpy", final_index, binsearch(x,s_final_index,TRUE), binsearch(x,s_final_index,FALSE))
  if(length(i)==2 && i[2] > length(.x$s))
    return(logical(0))
  .x$o[i]
}


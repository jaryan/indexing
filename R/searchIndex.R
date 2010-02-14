searchIndex <-
function(column, x, type='=', SIZE=1e5, env=.IndexEnv) {
  # only applicable to equality currently
  if(missing(column))
    stop("column must be specified")
  #.x <- get(".IndexEnv", .GlobalEnv)[[column]]
  .x <- env[[column]]
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
    i <- seq(1,final_index[x_start]-1)
  } else
  if(type == "<=") {
    i <- seq(1,final_index[x_end])
  } else
  if(type == ">") {
    i <- seq(final_index[x_end]+1, length(.x$s))
  } else
  if(type == ">=") {
    i <- seq(final_index[x_start], length(.x$s))
  }

  if(length(i)==2 && i[2] > length(.x$s))
    return(logical(0))
  .x$o[i]
}

loadIndex <- function(column, mode=int32(), omode=int32(), dir=NULL, envir=.IndexEnv, ...) {
  # ideally there is metadata in the database to remove the reliance
  # on 'mode'
  if(!is.list(mode))
    mode <- list(mode)
  mode <- rep(mode, length.out=length(column))
  if(!is.list(omode))
    omode <- list(omode)
  omode <- rep(omode, length.out=length(column))
  for(i in 1:length(column)) {
    sorted_path <- paste(column[i], "sorted.bin", sep="_")
    ordered_path <- paste(column[i], "ordered.bin", sep="_")
    stopifnot(file.exists(sorted_path) || file.exists(ordered_path))
    envir[[column[i]]] <- structure(list(), class="indexed")
    envir[[column[i]]]$s <- mmap(sorted_path, mode=mode[[i]])
    envir[[column[i]]]$o <- mmap(ordered_path, mode=omode[[i]])
  }
}

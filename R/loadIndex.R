loadIndex <- function(column,
                      mode=int32(),
                      omode=int32(),
                      dir=NULL,
                      envir=.IndexEnv, ...) {
  # ideally there is metadata in the database to remove the reliance
  # on 'mode'
  #   mode is on-disk data mode
  #   omode is ordered mode, typically int32
  if(!is.list(mode))
    mode <- list(mode)
  mode <- rep(mode, length.out=length(column))
  if(!is.list(omode))
    omode <- list(omode)
  omode <- rep(omode, length.out=length(column))
  for(i in 1:length(column)) {
    envir[[column[i]]] <- structure(list(), class="indexed")
    # each column in the envir contains a list of entries:
    #  d: original data mapping
    #  s: sorted data
    #  o: ordered data (location of sorted in data)
    #  
    #  optional:
    #    rle: run-length encoded vector
    #      l: levels for character/factor
    data_path <- paste(column[i], "data.bin", sep="_")
    if(file.exists(data_path))
      envir[[column[i]]]$d <- mmap(data_path, mode=mode[[i]])

    sorted_path <- paste(column[i], "sorted.bin", sep="_")
    ordered_path <- paste(column[i], "ordered.bin", sep="_")
    stopifnot(file.exists(sorted_path) || file.exists(ordered_path))
    # load sorted data
    envir[[column[i]]]$s <- mmap(sorted_path, mode=mode[[i]])
    # load order
    envir[[column[i]]]$o <- mmap(ordered_path, mode=omode[[i]])
  
    # rle is an experimental/alternate index search scheme
    rle_path <- paste(column[i], "rle.bin", sep="_")
    if(file.exists(rle_path))
      envir[[column[i]]]$rle <- mmap(rle_path, mode=omode[[i]])

    # levels exist for non-numeric data (char -> factor)
    levels_path <- paste(column[i], "levels.bin", sep="_")
    if(file.exists(levels_path))
      envir[[column[i]]]$l <- mmap(levels_path, mode=omode[[i]])
  }
  invisible(envir)
}

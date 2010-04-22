loadData  <- function(column,
                      mode=int32(),
                      subclass=NULL,
                      omode=int32(),
                      dir=NULL,
                      verbose=0,
                      envir=.IndexEnv, ...) {
  if(!is.list(subclass))
    subclass <- list(subclass)
  subclass <- rep(subclass, length.out=length(column))
  if(!is.list(mode))
    mode <- list(mode)
  mode <- rep(mode, length.out=length(column))
  if(!is.list(omode))
    omode <- list(omode)
  omode <- rep(omode, length.out=length(column))
  for(i in 1:length(column)) {
    if(!is.null(subclass[[i]]))
      subclass[[i]] <- paste("indexed",subclass[[i]],sep="_") 
    envir[[column[i]]] <- structure(list(), 
                                    class=c(subclass[[i]],"indexed"))
    # each column in the envir contains a list of entries:
    #  d: original data mapping
    #  s: sorted data
    #  o: ordered data (location of sorted in data)
    #  
    #  optional:
    #    rle: run-length encoded vector
    #      l: levels for character/factor
    data_path <- paste(column[i], "data.bin", sep="_")
    if(file.exists(data_path)) {
      if(verbose)
        message(paste("loading data",sQuote(column[i])))
      envir[[column[i]]]$d <- mmap(data_path, mode=mode[[i]])
    }
  }
  invisible(envir)
}

loadIndex <- function(column,
                      mode=int32(),
                      subclass=NULL,
                      omode=int32(),
                      dir=NULL,
                      verbose=0,
                      envir=.IndexEnv, ...) {
  # ideally there is metadata in the database to remove the reliance
  # on 'mode' and 'subclass'
  #   mode is on-disk data mode
  #   omode is ordered mode, typically int32
  if(!is.list(subclass))
    subclass <- list(subclass)
  subclass <- rep(subclass, length.out=length(column))
  if(!is.list(mode))
    mode <- list(mode)
  mode <- rep(mode, length.out=length(column))
  if(!is.list(omode))
    omode <- list(omode)
  omode <- rep(omode, length.out=length(column))
  for(i in 1:length(column)) {
    if(!is.null(subclass[[i]]))
      subclass[[i]] <- paste("indexed",subclass[[i]],sep="_") 
    envir[[column[i]]] <- structure(list(), 
                                    class=c(subclass[[i]],"indexed"))
    # each column in the envir contains a list of entries:
    #  d: original data mapping
    #  s: sorted data
    #  o: ordered data (location of sorted in data)
    #  
    #  optional:
    #    rle: run-length encoded vector
    #      l: levels for character/factor
    data_path <- paste(column[i], "data.bin", sep="_")
    if(file.exists(data_path)) {
      if(verbose)
        message(paste("loading data",sQuote(column[i])))
      envir[[column[i]]]$d <- mmap(data_path, mode=mode[[i]])
    }

    sorted_path <- paste(column[i], "sorted.bin", sep="_")
    ordered_path <- paste(column[i], "ordered.bin", sep="_")
    stopifnot(file.exists(sorted_path) || file.exists(ordered_path))
    # load sorted data
    if(verbose > 1)
      message(paste("loading sorted",sQuote(column[i])))
    envir[[column[i]]]$s <- mmap(sorted_path, 
                                 mode=mode[[i]],
                                 prot=mmapFlags("PROT_READ"))
    # load order
    if(verbose > 1)
      message(paste("loading index",sQuote(column[i])))
    envir[[column[i]]]$o <- mmap(ordered_path, 
                                 mode=omode[[i]],
                                 prot=mmapFlags("PROT_READ"))
  
    # rle is an experimental/alternate index search scheme
    rle_path <- paste(column[i], "rle.bin", sep="_")
    if(file.exists(rle_path))
      envir[[column[i]]]$rle <- mmap(rle_path, mode=omode[[i]])

    # levels exist for non-numeric data (char -> factor)
    levels_path <- paste(column[i], "levels.bin", sep="_")
    if(file.exists(levels_path)) {
      # currenly hard coded to 1mm levels, need to fix
      if(verbose > 1)
        message(paste("loading levels",sQuote(column[i])))
      l <- readBin(levels_path, character(), 1e6)
      envir[[column[i]]]$l <- l
      extractFUN(envir[[column[i]]]$d) <- function(x) {
        as.character(structure(x, levels=l, class='factor'))
      }
    }
  }
  invisible(envir)
}

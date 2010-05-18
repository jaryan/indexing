load_data <- 
loadData  <- function(column,
                      mode=int32(),
                      subclass=NULL,
                      omode=int32(),
                      lmode=NULL,
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

load_index <- loadIndex <- 
             function(column,
                      mode=int32(),
                      subclass=NULL,
                      omode=int32(),
                      lmode=NULL,
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
    # each column in the envir contains a list of a list of entries:
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
    envir[[column[i]]]$o <- seqfile(ordered_path, omode[[i]])

    # rle is an experimental/alternate index search scheme
    rle_path <- paste(column[i], "rle.bin", sep="_")
    if(file.exists(rle_path))
      envir[[column[i]]]$rle <- mmap(rle_path, mode=omode[[i]])

    # levels exist for non-numeric data (char -> factor)
    levels_path <- paste(column[i], "levels.bin", sep="_")
    if(file.exists(levels_path)) {
      # currenly hard coded to 1mm levels, need to fix
        col <- envir[[column[i]]]
        if(verbose > 1)
          message(paste("loading levels",sQuote(column[i])))
        if(is.null(lmode)) { # in-memory factor
          l <- readBin(levels_path, character(), 1e6)
          col$l <- l
          e <- new.env()
          e$l <- l
          extractFUN(col$d) <- function(x) {
            as.character(structure(x, levels=l, class='factor'))
          }
          environment(col$d$extractFUN) <- e
        } else { # disk-based factor (fixed width)
          col$l <- mmap(levels_path, lmode)  # lmode=char(10) 
          col$d <- structure(col$d, levels=col$l, class='dfactor')
        }
        envir[[column[i]]] <- col
        
    }
    envir[[column[i]]]$env <- envir
  }
  invisible(envir)
}

# new loadIndex to accommodate horizontally partitioned
# data structure on disk
#
#
#               
# dbname/       
#        column/.indexing
#        column/1/data.bin
#                /sorted.bin
#                /ordered.bin
#        column/2/data.bin
#                /sorted.bin
#                /ordered.bin

loadHIndex <- function(column,
                      mode=int32(),
                      subclass=NULL,
                      omode=int32(),
                      lmode=NULL,
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

    # col is the indexed object, ilist is the list of the indexed objs
    ilist <- list()

    # each column in the envir contains a list of a list of entries:
    #  d: original data mapping
    #  s: sorted data
    #  o: ordered data (location of sorted in data)
    #  
    #  optional:
    #    rle: run-length encoded vector
    #      l: levels for character/factor
    if(!is.null(dir))
      column_path <- dir(file.path(dir,column[i]))
    else column_path <- "."

    for( part in 1:length(column_path) ) {
      # each h-partitioned column is a list of col
      col <- structure(list(), class=c(subclass[[i]],"indexed"))

      data_path <- file.path(dir,column[i],column_path[part],
                            paste(column[i], "data.bin", sep="_"))
      if(file.exists(data_path)) {
        if(verbose)
          message(paste("loading data",sQuote(column[i])))
        col$d <- mmap(data_path, mode=mode[[i]])
      }
  
      sorted_path <- file.path(dir,column[i],column_path[part],
                              paste(column[i], "sorted.bin", sep="_"))
      ordered_path <- file.path(dir,column[i],column_path[part],
                               paste(column[i], "ordered.bin", sep="_"))
      stopifnot(file.exists(sorted_path) || file.exists(ordered_path))
      # load sorted data
      if(verbose > 1)
        message(paste("loading sorted",sQuote(column[i])))
      col$s <- mmap(sorted_path, 
                                   mode=mode[[i]],
                                   prot=mmapFlags("PROT_READ"))
      # load order
      if(verbose > 1)
        message(paste("loading index",sQuote(column[i])))
#      col$o <- mmap(ordered_path, 
#                                   mode=omode[[i]],
#                                   prot=mmapFlags("PROT_READ"))
    
      col$o <- seqfile(ordered_path, omode[[i]])
      # rle is an experimental/alternate index search scheme
      rle_path <- file.path(dir,column[i],column_path[part],
                            paste(column[i], "rle.bin", sep="_"))
      if(file.exists(rle_path))
        col$rle <- mmap(rle_path, mode=omode[[i]])
  
      # levels exist for non-numeric data (char -> factor)
      levels_path <- file.path(dir,column[i],column_path[part],
                              paste(column[i], "levels.bin", sep="_"))
      if(file.exists(levels_path)) {
        # currenly hard coded to 1mm levels, need to fix
        if(verbose > 1)
          message(paste("loading levels",sQuote(column[i])))
        if(is.null(lmode)) { # in-memory factor
          l <- readBin(levels_path, character(), 1e6)
          col$l <- l
          e <- new.env()
          e$l <- l
          extractFUN(col$d) <- function(x) {
            as.character(structure(x, levels=l, class='factor'))
          }
          environment(col$d$extractFUN) <- e
        } else { # disk-based factor (fixed width)
          col$l <- mmap(levels_path, lmode)  # lmode=char(10) 
          col$d <- structure(col$d, levels=col$l, class='dfactor')
        }
      }
      ilist <- append(ilist, list(col))
    } # end loop over h-partitioned columns
    class(ilist) <- c("indexed_list",subclass[[1]],"indexed")
    envir[[column[i]]] <- ilist
  }
  envir$.Partitions <- sapply(ilist, length)
  envir$.Paths <- column_path
  invisible(envir)
}

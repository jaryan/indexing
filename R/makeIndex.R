makeIndex <-
function(x, column=NULL, envir=.IndexEnv, ...) {
  UseMethod("makeIndex")
}

makeIndex.integer <-
function(x, column=NULL, envir=.IndexEnv, ...) {
  if(missing(column))
    column <- deparse(substitute(x))
  if(!is.numeric(x))
    x <- as.factor(x)
  envir[[column]]$o <- order(x)
  tmp.s <- as.integer(x[envir[[column]]$o])
   writeBin(tmp.s, paste(column,"_sorted.bin",sep=""))
  envir[[column]]$s <- mmap(file=paste(column,"_sorted.bin",sep=""))
   writeBin(envir[[column]]$o, paste(column,"_ordered.bin",sep=""))
  envir[[column]]$o <- mmap(file=paste(column,"_ordered.bin",sep=""))
  if(is.factor(x)) {
     writeBin(as.integer(rle(tmp.s)$length),paste(column,"_rle.bin",sep=""))
     envir[[column]]$rle <- readBin(paste(column,"_rle.bin",sep=""),integer(),10000)
  } else envir[[column]]$rle <- NULL
  if(is.factor(x)) {
    x <- levels(x)
  #} else x <- unique(x) # this should be NULL
  } else x <- NULL
  writeBin(x, paste(column,"_levels.bin",sep=""))
  envir[[column]]$l <- x
}

makeIndex.double <-
function(x, column=NULL, envir=.IndexEnv, ...) {
  if(missing(column))
    column <- deparse(substitute(x))
  envir[[column]]$o <- order(x)
  tmp.s <- as.double(x[envir[[column]]$o])
   writeBin(tmp.s, paste(column,"_sorted.bin",sep=""))
  envir[[column]]$s <- mmap(file=paste(column,"_sorted.bin",sep=""),mode=real64())
   writeBin(envir[[column]]$o, paste(column,"_ordered.bin",sep=""))
  envir[[column]]$o <- mmap(file=paste(column,"_ordered.bin",sep=""),mode=int32())
}

makeIndex.character <-
function(x, column=NULL, envir=.IndexEnv, ...) {
  if(missing(column))
    column <- deparse(substitute(x))
  if(!is.numeric(x))
    x <- as.factor(x)
  envir[[column]]$o <- order(x)
  tmp.s <- as.integer(x[envir[[column]]$o])
   writeBin(tmp.s, paste(column,"_sorted.bin",sep=""))
  envir[[column]]$s <- mmap(file=paste(column,"_sorted.bin",sep=""))
   writeBin(envir[[column]]$o, paste(column,"_ordered.bin",sep=""))
  envir[[column]]$o <- mmap(file=paste(column,"_ordered.bin",sep=""))
  if(is.factor(x)) {
     writeBin(as.integer(rle(tmp.s)$length),paste(column,"_rle.bin",sep=""))
     envir[[column]]$rle <- readBin(paste(column,"_rle.bin",sep=""),integer(),10000)
  } else envir[[column]]$rle <- NULL
  if(is.factor(x)) {
    x <- levels(x)
  #} else x <- unique(x) # this should be NULL
  } else x <- NULL
  writeBin(x, paste(column,"_levels.bin",sep=""))
  envir[[column]]$l <- x
}


createIndex <-
function(x, column=NULL, envir=.IndexEnv, ...) {
  if(missing(column)) 
    column <- deparse(substitute(x))
  if(is.character(x) && length(x) == 1)
    stop("createIndex requires 'x' to be a data object")
  if(file.exists(paste(column,"sorted.bin",sep="_")))
    stop("data already exists. Remove disk structure or use 'loadIndex'")
  UseMethod("createIndex")
}

createIndex.integer <-
function(x, column=NULL, mode=integer(), envir=.IndexEnv, ...) {
  if(missing(column))
    column <- deparse(substitute(x))
  envir[[column]] <- structure(list(),class="indexed")
  writeBin(x, paste(column,"data.bin",sep="_"))
  envir[[column]]$d <- mmap(file=paste(column,"data.bin",sep="_"))
  envir[[column]]$o <- order(x)
  tmp.s <- as.integer(x[envir[[column]]$o])
   writeBin(tmp.s, paste(column,"_sorted.bin",sep=""))
  envir[[column]]$s <- mmap(file=paste(column,"_sorted.bin",sep=""))
   writeBin(envir[[column]]$o, paste(column,"_ordered.bin",sep=""))
  envir[[column]]$o <- mmap(file=paste(column,"_ordered.bin",sep=""))
  envir
}

createIndex.double <-
function(x, column=NULL, mode=double(), envir=.IndexEnv, ...) {
  if(missing(column))
    column <- deparse(substitute(x))
  envir[[column]] <- structure(list(),class="indexed")
  writeBin(x, paste(column,"data.bin",sep="_"))
  envir[[column]]$d <- mmap(file=paste(column,"data.bin",sep="_"),mode=mode)
  envir[[column]]$o <- order(x)
  tmp.s <- as.double(x[envir[[column]]$o])
   writeBin(tmp.s, paste(column,"_sorted.bin",sep=""))
  envir[[column]]$s <- mmap(file=paste(column,"_sorted.bin",sep=""),mode=mode)
   writeBin(envir[[column]]$o, paste(column,"_ordered.bin",sep=""))
  envir[[column]]$o <- mmap(file=paste(column,"_ordered.bin",sep=""),mode=integer())
  envir
}

createIndex.character <-
function(x, column=NULL, envir=.IndexEnv, ...) {
  if(missing(column))
    column <- deparse(substitute(x))
  if(!is.numeric(x))
    x <- as.factor(x)
  envir[[column]] <- structure(list(),class="indexed")
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
    writeBin(x, paste(column,"_levels.bin",sep=""))
    envir[[column]]$l <- x
  } else x <- NULL
  envir
}

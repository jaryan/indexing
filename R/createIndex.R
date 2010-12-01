create_index <- createIndex <-
function(x, column=NULL, type="mmap", force=FALSE, envir=.IndexEnv, ...) {
  if(missing(column)) 
    column <- deparse(substitute(x))
  if(is.character(x) && length(x) == 1)
    stop("createIndex requires 'x' to be a data object")
  if(file.exists(paste(column,"sorted.bin",sep="_")))
    if(!force)
      stop("data already exists. Remove disk structure or use 'load_index'")
  get(paste("create_index",type,sep="."))(x=x,column=column,envir=envir,force=force,...)
}

create_index.mmap <- function(x, column, force, envir, ...) {
  UseMethod("create_index.mmap")
}
create_index.mem <- function(x, column, force, envir, ...) {
  stop("no memory indexing implemented yet: use type='mmap'")
}

create_index.mmap.integer <-
function(x, column, mode=integer(), force=FALSE, envir=.IndexEnv, ...) {
  envir[[column]] <- structure(list(),class="indexed")
  writeBin(x, paste(column,"data.bin",sep="_"))
  envir[[column]]$d <- mmap(file=paste(column,"data.bin",sep="_"))
  envir[[column]]$o <- order(x)
  tmp.s <- as.integer(x[envir[[column]]$o])
   writeBin(tmp.s, paste(column,"_sorted.bin",sep=""))
  envir[[column]]$s <- mmap(file=paste(column,"_sorted.bin",sep=""))
   writeBin(envir[[column]]$o, paste(column,"_ordered.bin",sep=""))
  #envir[[column]]$o <- mmap(file=paste(column,"_ordered.bin",sep=""))
  envir[[column]]$o <- seqfile(file=paste(column,"_ordered.bin",sep=""),int32())
  envir
}

create_index.mem.integer <-
function(x, column=NULL, mode=integer(), force=FALSE, envir=.IndexEnv, ...) {
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

create_index.mmap.double <-
function(x, column=NULL, mode=double(), force=FALSE, envir=.IndexEnv, ...) {
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
  #envir[[column]]$o <- mmap(file=paste(column,"_ordered.bin",sep=""),mode=integer())
  envir[[column]]$o <- seqfile(file=paste(column,"_ordered.bin",sep=""),mode=integer())
  envir
}

create_index.mmap.character <-
function(x, column=NULL, envir=.IndexEnv, ...) {
  if(missing(column))
    column <- deparse(substitute(x))
  if(!is.numeric(x))
    x <- as.factor(x)
  envir[[column]] <- structure(list(),class="indexed")
  writeBin(as.integer(x), paste(column,"data.bin",sep="_"))
  envir[[column]]$d <- mmap(file=paste(column,"data.bin",sep="_"),mode=integer())
  envir[[column]]$o <- order(x)
  tmp.s <- as.integer(x[envir[[column]]$o])
   writeBin(tmp.s, paste(column,"_sorted.bin",sep=""))
  envir[[column]]$s <- mmap(file=paste(column,"_sorted.bin",sep=""))
   writeBin(envir[[column]]$o, paste(column,"_ordered.bin",sep=""))
  #envir[[column]]$o <- mmap(file=paste(column,"_ordered.bin",sep=""))
  envir[[column]]$o <- seqfile(file=paste(column,"_ordered.bin",sep=""), integer())
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

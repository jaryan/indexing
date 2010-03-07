createData <-
function(x, column=NULL, envir=.IndexEnv, ...) {
  if(file.exists(paste(column,"data.bin",sep="_")))
    stop("data already exists. Remove disk structure or use 'loadIndex'")
  UseMethod("createData")
}

createData.integer <-
function(x, column=NULL, mode=integer(), envir=.IndexEnv, ...) {
  if(missing(column))
    column <- deparse(substitute(x))
  # should this be classed as 'data' or something different - probably
  envir[[column]] <- structure(list(),class="indexed")
  writeBin(x, paste(column,"data.bin",sep="_"))
  envir[[column]]$d <- mmap(file=paste(column,"data.bin",sep="_"))
}

createData.double <-
function(x, column=NULL, mode=double(), envir=.IndexEnv, ...) {
  if(missing(column))
    column <- deparse(substitute(x))
  envir[[column]] <- structure(list(),class="indexed")
  writeBin(x, paste(column,"data.bin",sep="_"))
  envir[[column]]$d <- mmap(file=paste(column,"data.bin",sep="_"),mode=mode)
}

createData.character <-
function(x, column=NULL, envir=.IndexEnv, ...) {
  if(missing(column))
    column <- deparse(substitute(x))
  if(!is.numeric(x))
    x <- as.factor(x)
  envir[[column]] <- structure(list(),class="indexed")
  int_x <- as.integer(x)
  writeBin(x_int, paste(column,"data.bin",sep="_"))
  envir[[column]]$d <- x_int
  rm(x_int)  # just in case we need to gc()
  if(is.factor(x)) {
    x <- levels(x)
    writeBin(x, paste(column,"_levels.bin",sep=""))
    envir[[column]]$l <- x
  }
}


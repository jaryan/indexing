# seqfile is a class to allow [ subsetting for
# fast sequential disk reads.  This is applicable
# for $o[] extraction from disk in searchIndex

seqfile <- function(file, mode) {
  f <- base::file(file, open="rb")
  structure(list(data=f, 
                 bytes=as.integer(file.info(file)$size),
                 filedesc=structure(as.integer(f), .Names=file),
                 storage.mode=as.Ctype(mode)
                ),
            class="seqfile")
}

`[.seqfile` <- function(x, i, ...) {
  bytes <- nbytes(x$storage.mode)
  seek(x$data, i[1] * bytes - bytes)
  ret <- readBin(x$data, x$storage.mode, i[length(i)],
                 size=bytes, signed=attr(x$storage.mode,"signed"))
  seek(x$data, 0)
  ret 
}

length.seqfile <- function(x) {
  size_in_bytes <- x$bytes
  size <- nbytes(x$storage.mode)
  as.integer(size_in_bytes/size)
}

print.seqfile <- function (x, ...) {
    stopifnot(inherits(x, 'seqfile'))
    file_name <- names(x$filedesc)
    if (nchar(file_name) > 10) 
        file_name <- paste(substring(file_name, 0, 10), "...", 
            sep = "")
    type_name <- switch(typeof(x$storage.mode), integer = "int", 
        double = "num", complex = "cplx", character = "chr", 
        raw = "raw")
    firstN <- x[1:min(6, length(x))]
    firstN <- if (cumsum(nchar(firstN))[length(firstN)] > 20) {
        firstN[1:min(3, length(x))]
    }
    else {
        firstN
    }
    cat(paste("<seqfile:", file_name, ">  (", class(x$storage.mode)[2], 
        ") ", type_name, " [1:", length(x), "]", sep = ""), firstN, 
        "...\n")
}


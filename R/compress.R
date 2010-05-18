compress_bitmap <- function(bitmap, type="LZO") {
  if(!is.bitmap(bitmap) || type != "LZO")
    stop("only LZO compression of bitmaps is implemented")
  cb <- .Call("indexing_compress_LZO", bitmap, PACKAGE="indexing")
  class(cb) <- "bitmap_lzo"
  cb
}

decompress_bitmap_lzo <- function(bitmap_lzo) {
  if(!is.bitmap_lzo(bitmap_lzo))
    stop("decompress requires an object created with compress_bitmap")
  .Call("indexing_decompress_LZO", bitmap_lzo)
  # need to have this returned as a c(bitmap, rowid) object
}

is.bitmap_lzo <- function(x) {
  inherits(x, "bitmap_lzo")
}

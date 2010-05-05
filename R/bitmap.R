
is.bitmap <- function(x) {
  inherits(x, "bitmap")
}

bitmap_template <- function(n) {
  .Call("make_bitmap", as.integer(n))
}

new_bitmap <- function(template) {
  # use memcpy to make each new e1/e2 as
  # fast as possible
  .Call("new_bitmap", template)
}

`[<-.bitmap` <- function(x, i, value,...) {
  if(value) {
    .Call("bitmap_true", i, x)
  } else {
    .Call("bitmap_false", i, x)
  }
}

`&.bitmap` <- function(e1, e2) {
  .Call("bitmap_dlogical", e1, e2, 1L)
}
`|.bitmap` <- function(e1, e2) {
  .Call("bitmap_dlogical", e1, e2, 2L)
}

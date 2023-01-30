.onLoad <- function(lib, pkg) {
  class(.IndexEnv) <- c("indexed_db", "environment")
  invisible(.Call("make_bitmask", PACKAGE="indexing"))
}

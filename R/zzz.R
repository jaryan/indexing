.onLoad <- function(lib, loc) {
  class(.IndexEnv) <- c("indexed_db", "environment")
}

# custom subclass methods for common types
# to be used for general R functionality as
# well as to illustrate simple extensibility

# indexed_posixct:  
#           An indexed subclass that allows
#           for special handling of `==`,
#           `>=`, `<=`, `!=`, `>`, and `<`methods

`==.indexed_posixct` <- function(e1,e2) {
  e <- integer()
  if (inherits(e1, "indexed")) {
    for (i in 1:length(e2)) {
      e <- c(e, searchIndex(deparse(substitute(e1)), 
          as.numeric(as.POSIXct(e2[i])), 
        "="))
      e <- unique(e)
    }   
  }   
  else {
    # LHS is 'POSIXct' formatted.  Could have assymmetric
    # behavior if desire(able). This is set up to fail now ;-)
    # read somewhere the best programmers are lazy programmers ...
    for (i in 1:length(e2)) {
      e <- c(e, searchIndex(deparse(substitute(e1)), e2[i], 
        "="))
      e <- unique(e)
    }   
  }   
  structure(e, class = "rowid")
}

# indexed_date 
`==.indexed_date` <- function(e1,e2) {
  e <- integer()
  if (inherits(e1, "indexed")) {
    for (i in 1:length(e2)) {
      e <- c(e, searchIndex(deparse(substitute(e1)), 
          as.integer(as.Date(e2[i])), 
        "="))
      e <- unique(e)
    }   
  }   
  else {
    # LHS is 'date' formatted.  Could have assymmetric
    # behavior if desire(able). This is set up to fail now ;-)
    # read somewhere the best programmers are lazy programmers ...
    for (i in 1:length(e2)) {
      e <- c(e, searchIndex(deparse(substitute(e1)), e2[i], 
        "="))
      e <- unique(e)
    }   
  }   
  structure(e, class = "rowid")
}

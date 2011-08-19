# disk-based factors using mmap for levels

`[.dfactor` <- function(x, i, ...) {
  class(x) <- "mmap"
  #structure(x[i], levels=attr(x, "levels"), class=c("dfactor","mmap"))
  xx <- structure(x[i],
        levels=attr(x, "levels"), class=c("dfactor","mmap"))
  class(x) <- c("dfactor","mmap")
  as.character(attr(xx, "levels")[xx])
}

print.dfactor <- function(x, ...) {
  if(!is.numeric(x)) {
    class(x) <- "mmap"
    print(x) 
  } else
  print(attr(x, "levels")[x])
}

#include <R.h>
#include <Rinternals.h>

SEXP R_memcpy (SEXP x, SEXP _from, SEXP _to) {
  SEXP result;
  int *from, *to;
  from = INTEGER(_from);
  to = INTEGER(_to);

  PROTECT(result = allocVector(TYPEOF(x),(to[0]-from[0]+1)));
  switch(TYPEOF(x)) {
    case INTSXP:
      memcpy(INTEGER(result),
             &(INTEGER(x)[(from[0]-1)]),
             length(result) * sizeof(int));
      break;
  }
  UNPROTECT(1);
  return(result);
}

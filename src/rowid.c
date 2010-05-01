#include <R.h>
#include <Rinternals.h>

SEXP indexing_add_class (SEXP _index, SEXP _len, SEXP _class) {
  setAttrib(_index, install("class"), _class);
  setAttrib(_index, install("length"), _len);
  return _index;
}

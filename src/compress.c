#include <R.h>
#include <Rinternals.h>
#include "lzo/minilzo.h"
#include "lzo/minilzo.c"
//#include "lzoconf.h"

#define indexing_BITMAPLZO    install("indexing_BITMAPLZO")

static void indexing_compress_LZO_finalizer (SEXP ptr) {
  if(!R_ExternalPtrAddr(ptr)) return;
  int *p;
  p = R_ExternalPtrAddr(ptr);
  Free(p);
  R_ClearExternalPtr(ptr);
}

SEXP indexing_compress_LZO (SEXP _bitmap) {
  /* use this to compress the bitmap itself */
  int *bitmap = INTEGER(_bitmap);

  lzo_bytep in;
  lzo_bytep out;
  lzo_bytep wrkmem;
  lzo_uint in_len;
  lzo_uint out_len;

  in_len = (lzo_uint)length(_bitmap)*sizeof(int);
  out_len = (in_len + in_len / 16 + 64 + 3); /* add padding per LZO examples */

  in = (lzo_bytep) Calloc(in_len, lzo_bytep);
  out = (lzo_bytep) Calloc(out_len, lzo_bytep);
  wrkmem = (lzo_bytep) Calloc(LZO1X_1_MEM_COMPRESS, lzo_bytep);
  memcpy(in, bitmap, in_len);  /* possible that we can skip this?? */

  if(lzo_init() != LZO_E_OK) {
    warning("LZO initialization failed. Please report to indexing maintainer");
    return(R_NilValue);
  }

  //int r = lzo1x_1_compress(in, in_len, out, &out_len, wrkmem);
  int r = lzo1x_1_compress((lzo_bytep)bitmap, in_len, out, &out_len, wrkmem);
  if(r != LZO_E_OK) {
    warning("LZO compression failed. Please report to indexing maintainer");
    return(R_NilValue);
  }
  Free(in);
  Free(wrkmem);
  
  SEXP bitmap_lzo = allocVector(VECSXP, 3);
  PROTECT(bitmap_lzo);

  SEXP ptr = R_MakeExternalPtr(out, indexing_BITMAPLZO, ScalarLogical(TRUE));
  PROTECT(ptr);
  R_RegisterCFinalizerEx(ptr, indexing_compress_LZO_finalizer, TRUE);

  SET_VECTOR_ELT(bitmap_lzo,0,ptr);
  SET_VECTOR_ELT(bitmap_lzo,1,ScalarInteger(out_len));
  SET_VECTOR_ELT(bitmap_lzo,2,ScalarInteger(in_len));
  
  UNPROTECT(2);
  return bitmap_lzo;
}

SEXP indexing_decompress_LZO (SEXP _bitmap_lzo) {
  lzo_bytep out;
  lzo_bytep wrkmem;
  lzo_uint in_len;
  lzo_uint out_len;
  lzo_uint new_len;

  out_len  = (lzo_uint)INTEGER(VECTOR_ELT(_bitmap_lzo, 1))[0];
  in_len = (lzo_uint)INTEGER(VECTOR_ELT(_bitmap_lzo, 2))[0];

  out = (lzo_bytep) R_ExternalPtrAddr(VECTOR_ELT(_bitmap_lzo, 0));
  wrkmem = (lzo_bytep) Calloc(LZO1X_1_MEM_COMPRESS, lzo_bytep);

  if(lzo_init() != LZO_E_OK) {
    warning("LZO initialization failed. Please report to indexing maintainer");
    return(R_NilValue);
  }
  /* now decompress */
  SEXP _bitmap;
  PROTECT(_bitmap = allocVector(INTSXP, in_len / sizeof(int)));
  int r = lzo1x_decompress(out, out_len, (lzo_bytep)INTEGER(_bitmap), &new_len, NULL);
  if(r != LZO_E_OK || new_len != in_len) {
    warning("LZO decompression failed");
    return R_NilValue;
  }
  Free(wrkmem);

  UNPROTECT(1);
  return _bitmap;
}

SEXP indexing_bitmap_lzo_as_raw (SEXP _bitmap_lzo) {
  /* extract the compressed bitmap from the pointer */
  unsigned char *cbits = (unsigned char *) R_ExternalPtrAddr(VECTOR_ELT(_bitmap_lzo, 0));
  SEXP _cbits = allocVector(RAWSXP, INTEGER(VECTOR_ELT(_bitmap_lzo,1))[0]);
  PROTECT(_cbits);
  memcpy(RAW(_cbits), cbits, length(_cbits));
  UNPROTECT(1);
  return _cbits;
}

SEXP indexing_write_bitmap_lzo (SEXP _bitmap_lzo, SEXP _filename) {
  FILE *fh;

  int len = INTEGER(VECTOR_ELT(_bitmap_lzo,1))[0];
  fh = fopen(CHAR(STRING_ELT(_filename,0)), "w");
  int r;
  r = fwrite(&len, sizeof(int), 1, fh);
  r = fwrite(R_ExternalPtrAddr(VECTOR_ELT(_bitmap_lzo,0)), sizeof(unsigned char), len, fh);
  fclose(fh);

  if(r != len) {
    error("failed to write compressed bitmap to disk");
  }
  return ScalarInteger(r);
}

/*
     add a compressed bitmap (RAW in R) 
     Not too sure how this would be used, as
     reading from disk directly in blocks
     would likely be much better.  Possibly
     could make use of something where instead of
     a pointer we can just decompress a RAW object,
     though we would need to know the original
     length I think??
SEXP indexing_raw_as_bitmap_lzo (SEXP _cbits) {
}
*/
/* TODO add ability to read/write compressed bitmaps to disk */

#include <R.h>
#include <Rinternals.h>

/* used throughout, so defined globally */
int bitmask[32];
int nbitmask[32];

#define BITMAP_AND 1
#define BITMAP_OR  2

void create_bitmask (void){
  int i;
  /* little-endian for now */
  for(i=0; i<32; i++) {
    bitmask[i] = 1 << i;
    nbitmask[i] = ~bitmask[i];
  }
}

SEXP make_bitmask () {
  create_bitmask();
  return R_NilValue;
}

/*  
    create fixed length zero-bitmap to eventually
    memcpy from.  This would be called for each
    bitmap you would need to inherit from.  Subsequent
    creations per call would then use `new_bitmap`
    to make a fast memcpy of the zeroed out vector
*/
SEXP make_bitmap (SEXP _len) {
  int i, len = (int)(INTEGER(_len)[0]/32 + 1);
  SEXP _zb;

  PROTECT(_zb = allocVector(INTSXP, len));
  int *zb = INTEGER(_zb);

  for(i=0; i<len; i++)
    zb[i] = 0;

  /* add class=c("bitmap","rowid") */
  SEXP _class;
  PROTECT(_class = allocVector(STRSXP, 2));
  SET_STRING_ELT(_class, 0, mkChar("bitmap"));
  SET_STRING_ELT(_class, 1, mkChar("rowid"));
  setAttrib(_zb, R_ClassSymbol, _class);

  UNPROTECT(2);
  return _zb;   
}

SEXP new_bitmap (SEXP _zb) {
  /* memcpy is ~50% faster than creating a new vector */
  SEXP _bitmap;
  PROTECT(_bitmap = allocVector(INTSXP, length(_zb)));

  memcpy(INTEGER(_bitmap), INTEGER(_zb), length(_zb) * sizeof(int));

  /* add class=c("bitmap","rowid") */
  SEXP _class;
  PROTECT(_class = allocVector(STRSXP, 2));
  SET_STRING_ELT(_class, 0, mkChar("bitmap"));
  SET_STRING_ELT(_class, 1, mkChar("rowid"));
  setAttrib(_bitmap, R_ClassSymbol, _class);

  UNPROTECT(2);
  return _bitmap;
}

SEXP bitmap_logical (SEXP _e1, SEXP _e2, SEXP _zb, SEXP _type) {
  /* _e1, _e2 and _zb must all be of the same length */
  if(length(_e1) != length(_e2) ||
     length(_e1) != length(_zb))
    error("bitwise AND requires equal length bitmaps");

  SEXP _bitmap;
  PROTECT(_bitmap = new_bitmap(_zb));
  int i, len;
  len = length(_e1);

  int *bitmap = INTEGER(_bitmap),
      *e1     = INTEGER(_e1),
      *e2     = INTEGER(_e2);

  switch(INTEGER(_type)[0]) {
  case BITMAP_AND:
    for(i=0; i<len; i++)
      bitmap[i] = e1[i] & e2[i];
    break;
  case BITMAP_OR:
    for(i=0; i<len; i++)
      bitmap[i] = e1[i] | e2[i];
    break;
  default:
    error("unimplemented");
  }

  UNPROTECT(1);
  return _bitmap;
}
SEXP bitmap_dlogical (SEXP _e1, SEXP _e2, SEXP _type) {
  /* destructive!  BEWARE!!! */
  if(length(_e1) != length(_e2))
    error("bitwise AND requires equal length bitmaps");

  int i, len;
  len = length(_e1);

  int *e1     = INTEGER(_e1),
      *e2     = INTEGER(_e2);

  switch(INTEGER(_type)[0]) {
  case BITMAP_AND:
    for(i=0; i<len; i++)
      e1[i] = e1[i] & e2[i];
    break;
  case BITMAP_OR:
    for(i=0; i<len; i++)
      e1[i] = e1[i] | e2[i];
    break;
  default:
    error("unimplemented");
  }

  return _e1;
}

SEXP which_bits (SEXP _bits) {
  int i, b, count=0;
  int *bits = INTEGER(_bits);
  int bits_len = length(_bits);

  /* probably need some sort of block doubling 
     routine, as 32x a big vector is too big 
     Here we opt to count first. About 15% overhead.
  */
  for(b=0; b<bits_len; b++) {
  for(i=0; i<32; i++) {
    if(bitmask[i] & bits[b]) {
      count++;
    }
  }
  }

  SEXP _index;
  PROTECT(_index=allocVector(INTSXP,count));
  int *index = INTEGER(_index);
  count = 0;
  for(b=0; b<bits_len; b++) {
  for(i=0; i<32; i++) {
    if(bitmask[i] & bits[b]) {
      index[count] = i+(32*b)+1; 
      count++;
    }
  }
  }
  UNPROTECT(1);
  return _index;
}

SEXP count_bits (SEXP _bits) {
  int i, b, count=0;
  int *bits = INTEGER(_bits);
  int bits_len = length(_bits);

  for(b=0; b<bits_len; b++) {
  for(i=0; i<32; i++) {
    if(bitmask[i] & bits[b]) {
      count++;
    }
  }
  }
  return ScalarInteger(count);
}

SEXP bitmap_replace (SEXP _elem, SEXP _bitmap) {
  /* bitwise operation on select chunks
     of bitmap.  DONE IN PLACE!!! BEWARE!!!  */
  int i;
  int off, chunk, len;
  int *elem   = INTEGER(_elem),
      *bitmap = INTEGER(_bitmap);
  len = length(_elem);

  for(i = 0; i < len; i++) {
    off = (int)((elem[i]-1) % 32); 
    chunk = (elem[i]-1) / 32;
    bitmap[chunk] = bitmap[chunk] | bitmask[off];
  }

  return _bitmap;
}

SEXP bitmap_zero (SEXP _elem, SEXP _bitmap) {
  /* bitwise operation on select chunks
     of bitmap.  DONE IN PLACE!!! BEWARE!!!  */
  int i;
  int off, chunk, len;
  int *elem   = INTEGER(_elem),
      *bitmap = INTEGER(_bitmap);
  len = length(_elem);

  for(i = 0; i < len; i++) {
    off = (int)((elem[i]-1) % 32); 
    chunk = (elem[i]-1) / 32;
    bitmap[chunk] = bitmap[chunk] & nbitmask[off];
  }
  return _bitmap;
}

SEXP file_to_bitmap (SEXP _file, SEXP _from, SEXP _len, SEXP _bitmap) {
  FILE *fd;
  fd = fopen(CHAR(STRING_ELT(_file,0)), "rb");

  int i=0, off, chunk, len;
  int *ints,
      *bitmap = INTEGER(_bitmap);

  fseek(fd, INTEGER(_from)[0] * sizeof(int), SEEK_SET);
  //Rprintf("%i, %i \n", len, ftell(fd));
  //while(!feof(fd)) {
  len = INTEGER(_len)[0];
  while(i < len) {
    fread(ints, sizeof(int), 1, fd);
    off = (int)((ints[0]-1) % 32); 
    chunk = (ints[0]-1) / 32;
    bitmap[chunk] = bitmap[chunk] | bitmask[off];
    i++;
  }
  fclose(fd);
  return _bitmap;
}


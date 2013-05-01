#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

SEXP bitFlip( SEXP a, SEXP bitWidth );
SEXP bitAnd( SEXP a,  SEXP b );
SEXP bitOr ( SEXP a,  SEXP b );
SEXP bitXor( SEXP a,  SEXP b );
SEXP bitShiftL( SEXP a,  SEXP b);
SEXP bitShiftR( SEXP a,  SEXP b);

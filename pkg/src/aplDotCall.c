#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

// SEXP APLDECODE( SEXP, SEXP );
// SEXP APLENCODE( SEXP, SEXP );
// SEXP APLSELECT( SEXP, SEXP, SEXP );
// SEXP APLTRANSPOSE( SEXP, SEXP, SEXP, SEXP, SEXP );
// SEXP APLSCAN( SEXP, SEXP, SEXP, SEXP, SEXP );
// SEXP APLREDUCE( SEXP, SEXP, SEXP, SEXP, SEXP, SEXP );
// SEXP APLINNERPRODUCT( SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP );

SEXP
APLDECODE( SEXP cell, SEXP dims )
{
    int  aux = 1, n = length( dims ), nProtected = 0;
	PROTECT(   cell = AS_INTEGER( cell ) ); ++nProtected;
    int *icell = INTEGER_POINTER( cell );
    SEXP ind;
    PROTECT( ind = allocVector( INTSXP, 1 ) ); ++nProtected;
    INTEGER( ind )[0] = 1;
    for( int i = 0; i < n; i++ ) {
        INTEGER( ind )[0] += aux * ( icell[i] - 1 );
        aux *= INTEGER( dims )[i];
    }
    UNPROTECT( nProtected );
    return ind;
}

SEXP
APLENCODE( SEXP ind, SEXP dims )
{
    int  n = length( dims ), aux = INTEGER( ind )[0], pdim = 1, nProtected = 0;
    SEXP cell;
    PROTECT( cell = allocVector( INTSXP, n ) ); ++nProtected;
    int *icell = INTEGER_POINTER( cell );
    for( int i = 0; i < n - 1; i++ ){
        pdim *= INTEGER( dims )[i];
    }
    for( int i = n - 1; i > 0; i-- ){
		icell[i] = ( aux - 1 ) / pdim;
		aux -= pdim * icell[i];
        pdim /= INTEGER( dims )[i - 1];
        icell[i] += 1;
    }
 	icell[0] = aux;
    UNPROTECT( nProtected );
    return cell;
}

SEXP
APLSELECT( SEXP a, SEXP dima, SEXP list )
{
   int  r = length( dima ), lz = 1, dimzi, nProtected = 0;
   SEXP dimz, itel, cell, czll, nind, z;
   PROTECT( dimz = allocVector( INTSXP, r ) ); ++nProtected;
   PROTECT( cell = allocVector( INTSXP, r ) ); ++nProtected;
   PROTECT( czll = allocVector( INTSXP, r ) ); ++nProtected;
   PROTECT( itel = allocVector( INTSXP, 1 ) ); ++nProtected;
   PROTECT( nind = allocVector( INTSXP, 1 ) ); ++nProtected;
   for( int i = 0; i < r; i++ ){
       dimzi = length( VECTOR_ELT( list, i ) );
       INTEGER( dimz )[i] = dimzi;
       lz *= dimzi;
   }
   PROTECT( z = allocVector( REALSXP, lz ) );  ++nProtected;
   for( int i = 0; i < lz; i++ ){
       INTEGER( itel )[0] = i + 1;
       cell = APLENCODE ( itel, dimz );
       for ( int j = 0; j < r; j++ ) {
           INTEGER ( czll )[j] = INTEGER ( VECTOR_ELT( list, j ) )[INTEGER( cell )[j] - 1];
       }
       nind = APLDECODE( czll, dima );
       REAL( z )[i] = REAL( a )[INTEGER( nind )[0] - 1];
   }
   UNPROTECT( nProtected );
   return( z );
}

SEXP
APLTRANSPOSE( SEXP a, SEXP x, SEXP sa, SEXP sz, SEXP rz )
{
    int  na = 1, nz = 1, ra = length( sa ), lsz = length( sz ), nProtected=0;
    SEXP ivec, jvec, z, itel, nind;
    for( int i = 0; i < ra ; i++ ){ na *= INTEGER( sa )[i]; }
    for( int i = 0; i < lsz; i++ ){ nz *= INTEGER( sz )[i]; }
    PROTECT( itel = allocVector( INTSXP,              1  ) ); ++nProtected;
    PROTECT( nind = allocVector( INTSXP,              1  ) ); ++nProtected;
    PROTECT( ivec = allocVector( INTSXP,  INTEGER(rz)[0] ) ); ++nProtected;
    PROTECT( jvec = allocVector( INTSXP,             ra  ) ); ++nProtected;
    PROTECT( z    = allocVector( REALSXP,            nz  ) ); ++nProtected;
    for( int i = 0; i < nz; i++ ){
        INTEGER( itel )[0] = i + 1;
        ivec = APLENCODE( itel, sz );
        for( int j = 0; j < ra; j++ ){
            INTEGER( jvec )[j] = INTEGER( ivec )[INTEGER( x )[j] - 1];
        }
        nind = APLDECODE( jvec, sa );
        REAL( z )[i] = REAL( a )[INTEGER(nind)[0] - 1];
    }
    UNPROTECT( nProtected );
    return z;
}

SEXP
APLEXPAND( SEXP a, SEXP sa, SEXP p, SEXP se, SEXP axis )
{
    int  na = 1, nz = 1, lsa = length( sa ), lse = length( se ), nProtected = 0;
    for( int i = 0; i < lsa; i++ ){ na *= INTEGER( sa )[i]; }
    for( int i = 0; i < lse; i++ ){ nz *= INTEGER( se )[i]; }
    SEXP ivec, z,itel, nind;
    PROTECT( itel = allocVector( INTSXP,    1  ) ); ++nProtected;
    PROTECT( nind = allocVector( INTSXP,    1  ) ); ++nProtected;
    PROTECT( ivec = allocVector( INTSXP,  lsa  ) ); ++nProtected;
    PROTECT( z    = allocVector( REALSXP,  nz  ) ); ++nProtected;
    for( int i = 0; i < na; i++ ){
        INTEGER( itel )[0] = i + 1;
        ivec = APLENCODE( itel, sa );
        INTEGER( ivec )[INTEGER( axis )[0] - 1] = INTEGER( p )[INTEGER( ivec )[INTEGER( axis )[0] - 1]-1];
        nind = APLDECODE( ivec, se );
        REAL( z )[INTEGER( nind )[0] - 1] = REAL( a )[i];
    }
    UNPROTECT( nProtected );
    return z;
}

SEXP
APLREDUCE( SEXP f, SEXP a, SEXP k, SEXP sa, SEXP sz, SEXP env )
{
    int u, r, kk, na = 1, nz = 1, nProtected = 0;
    int nk = length( k ), ra = length( sa ), rz = length( sz );
    SEXP ivec, kvec, ind, z,itel, nind, R_fcall= R_NilValue;
    SEXP Z = R_NilValue, A = R_NilValue;
    for( int i = 0; i < ra; i++ ){ na *= INTEGER( sa )[i]; }
    for( int i = 0; i < rz; i++ ){ nz *= INTEGER( sz )[i]; }
    kk = ra - nk;
    PROTECT( R_fcall= lang3(f, R_NilValue, R_NilValue) ); ++nProtected;
    PROTECT( Z      = allocVector( REALSXP,       1  ) ); ++nProtected;
    PROTECT( A      = allocVector( REALSXP,       1  ) ); ++nProtected;
    PROTECT( itel   = allocVector( INTSXP,        1  ) ); ++nProtected;
    PROTECT( ivec   = allocVector( INTSXP,        ra ) ); ++nProtected;
    PROTECT( kvec   = allocVector( INTSXP,        kk ) ); ++nProtected;
    PROTECT( ind    = allocVector( INTSXP,        nz ) ); ++nProtected;
    PROTECT( z      = allocVector( REALSXP,       nz ) ); ++nProtected;
    for( int i = 0; i < nz; i++ ){
        INTEGER( ind )[i] = 0;
    }
    for( int i = 0; i < na; i++ ){
        INTEGER( itel )[0] = i + 1;
        ivec = APLENCODE( itel, sa );
        u = 0;
        for( int j = 0; j < ra; j++ ){
            r = 0;
            for( int v = 0; v < nk; v++ ){
                if( j == ( INTEGER( k )[v] - 1 ) ) r = 1;
            }
            if( r == 0 ){
                INTEGER( kvec )[u] = INTEGER( ivec )[j];
                u += 1;
            }
        }
        nind = APLDECODE( kvec, sz );
        if ( INTEGER( ind )[INTEGER( nind )[0] - 1] == 0 ) {
            REAL( z )[INTEGER( nind )[0] - 1] = REAL( a )[i];
            INTEGER( ind )[INTEGER( nind )[0] - 1] = 1;
        } else {
            REAL( Z )[0] = REAL( z )[INTEGER( nind )[0] - 1];
            REAL( A )[0] = REAL( a )[i];
            SETCADR( R_fcall, Z );
            SETCADDR( R_fcall, A );
            REAL( z )[INTEGER( nind )[0] - 1] = REAL( eval( R_fcall, env ) )[0];
        }
    }
    UNPROTECT( nProtected );
    return z;
}

SEXP 
APLSCAN( SEXP f, SEXP a, SEXP k, SEXP sa, SEXP env )
{
    int sk, l, na=1, ra = length( sa ),nProtected=0;
    for( int i = 0; i < ra; i++ ){ na *= INTEGER( sa )[i]; }
    SEXP ivec, z, itel, nind, Z, A,R_fcall=R_NilValue;
    PROTECT( R_fcall = lang3( f, R_NilValue, R_NilValue ) ); ++nProtected;
    PROTECT( itel    = allocVector( INTSXP,          1  ) ); ++nProtected;
    PROTECT( nind    = allocVector( INTSXP,          1  ) ); ++nProtected;
    PROTECT( Z       = allocVector( REALSXP,         1  ) ); ++nProtected;
    PROTECT( A       = allocVector( REALSXP,         1  ) ); ++nProtected;
    PROTECT( ivec    = allocVector( INTSXP,          ra ) ); ++nProtected;
    PROTECT( z       = allocVector( REALSXP,         na ) ); ++nProtected;
    l = INTEGER( k )[0] - 1;
    for( int i = 0; i < na; i++ ){
        INTEGER( itel )[0] = i + 1;
        ivec = APLENCODE( itel, sa );
        sk = INTEGER( ivec )[l];
        if( sk == 1 ){
             REAL( z )[i] = REAL( a )[i];
        }else{
            INTEGER( ivec )[l] -= 1;
            nind = APLDECODE( ivec, sa );
            REAL( Z )[0]=REAL( z )[INTEGER( nind )[0]-1];
            REAL( A )[0]=REAL( a )[i];
            SETCADR( R_fcall, Z );
            SETCADDR( R_fcall, A );
            REAL( z )[i] = REAL( eval( R_fcall, env ) )[0];
        }
    }
    UNPROTECT( nProtected );
    return z;
}

SEXP
APLINNERPRODUCT(SEXP f, SEXP g, SEXP a, SEXP b, SEXP sa, SEXP sb, SEXP sz, SEXP ns, SEXP env)
{
    int nz = 1, nProtected = 0;
    int ra = length( sa ),rb = length( sb ), rz = length( sz );
    SEXP ivec, jvec, kvec, z, A, B, Z, itel, k, l, t;
    SEXP R_fcall = R_NilValue, R_gcall = R_NilValue;
    for( int i = 0; i < rz; i++ ){ nz *= INTEGER( sz )[i]; }
    PROTECT( R_fcall = lang3( f, R_NilValue, R_NilValue ) ); ++nProtected;
    PROTECT( R_gcall = lang3( g, R_NilValue, R_NilValue ) ); ++nProtected;
    PROTECT( itel    = allocVector( INTSXP,          1  ) ); ++nProtected;
    PROTECT( k       = allocVector( INTSXP,          1  ) ); ++nProtected;
    PROTECT( l       = allocVector( INTSXP,          1  ) ); ++nProtected;
    PROTECT( ivec    = allocVector( INTSXP,          rz ) ); ++nProtected;
    PROTECT( jvec    = allocVector( INTSXP,          ra ) ); ++nProtected;
    PROTECT( kvec    = allocVector( INTSXP,          rb ) ); ++nProtected;
    PROTECT( z       = allocVector( REALSXP,         nz ) ); ++nProtected;
    PROTECT( t       = allocVector( REALSXP,         1  ) ); ++nProtected;
    PROTECT( Z       = allocVector( REALSXP,         1  ) ); ++nProtected;
    PROTECT( B       = allocVector( REALSXP,         1  ) ); ++nProtected;
    PROTECT( A       = allocVector( REALSXP,         1  ) ); ++nProtected;
    for( int i = 0; i < nz; i++ ){
        INTEGER( itel )[0] = i + 1;
        ivec = APLENCODE( itel, sz );
        for( int j = 0; j < INTEGER( ns )[0]; j++ ){
            for( int u = 0; u < ra - 1; u++ ){
                INTEGER( jvec )[u] = INTEGER( ivec )[u];
            }
            INTEGER( jvec )[ra - 1] = j + 1;
            k = APLDECODE( jvec, sa );
            for( int u = 1; u < rb; u++ ){
                INTEGER( kvec )[u] = INTEGER( ivec )[ra + u - 2];
            }
            INTEGER( kvec )[0] = j + 1;
            l = APLDECODE( kvec,sb );
            REAL( A )[0] = REAL( a )[INTEGER( k )[0]-1];
            REAL( B )[0] = REAL( b )[INTEGER( l )[0]-1];
            SETCADR( R_fcall, A );
            SETCADDR( R_fcall, B );
            REAL( t )[0] = REAL( eval( R_fcall, env ) )[0];
            if( j == 0 ){ 
                REAL( z )[i] = REAL( t )[0];
            } else {
                REAL( Z )[0] = REAL( z )[i];
                SETCADR( R_gcall, t );
                SETCADDR( R_gcall, Z );
                REAL( z )[i] = REAL( eval( R_gcall, env ) )[0];
            }
        }
    }
    UNPROTECT( nProtected );
    return z;
}


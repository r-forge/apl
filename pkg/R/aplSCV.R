aplSCV <- function( x, f = "+" ){
    if ( length( x ) <= 1 ) return( x )
    return( sapply( 1:length( x ), function( i ) aplRDV( x[1:i], f ) ) )
}

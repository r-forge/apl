aplScan <- function( a, k, f = "+" ){
    if( is.vector( a ) ){ return( aplSCV( a, f ) ); }
    if( !is.function( f ) ){ f <- match.fun( f ); }
    env <- new.env( );
    environment( f ) <- env;
    sa <-aplShape(a); ra <-aplRank(a); 
    z  <- .Call( "APLSCAN",
                 f, 
                 as.double( a ), 
                 as.integer( k ),
                 as.integer( sa ),
                 as.integer( ra ),
                 env
               );
    return( array( z, sa ) );
}


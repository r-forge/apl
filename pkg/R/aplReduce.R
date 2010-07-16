aplReduce <- function( a, k, f = "+" ){
    if( is.vector( a ) ){
        return( aplRDV( a, f ) );
    }
    nk  <- length(k);
    sa  <- aplShape(a); 
    sz  <- sa[(1:length(sa))[-k]]; 
    if( !is.function( f ) ){ f <- match.fun( f ); }
    env <- new.env( );
    environment( f ) <- env;
    z <- .Call( "APLREDUCE", 
                f, 
                as.double(a), 
                as.integer(k),  
                as.integer(sa), 
                as.integer(sz), 
                env
              );
    return( array( z, sz ) )
}


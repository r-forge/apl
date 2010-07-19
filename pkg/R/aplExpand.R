aplExpand <- function( x, y, axis = 1 ){
    if( is.vector( x ) ){ return( aplEXV( x, y ) ) }
    d <- dim( x ); m <- which( y ); e <- d; e[axis] <- m;
    if( m != d[axis] ){
        stop("Incorrect dimension length")
    }
    z <- array( 0, e )
    apply( z,( 1:n )[-axis],function(i) z[i]<-x[i])
}


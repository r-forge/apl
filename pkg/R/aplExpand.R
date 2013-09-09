# aplExpando <- function( x, y, axis = 1 ){
#     if( is.vector( x ) ){ return( aplEXV( x, y ) ) }
#     d <- dim( x ); m <- which( y ); l <- length( y ); e <- d; e[axis] <- l
#     if( length( m ) != d[axis] ){
#         stop( "Incorrect dimension length" )
#     }
#     z <- array( 0, e )
#     for( i in 1 : prod( d ) ) {
#         l <- aplEncode(i, d)
#         l [axis] <- m[l[axis]]
#         z[aplDecode( l, e )] <- x[i]
#     }   
#     return( z )
# }

aplEXV <- function( x, y ){
    z <- rep( 0, length( y ) )
    m <- which( y )
    if( length( m ) != length( x ) ){
        stop("Incorrect vector length")
    }
    z[m] <- x
    return( z )
}

aplExpand <- function( x, y, axis = 1 ){
    if( is.vector( x ) ){ return( aplEXV( x, y ) ) }
    d <- dim( x ); m <- which( y ); l <- length( y ); e <- d; e[axis] <- l
    if( length( m ) != d[axis] ){
        stop( "Incorrect dimension length" )
    }
    z <- .Call( "APLEXPAND", 
                as.double(     x ), 
                as.integer(    d ),
                as.integer(    m ),
                as.integer(    e ), 
                as.integer( axis ),
                package = "apl" 
              );
    z <- array( z, e );
    return( z )
}


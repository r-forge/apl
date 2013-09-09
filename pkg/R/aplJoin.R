aplJoin <-
function(a,b,k) {
    if (is.vector(a) && is.vector(b)) return(c(a,b))
    sa<-aplShape(a); sb<-aplShape(b); ra<-aplRank(a); rb<-aplRank(b)
    if (ra != rb)
        stop("Rank error in aplJoin")
    if (!identical(sa[-k],sb[-k]))
        stop("Shape error")
    sz<-sa; sz[k]<-sz[k]+sb[k]; 
    z <- .Call( "APLJOIN", 
                as.double(     a ), 
                as.integer(   sa ),
                as.double(     b ), 
                as.integer(   sb ),
                as.integer(   sz ),
                as.integer(    k ),
                package = "apl" 
              );
    z <- array( z, sz );
    return(z)
}

# aplJoin <-
# function(a,b,k) {
#     if (is.vector(a) && is.vector(b)) return(c(a,b))
#     sa<-aplShape(a); sb<-aplShape(b); ra<-aplRank(a); rb<-aplRank(b)
#     if (ra != rb)
#         stop("Rank error in aplJoin")
#     if (!identical(sa[-k],sb[-k]))
#         stop("Shape error")
#     sz<-sa; sz[k]<-sz[k]+sb[k]; nz<-prod(sz); u<-unit(k,ra)
#     z<-array(0,sz)
#     for (i in 1:nz) {
#         ivec<-aplEncode(i,sz)
#         if (ivec[k] <= sa[k]) z[i]<-a[aplDecode(ivec,sa)]
#             else z[i]<-b[aplDecode(ivec-sa[k]*u,sb)]
#         }
# return(z)
# }


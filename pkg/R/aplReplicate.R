aplReplicate <-
function(x,y,k) {
    if (is.vector(x)) return(aplCRV(x,y))
    sx<-aplShape(x); sy<-aplShape(y); sk<-sx[k]
    if (sy == 1) y<-rep(y,sk)
    if (length(y) != sk)
        stop("Length Error")
    sz<-sx; sz[k]<-sum(y); nz<-prod(sz)
    gg<-aplCRV(1:sk,y)
    z<-array(0,sz)
    for (i in 1:nz){
        jvec<-aplEncode(i,sz)
        jvec[k]<-gg[jvec[k]]
        z[i]<-x[aplDecode(jvec,sx)]
    }
return(z)
}


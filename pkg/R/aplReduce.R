aplReduce <-
function(a,k,f="+") {
if (is.vector(a)) 
    return(aplRDV(a,f))
ff<-if (is.function(f)) f else match.fun(f)
sa<-aplShape(a); ra<-aplRank(a); na<-prod(sa)
sz<-sa[(1:ra)[-k]]; z<-array(0,sz); rz<-aplRank(z); nz<-prod(sz)
z<-.C("aplReduceC",list(ff),as.double(a),as.integer(k),as.integer(length(k)),as.integer(na),
    as.integer(sa),as.integer(ra),as.integer(nz),as.integer(sz),as.integer(rz),as.double(z))
return(array(z[[11]],sz))
}


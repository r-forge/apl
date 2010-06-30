aplTranspose <-
function(a,x=rev(1:aplRank(a))) {
sa<-aplShape(a); ra<-aplRank(a); na<-prod(sa)
if (length(x) != ra)
    stop("Length Error")
rz<-max(x); sz<-rep(0,rz)
for (i in 1:rz)
    sz[i]<-min(sa[which(x==i)])
nz<-prod(sz); z<-array(0,sz)
array(.C("aplTransposeC",as.double(a),as.integer(x),as.integer(sa),as.integer(ra),as.integer(na),
	as.integer(sz),as.integer(rz),as.integer(nz),as.double(z))[[9]],sz)
}


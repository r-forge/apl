aplSelect <-
function(a,x,drop=FALSE) {
sa<-aplShape(a); ra<-aplRank(a)
sz<-sapply(x,length); z<-array(0,sz); rz<-aplRank(z); nz<-prod(sz)
z<-array(.C("aplSelectC",as.double(a),as.integer(sa),as.integer(ra),lapply(x,as.integer),
    as.double(z),as.integer(sz),as.integer(rz),as.integer(nz))[[5]],sz)
if (drop) return(drop(z)) else return(z)
}


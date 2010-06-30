aplInnerProduct <-
function(a,b,f="*",g="+") {
sa<-aplShape(a); sb<-aplShape(b)
ra<-aplRank(a); rb<-aplRank(b)
ia<-1:(ra-1); ib<-(ra-1)+(1:(rb-1))
ff<-match.fun(f); gg<-match.fun(g)
ns<-last(sa); nt<-first(sb)
if (ns != nt) 
    stop("Incompatible array dimensions")
sz<-c(butLast(sa),butFirst(sb)); nz<-prod(sz)
z<-array(0,sz); rz<-aplRank(z)
res<-.C("aplInnerProductC",list(ff,gg),as.double(a),as.double(b),as.integer(sa),as.integer(ra),
	as.integer(sb),as.integer(rb),as.integer(sz),as.integer(rz),as.integer(nz),as.integer(ns),as.double(z))
return(array(res[[12]],sz))
}


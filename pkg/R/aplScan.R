aplScan <-
function(a,k,f="+") {
if (is.vector(a)) 
	return(aplSCV(a,f))
ff<-if (is.function(f)) f else match.fun(f)
sa<-aplShape(a); ra<-aplRank(a); sk<-sa[k]; u<-unit(k,ra)
na<-prod(sa); z<-a
res<-.C("aplScanC",list(ff),as.double(a),as.integer(k),
	as.integer(na),as.integer(sa),as.integer(ra),as.double(z))
return(array(res[[7]],sa))
}


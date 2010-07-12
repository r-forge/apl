aplEncode <-
function(ind,dims) {
n<-length(dims); cell<-integer(n)
if ((ind < 1) || (ind > prod(dims))) stop("No such cell")
.C("aplEncodeC",as.integer(cell),as.integer(dims),as.integer(n),as.integer(ind))[[1]]
}


aplDecode <-
function(cell,dims) {
n<-length(dims)
if (any(cell>dims) || any(cell<1)) stop("No such cell")
.C("aplDecodeC ",as.integer(cell),as.integer(dims),as.integer(n),as.integer(1))[[4]]
}

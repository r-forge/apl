aplRTV <-
function(a,k) {
    n<-aplShape(a)
    if (k > 0)
        return(c(a[-(1:k)],a[1:k]))
    if (k < 0)
        return(c(a[(n+k+1):n],a[1:(n+k)]))
    return(a)
}


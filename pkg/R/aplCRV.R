aplCRV <-
function(x,y) {
    n<-aplShape(x); m<-aplShape(y)
    if (m == 1) y<-rep(y,n)
    if (length(y) != n)
        stop("Length Error")
    z<-vector()
    for (i in 1:n)
        z<-c(z,rep(x[i],y[i]))
return(z)
}


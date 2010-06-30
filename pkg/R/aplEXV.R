aplEXV <-
function(x,y) {
z<-rep(0,length(y))
m<-which(y)
if (length(m) != length(x))
    stop("Incorrect vector length")
z[which(y)]<-x
return(z)
}


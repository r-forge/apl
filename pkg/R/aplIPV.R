aplIPV <-
function(x,y,f="*",g="+"){
if (length(x) != length(y))
    stop("Incorrect vector length")
if (length(x) == 0) return(x)
z<-match.fun(f)(x,y)
return(aplRDV(z,g))
}


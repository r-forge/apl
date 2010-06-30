aplTake <-
function(a,x,drop=FALSE) {
sa<-aplShape(a); ra<-aplRank(a)
y<-as.list(rep(0,ra))
for (i in 1:ra) {
    ss<-sa[i]; xx<-x[i]; sx<-ss-xx
    if (xx > 0) y[[i]]<-1:xx
    if (xx < 0) y[[i]]<-(sx+1):ss
    }
return(aplSelect(a,y,drop))
}


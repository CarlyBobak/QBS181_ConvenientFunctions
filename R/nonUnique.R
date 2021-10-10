nonUnique <-
function(x){
  u<-unique(x)
  return(x[x%in%u==F])
}

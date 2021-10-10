library(stats)

#remove NAs based on specified columns in the data
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#function to calculate the mode of a variable
Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

#function to calculate geometric mean
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

#function to  calculate the factorial of a number:
factorial <- function(x){
  if(x==0)
    return(1)
  else
    return(x*factorial(x-1))
} #note this function is recursive!

#not unique function that returns a list of non-unique values
nonUnique<-function(x){
  u<-unique(x)
  return(x[x%in%u==F])
}
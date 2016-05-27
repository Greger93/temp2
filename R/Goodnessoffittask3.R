#' @export
#' @return The function returns the chi squared test value T
#' @title Goodness of fit
#' @keywords Chi-squared Goodness_of_fit
#' @usage Goodnessoffittask3(x, p)
#' @author Sebastian Gregersen and Jeppe Korsgaard Kristensen
#' @examples reads two vectors from the nData which is the data of the soccer cards and then returns the Goodness of fit test value
#' Goodnessoffittask3(c(1,2,3),c(0.33,0.33,0.33))
#' @param x is a vector of a given data set that you want to test for a given probability
#' @param p is the given probability of a distribution for each data of x

Goodnessoffittask3 <- function(x,p){
  T <- c()
  n <- sum(x)
  for(i in 1:length(p)){
  #if(is.double(x[i])==FALSE){
  #   x[i] = 0
  # }else{
    #if(p[i] == NULL){
     # p[i] = 1/length(x)
    #}
    T <- append(T,(x[i]-n*p[i])^2/(n*p[i]))
  #  }
  }
  return(sum(T))
}
#Goodnessoffittask3(nData[,2],nData[,3])
#HUSK 3.3

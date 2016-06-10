#' @export
#' @return The function returns the chi squared test value T
#' @title Goodness of fit
#' @keywords Chi-squared Goodness_of_fit
#' @usage Goodnessoffittask3(x, p)
#' @author Sebastian Gregersen and Jeppe Korsgaard Kristensen
#' @examples Goodnessoffittask3(c(1,2,3),c(0.33,0.33,0.33)) reads two vectors and then returns the Goodness of fit test value
#' @param x is a vector of a given data set that you want to test for a given probability
#' @param p is the given probability of a distribution for each data of x

Goodnessoffittask3 <- function(x,p){
  T <- c()
  if(is.character(x) == TRUE || is.character(p) == TRUE){
    stop("You need to have integers or double as input")
  }
  n <- sum(x)
  for(i in 1:length(p)){
    T <- append(T,(x[i]-n*p[i])^2/(n*p[i]))
  }
  return(sum(T))
}


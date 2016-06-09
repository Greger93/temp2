#' @export
#' @return The function returns the probability that the chi square distribution with a given degree of freedom is larger than a given x
#' @title Chi probability
#' @keywords Chi-squared probability distribution estimation and monte Carlo
#' @usage chiprobability(x, df, n)
#' @author Sebastian Gregersen and Jeppe Korsgaard Kristensen
#' @examples chiprobability(1.5, 2, 10000) which return that there is approximately 47 percent chance that the chi square distribution is larger than 1.5 with 2 degrees of freedom
#' @param x is the parameter that you want the chi square distribution to be larger than
#' @param df is degrees of dreedom in the Chi square distribution
#' @param n is the amount of generated data from the chi square distribution

chiprobability <- function(x, df, n){
  #Creates n random numbers from the chi square distribution with a certain amount of degrees of freedom
  chisq <- rchisq(n,df)
  #Creates a empirical distribution function
  p <- ecdf(chisq)
  #Finds the probability that you get a value larger than the x value
  return(1-p(x))
}

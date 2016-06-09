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
  n <- sum(x)
  #Runs up to the size of the vector
  for(i in 1:length(p)){
    #if(is.character(x[i])==TRUE){
    # x[i] <- 0
    #}
    #if(is.character(p[i])==TRUE){ #" " vil også virke
    #  p[i] <- 1/length((x))
    #  print(p[i])
    #}
    #Uses the Pearson chi-squared test for each i
    T <- append(T,(x[i]-n*p[i])^2/(n*p[i]))
  }
  #The test value is the sum of those values
  return(sum(T))
}
#Goodnessoffittask3(nData[,2],nData[,3])
#HUSK 3.3
#nData <-read.table("https://raw.githubusercontent.com/haghish/ST516/5e00636e072db393354d468da5d53a7b7bf5a8d8/data/soccer.txt")


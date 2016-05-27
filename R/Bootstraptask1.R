#' @export
#' @return The function returns the estimated correlation, standard error, bias and 95 % confidence interval
#' @title Bootstrap
#' @keywords Bootstrap correlation confidence_interval
#' @usage Bootstraptask1(n,x,y)
#' @author Sebastian Gregersen and Jeppe Korsgaard Kristensen
#' @examples  Bootstraptask1(500,Data$Sport,Data$Grades) reads two vectors from the Data with 500 estimations
#' @param x is a vector of a given data set that you want to find a relation with another set
#' @param y is another vector of that you want to see if it has a relation to the first data set
#' @param n is the number of bootstrap estimations

Bootstraptask1 <- function(n,x,y){
  Corr <- cor(x,y)
  sample.size <- length(x)
  storage <- numeric(n)
  for(i in 1:n){
    j <- sample(1:sample.size, size = sample.size, replace = TRUE)
    V1 <- x[j]
    V2 <- y[j]
    storage[i] <- cor(V1, V2)
  }
  se <- sd(storage)/sqrt(sample.size)
  boot.Corr <- mean(storage)
  bias <- boot.Corr - Corr
  confidence <- qnorm(1-(1-0.95)/2)
  c1 <- mean(storage) + se*confidence
  c2 <- mean(storage) - se*confidence

  Estimations <- matrix(c(se, bias, c1, c2, boot.Corr), ncol = 1, byrow = TRUE)
  colnames(Estimations) <- c("Estimations")
  rownames(Estimations) <- c("Standard error", "Bias", "conf interval 1", "conf interval 2", "Correlation")
  Estimations <- as.table(Estimations)
  return(Estimations)
}
#Bootstraptask1(500,Data$Sport,Data$Grades)

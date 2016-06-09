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
  Corr <- cor(x,y) #Correlation between the given datas
  sample.size <- length(x)
  storage <- numeric(n) #creates a vector with 0's which is used to find the estimated corrolations
  for(i in 1:n){ #a forloop which runs n times and find n correlations
    j <- sample(1:sample.size, size = sample.size, replace = TRUE) #makes numbers between 1 and the length of the data sets(used as index)
    V1 <- x[j] #looking at the places in the first data set according to the indexes of j.
    V2 <- y[j] #looking at the places in the second data set according to the indexes of j.
    storage[i] <- cor(V1, V2) #stores the correlation of the new created samples n times in storages from V1 and V2, which is datapoints from the original data.
  }
  se <- sd(storage)/sqrt(sample.size) #standard error
  boot.Corr <- mean(storage) #Estimated correlation is found as the average/mean of the n new correlation
  bias <- boot.Corr - Corr #finds the differens between the correlation and the estimated correlation
  confidence <- qnorm(1-(1-0.95)/2) #Used to find the confidens-interval
  c1 <- mean(storage) + se*confidence #one side of the confidens-interval
  c2 <- mean(storage) - se*confidence #second side of the confidens-interval
  #making a table of the parameters found before.
  Estimations <- matrix(c(se, bias, c1, c2, boot.Corr), ncol = 1, byrow = TRUE)
  colnames(Estimations) <- c("Estimations")
  rownames(Estimations) <- c("Standard error", "Bias", "conf interval 1", "conf interval 2", "Correlation")
  Estimations <- as.table(Estimations)
  return(Estimations) #returns and prints the table of parameters.
}

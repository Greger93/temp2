#' @export
#' @return a density plot of a data set with a given method
#' @title Density plot
#' @keywords Density_plot gaussian_kernel Sturges_naive
#' @usage plotdens(x, n, method, from, to)
#' @author Sebastian Gregersen and Jeppe Korsgaard Kristensen
#' @examples plotdens(rgamma(10000,1), 500, "naive", 0, 6) returns a density plot of a gamma distribution with mean 1 and the naive method is used in the inverval between 0 and 6.
#' @param x is the data set you want to make a density plot of
#' @param n is the number of points used for plotting the density function.
#' @param method is either naive or kernel depending on which method you want to use.
#' @param from is the start point of your plot.
#' @param to is the end point of your plot.

plotdens <- function(x, n = 500, method = "naive", from = min(x)-(sd(x)/3), to = max(x)+(sd(x)/3)){
  Probabilities <- c()
  if(method != "naive" && method != "kernel"){
    return("not a specified method") #if a wrong method is written, the function stops and says, that it is not a specified method
  }
  Interval <- seq(from, to, by = (to-from)/(n-1)) #Finds the interval according the "from" and "to" which is the function is taken. Furthermore it finds the points in which is finds the probabilities to plot.
  for(i in 1:n){ #finds all the probabilites of the interval-sequence
    Probabilities <- append(Probabilities, estdens(x, d = Interval[i], method = method)) #Uses the function "estdens" to find the n probabilities, at each point of the interval-sequence.
  }
  plot(Interval, Probabilities, type = 'l', main = "Density plot") #plots all the n probabilities and binds the points with the "l" shape.
}

#' @export
#' @return a probability of a given point d if a d value is given. If the d value is not defined it returns the probability of the quantiles and the mean
#' @title Density estimation
#' @keywords Density_estimation Gaussian_Kernel Sturges_naive
#' @usage estdens(x, d, h, method)
#' @author Sebastian Gregersen and Jeppe Korsgaard Kristensen
#' @examples estdens(rgamma(10000,1), 1, 1, "naive") which returns the probability of 1 with a bandwidth of 1 and with a random gamma distribution with mean 1.
#' @param x is the data set you want to obtain probabilities from
#' @param d is the point in which you want to approximate the probability
#' @param h is the bandwidth which is the width of the categories that you want to the data into.
#' @param method is either naive or kernel depending on which method you want to use.

estdens <- function(x, d = 0, h =0, method = "naive"){
  vector <- c()
  if (d == 0) { #if no point to find the density is given, the function find the probabilitis of the quantiles and the mean
    Q = as.numeric(quantile(x)) #adding the quantiles
    Q = append(Q,mean(x)) #adding the mean
    Q = sapply(Q, function(q) estdens(x,q,h,method)) #estimates the density of the quanttiles and mean with the same x,h and method.
    Quantiles <- matrix(c(Q[1], Q[2], Q[3], Q[4], Q[5], Q[6]),
                        ncol = 1, byrow = TRUE)
    colnames(Quantiles) <- c("Probabilities")
    rownames(Quantiles) <- c("Min", "1st quartile", "Median",
                             "3rd quartile", "Max", "Mean")
    return(Quantiles) #returns and prints the table of the founded probabilites.
  }
  if(method == "naive"){
    if (h == 0) { #if no banwitdh is given, it calculates the banwitdh using the Struges.
      h = (max(x)-min(x))/(1+log2(length(x)))
    } #calculates the density with the naive method
    for(i in 1:length(x)) {
      if(abs((d-x[i])/h)<1) { #the indecator functino of the naive method
        vector <- append(vector,(1/2))
      }
    }
    return((1/(h*length(x)))*sum(vector)) #returns the density of the given point with the naive method.
  } else if (method == "kernel"){
    if (h == 0) { #if no banwitdh is given, it calculates the banwidth using Silverman's suggestion.
      h = 0.9*sd(x)*length(x)^(-1/5)
    }
    for(i in 1:length(x)){ #the gaussian function for kernel estimator
      vector <- append(vector, (1/sqrt(2*pi))*exp(-((d-x[i])/h)^2/2))
    }
    return(1/(length(x)*h)*sum(vector)) #returns the density of the given point with the kernel method.
  }
  else { #if a wrong method is typed as method(a method which is not naive or kernel)
    return("not a specified method")
  }
}
#estdens(data, method="kernel")
#set.seed(1)
#data <- rgamma(10000,1)
#print kun 1 h ved ikke givet h

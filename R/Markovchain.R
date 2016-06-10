#' @export
#' @return a table with the stationary probability of each state
#' @title Markov chain
#' @keywords discrete_Markov_chain transition_probability_matrix
#' @usage Markovchain(p, k, n)
#' @author Sebastian Gregersen and Jeppe Korsgaard Kristensen
#' @examples Markovchain(A,3,10000) and returns the probability for each state for 10000 steps.
#' @param p is the transition probability matrix where index [1,1] is the probability that state 1 returns to state 1 again, [1,2] is the probablity that state 1 goes to state 2 and so on
#' @param k is an integer indicating the initial state
#' @param n is the number of simulated steps
Markovchain <- function(p,k,n) {
  C <- c()
  for (i in 1:dim(p)[1]) {
    C <- append(C,0) #fills the vector with 0's.
  }
  current = k #let the chain start in stage k.
  for (i in 1:n) { #runs the chain n times.
    currentprob = 0 #currentprob is used to find the intervals of probabilites where the uniform parameter shall lie inbetween.
    U = runif(1) #uniform parameter which is used to decide in which stage you have to enter in each iteration
    for (i in 1:dim(p)[1]) { #looking at all probabilities in the matrix is the given row, which is the places you can go to from the given stage.
      if (U <= A[current,i] + currentprob && U > currentprob) { #if U is in the interval of the probability, then it should break out of the forloop, and 1 to the index of which state it is in.
        current = i
        C[i] <- C[i] + 1
        break
      }
      currentprob = currentprob + A[current,i] #is U is not in the firs interval, then currenntprob is used to find the next interval.
    }
  }
  #creates a table to show to probabilities of each stage, which is found be the amount of times you are in each state divided by the n steps around the chain.
  Stateprob <- matrix(c(C[1]/n,C[2]/n,C[3]/n,C[4]/n,C[5]/n,C[6]/n),
                      ncol = 1, byrow = TRUE)
  colnames(Stateprob) <- c("state probabilities")
  rownames(Stateprob) <- c("State 1", "State 2", "State 3",
                           "State 4", "State 5", "State 6")
  return(Stateprob) #returns and prints the probabilites.
}

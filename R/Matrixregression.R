#' @export
#' @return 2 tables, the first with coefficients for a linear model fitting the data points for each of those coefficents there is a standard error, t-value and p-value. The second table returns the residual standard error, R^2, R^2adj, F-statistic, degrees of freedom, p-value.
#' @title Multiple linear regression
#' @keywords Matrix_regression formula Multiple_linear_regression Residual
#' @usage Matrixregression(dependent ~ predictor1 + predictor2 + ...)
#' @author Sebastian Gregersen and Jeppe Korsgaard Kristensen
#' @examples Matrixregression(cars$speed~cars$dist) which returns a linear function of speed dependent on the break distance.
#' @param formula is the dependent variable as a function of the predictors.
#' @param Dependent is the variable you want to check if it fits a linear model of the predictors
#' @param Predictors are the variables that you want to see if there is a linear model between them and the dependent variable

Matrixregression <- function(formula){
  V <- c()
  Sde <- c()
  tvalue <- c()
  #Code from View(lm) which creates a matrix mf of the given dependent variable og and the predictors.
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())

  #Creates a vector of ones with the same length as the rest of the coloumns of mf.
  X <- rep(1, length(mf[,1]))
  #Creates a vector of the dependent variable
  Y <- mf[,1]
  #Adds the rest of the coloumns of mf to X
  for(i in 2:length(mf[1,])){
    X <- cbind(X, mf[,i])
  }
  #Creates a vector of the coefficients of the predictors and the intercept
  Bhat <- solve(t(X)%*%X)%*%t(X)%*%Y
  #Creates a vector which includes the names for the table
  Q<-"Intercept"
  for(i in 2:length(Bhat[,1])){
    Q <- append(Q, paste0("Predictor ", i-1))
  }

  #The estimated dependent variable
  Yhat <- X%*%Bhat
  #The error which is the difference between the dependent variable and the estimated dependent variable
  ehat <- Y - Yhat
  #Finds the residual sum of squares
  RSS <- sum(ehat^2)
  #Finds the total corrected sum of squares
  for(i in 1:length(Y)){
    V <- append(V, (Y[i]-mean(Y))^2)
  }
  SST <- sum(V)
  #Finds the regression sum of squares
  SSreg <- SST - RSS
  #Finds the degrees of freedom which is n-p-1
  df <- length(mf[,1]) - (length(mf[1,])-1)  - 1
  #Finds the residual standard error
  ReSE <- sqrt(RSS/df)
  #The G matrix is used to find the standard error
  G <- (ReSE^2*solve(t(X)%*%X))
  for(i in 1:length(G[,1])){
    #The standard error for each predictor and intercept is the square root of the diagonal
    Sde <- append(Sde, sqrt(G[i,i]))
  }
  #Finds the t-value from the nul-hypothesis which is that there does not exist a linear relation between the predictors and the dependent variable
  for(i in 1:length(G[,1])){
    tvalue <- append(tvalue, (Bhat[i,]-0)/Sde[i]) #nul hypotese der er ikke nogen sammenhæng mellem variabler
  }
  #Finds the p value for each predictor and the intercept according to the t-distribution.
  Pr <- 2*pt(-abs(tvalue), df)
  #Creates a matrix to print the values for each predictor and the intercept
  lm <- matrix(c(Bhat,Sde, tvalue, Pr), ncol = 4, nrow = length(X[1,]))
  colnames(lm) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  rownames(lm) <- c(Q)
  lm <- as.table(lm)
  print(lm)
  #Finds R2 which is the coefficient of determination of the regression line
  R2 <- SSreg/SST
  #Finds an adjusted R2 value
  R2adj <- 1-((RSS/df)/(SST/(length(mf[,1])-1)))
  #Finds the f-statistic
  Fstat <- (SSreg/(length(mf[1,])-1))/(RSS/df)
  #Finds the p-value for a significance level of 5 % of the F distribution
  pval <- 1-pf(Fstat,length(mf[1,])-1,df) #???
  #Creates a matrix to print the data
  test <- matrix(c(ReSE, R2, R2adj, Fstat, df, pval), ncol = 1, byrow = TRUE)
  colnames(test) <- c("Estimations")
  rownames(test) <- c("Residual standard error", "R2", "R2adj", "F-statistic", "Degrees of freedom", "p-value")
  test <- as.table(test)
  return(test)
}

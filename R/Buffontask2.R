#' @export
#' @return the estimated pi values with a certain amount of thrown needles
#' @title Buffon needle experiment
#' @keywords Buffon's_needle pi
#' @usage Buffontask2(N,l,d)
#' @description Buffon's needle experiment a way of estimating pi by throwing needles on a floor with parallel lines and count the number of times the needle hits the lines.
#' @author Sebastian Gregersen and Jeppe Korsgaard Kristensen
#' @param N is the amount of needles thrown
#' @param l is the length of the needles
#' @param d is the distance between the parallel lines
#' @examples add the example text
#' Buffontask2(100,1,2)


Buffontask2 <- function(N,l,d){
  PIlist = c()
  count = 0
  hit <- 0
  for(i in 1:N){
    start = runif(1,0,d)
    if(start == 0){
      hit = hit + 1
    }else{
      angle = runif(1,0,180)
      radians = angle*pi/180
      height = sin(radians)*l
      needle = height + start
      if(needle>=d){
        hit = hit+1
      }
    }
  }
  if(round(2*l*n/(d*hit),digits = 6) == round(pi,digits = 6)){
    count = count + 1
  }
  PIlist <- append(PIlist, 2*l*N/(d*hit))
  2*l/(pi*d) #analytic pHit
  hit/N #estimated pHit
  return(mean(PIlist))
}
#Buffontask2(100,1,1)

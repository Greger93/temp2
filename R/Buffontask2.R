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


Buffontask2 <- function(N,l = 1,d = 1){
  PIlist = c()
  hit <- 0
  for(i in 1:N){
    #Simulates the lowest point of the needle using the random uniform distribution.
    start = runif(1,0,d)
    #If the lowest point of the needle is on top of the first parallel line there is a hit.
    if(start == 0){
      hit = hit + 1
    }else{
      #Simulates an angle using the random uniform distribution up to 180 since if it is larger than 180 it is not the lowest point of the needle.
      angle = runif(1,0,180)
      #The angle is calculated to readians since R uses radians in the sinus, cosinus and tangens functions.
      radians = angle*pi/180
      #The vertical distance of the needle is calculated using calculation rules in perpendicular triangles.
      height = sin(radians)*l
      #The highest point of the needle is found
      needle = height + start
      #If that point is higher or equal to the parallel line there is a hit.
      if(needle>=d){
        hit = hit+1
      }
    }
  }
  #Calculates the estimated pi value of the trial
  PIlist <- append(PIlist, 2*l*N/(d*hit))
  return(mean(PIlist))
}
#Buffontask2(100,1,1)

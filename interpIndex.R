#index with linear interpolation given an index value falling between items in a sorted list

interpIndex <- function(vec1, index1){
  if(length(vec1)/2 == floor(length(vec1)/2)){
    vec1 <- sort(vec1)
    len1 <- length(vec1)
    interpDist1 <- index1*len1 - floor(index1*len1)
    interpDist2 <- 1 - interpDist1
    interpVal <- as.numeric(((vec1[floor(index1*len1)]*interpDist1) + (vec1[ceiling(index1*len1)]*interpDist2)))
  }else{
    interpVal <- sort(vec1)[ceiling(length(vec1)*index1)]
  }
  return(interpVal)
}
dScore <- function(m1, m2, sd1, sd2, n1, n2){
  # with a set of block mean latencies, sds, and ns, returns a d score
  numerator <- m1-m2
  denominator <- sqrt( ( ( (n1-1)*sd1^2+(n2-1)*sd2^2) +
                           ( (n1+n2) * ((m1-m2)^2 ) / 4) ) / (n1+n2-1))
  d <- numerator/denominator
  return(d)
}

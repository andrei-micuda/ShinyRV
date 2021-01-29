validate_probability <- function(v, p) {
  values <- array(v)
  probs <- array(p)
  print(sum(probs))
  
  #if(!is.array(values) && !is.array(probs))
  #  stop("values are not arrays.")
  
  if(length(values) != length(probs))
    stop("values do not have the same length")
  
  if(abs(1  - sum(probs)) > 0.000001)
    stop("probabilites must add up to 1")
  
  if(length(unique(values)) != length(values)) 
    stop("all values must occur only once")
  
  random_variable <- matrix(data=c(values,probs), nrow=2, byrow=TRUE)
  rownames(random_variable) <- c('Outcomes', 'Probs')
  
  random_variable
}

applyFunction <- function(m, f) {
  m[1,] = f(m[1,])
  m
}

getMean <- function(m) {
  sum(m[2,] * m[1,])
}

getVariance <- function(m) {
  getMean(applyFunction(m, function(x) x**2)) - getMean(m) ** 2
}

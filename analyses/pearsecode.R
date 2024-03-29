# This code is, at the time of writing, also available online at
#' https://github.com/willpearse/phest
#' in the form of a small R package, which may be more user-friendly
#' Calculating the limit of a distribution
#'
#' This function estimates either the the lower or upper limit of a
#' distribution of numbers given it. This is used in the manuscript to
#' calculate the onset of flowering given a set of flowering
#' dates. Examples and descriptions of how each of the arguments
#' (parameters) are given below in 'roxygen' format, along with
#' examples of how to use the function.
#'
#'
#' @param x vector of dates/times of observations, given as numbers
#' @param k how many entries in 'x' will be used to calculate the
#' estimate. See 'Methods' in the manuscript for an explanation of
#' why not all numbers need be used in the estimate
#' @param upper whether to calculate the upper limit (if TRUE) or the
#' lower limit (if FALSE, the default). In the context of plant
#' flowering phenology, the default option calculates when
#' flowering started (the focus of this manuscript)
#' @param alpha the alpha value for the confidence intervals for the
#' estimate of the limit of the distribution
#'
#' @examples
#' # Gather some observations of when flowers were in bloom
#' observations <- 5:15
#' # Estimate the onset of flowering
#' est.limit(observations)
#' # Estimate the end of flowering
#' est.limit(observations, upper=TRUE)
#' # Change the alpha value for the confidence about those observations
#' est.limit(observations, alpha=.2)
#' # Make use of fewer observations in estimating the onset (note the CI widen as a result)
#' est.limit(observations, k=10)
est.limit <- function(x, k=NULL, upper=FALSE, alpha=0.05){
  # Check the k-value is setup correctly
  k.check <- function(x, k=NULL){
    if(is.null(k))
      k <- length(x)
    if(k < 3 )
      stop("Insufficient data or insufficient time window")
    if(k > length(x)){
      warning("'k' longer than 'x'; using all of 'x'")
      k <- length(x)
    }
    return(k)
  }
  # Wrappers for parts of the calculation
  lam <- function(i, j, v){
    std <- function(x,y) (gamma(2*v+x) * gamma(v+y)) / (gamma(v+x) * gamma(y))
    inv <- function(x,y) (gamma(2*v+y) * gamma(v+x)) / (gamma(v+y) * gamma(x))
    return(ifelse(j <= i, std(i,j), inv(i,j)))
  }
  weights <- function(x){
    k <- length(x)
    v <- v.hat(x)
    lam.mat <- outer(seq_along(x), seq_along(x), lam, v)
    e <- matrix(rep(1,k), ncol=1)
    alpha <- as.vector(solve(t(e) %*% solve(lam.mat) %*% e)) * as.vector(solve(lam.mat) %*% e)
    return(alpha)
  }
  v.hat <- function(x){
    x <- sort(x, FALSE)
    if(x[1] == x[2]){
      warning("Repeated earliest measurements; applying correction")
      x <- c(x[1], x[x != x[1]])
    }
    if(length(unique(x)) == 2){
      warning("Only two unique measurement dates; unable to compute")
      return(NA)
    }
    k <- length(x)
    return((1/(k-1)) * sum(log((x[1] - x[k]) / (x[1] - x[seq(2,k-1)]))))
  }
  Sl <- function(x,k,alpha) (-log(1 - alpha/2)/k)^(-v.hat(x))
  Su <- function(x,k,alpha) (-log(alpha/2)/k)^(-v.hat(x))
  
  # Calculate the estimate
  k <- k.check(x, k)
  x <- sort(x, FALSE, decreasing=upper)
  x <- x[seq_len(k)]
  theta <- tryCatch(sum(x * weights(x)), error=function(x) NA)
  # Calculate the CIs
  upper <- tryCatch(x[1] + ((x[1]-x[k]) / (Sl(x,k,alpha) -1)), error=function(x) NA)
  lower <- tryCatch(x[1] + ((x[1] - x[k]) / (Su(x,k,alpha)-1)), error=function(x) NA)
  # Return
  return(setNames(c(theta, lower, upper), c("estimate", "conf-interval", "conf-interval")))
}
# Bootstrap SE values for estimate of limit
#
# This function bootstraps an estimate of the Standard Error for a
# particular estimate of the limit of a distribution. As discussed in
# the Methods section of the manuscript, care should be taken when
# bootstrapping estimates: the confidence intervals of these estimates
# are asymmetrical, and so a single SE value should be interpreted
# with caution. There are many ways of calculating bootstrapped
# values, and (personally) I would advise you to write your own
# function to make sure you're comfortable with what's going
# on. Examples and descriptions of how each of the arguments
# (parameters) are given below in 'roxygen' format, along with
# examples of how to use the function.
#'
#' @param x vector of dates/times of observations, given as numbers
#' @param k how many entries in 'x' will be used to calculate the
#' estimate. See 'Methods' in the manuscript for an explanation of
#' why not all numbers need be used in the estimate
#' @param n how many times to run the bootstrapping
#' @param max.iter it is not always possible to calculate an estimate
#' for the limit (see Methods), and this means it is sometimes not
#' possible to calculate an estimate across all bootstraps. If
#' such a case occurs, this parameter sets how many times the
#' function will try the bootstrapping again until the problem
#' doesn't occur.
#'
#' @examples
#' # Gather some observations of when flowers were in bloom
#' observations <- 5:15
#' # Estimate the onset of flowering
#' bootstrap.se.limit(observations)
bootstrap.se.limit <- function(x, k=NULL, n=1000, max.iter=10, upper=FALSE){
  # Check the k-value is setup correctly
  k.check <- function(x, k=NULL){
    if(is.null(k))
      k <- length(x)
    if(k < 3 )
      stop("Insufficient data or insufficient time window")
    if(k > length(x)){
      warning("'k' longer than 'x'; using all of 'x'")
      k <- length(x)
    }
    return(k)
  }
  # Wrappers for parts of the calculation
  lam <- function(i, j, v){
    std <- function(x,y) (gamma(2*v+x) * gamma(v+y)) / (gamma(v+x) * gamma(y))
    inv <- function(x,y) (gamma(2*v+y) * gamma(v+x)) / (gamma(v+y) * gamma(x))
    return(ifelse(j <= i, std(i,j), inv(i,j)))
  }
  weights <- function(x){
    k <- length(x)
    v <- v.hat(x)
    lam.mat <- outer(seq_along(x), seq_along(x), lam, v)
    e <- matrix(rep(1,k), ncol=1)
    alpha <- as.vector(solve(t(e) %*% solve(lam.mat) %*% e)) * as.vector(solve(lam.mat) %*% e)
    return(alpha)
  }
  v.hat <- function(x){
    x <- sort(x, FALSE)
    if(x[1] == x[2]){
      warning("Repeated earliest measurements; applying correction")
      x <- c(x[1], x[x != x[1]])
    }
    if(length(unique(x)) == 2){
      
      warning("Only two unique measurement dates; unable to compute")
      return(NA)
    }
    k <- length(x)
    return((1/(k-1)) * sum(log((x[1] - x[k]) / (x[1] - x[seq(2,k-1)]))))
  }
  theta.hat <- function(x, k=NULL, upper=FALSE){
    k <- k.check(x, k)
    x <- sort(x, FALSE, decreasing=upper)
    x <- x[seq_len(k)]
    tryCatch(sum(x * weights(x)), error=function(x) NA)
  }
  #Estimate parameters
  k <- k.check(x, k)
  theta <- theta.hat(x, k=k, upper=upper)
  shape <- v.hat(x)
  if(is.na(theta))
    return(c(NA,NA))
  #Handle values less than 0 (through recursion)
  if(theta < 0){
    warning("theta < 0; scaling (will unscale before return)")
    output <- Recall(x + max(abs(c(theta, min(x)))), k, n, max.iter)
    return(c(theta, output[2]))
  }
  #Simulate
  sims <- replicate(n, rpois(k, theta^shape))
  s.thetas <- apply(sims, 2, theta.hat, upper=upper)
  for(i in seq_len(max.iter)){
    s.thetas <- s.thetas[!is.na(s.thetas)]
    if(length(s.thetas) >= n){
      s.thetas <- s.thetas[seq_len(n)]
      break
    }
    sims <- replicate(n, rpois(k, theta^shape))
    s.thetas <- append(s.thetas, apply(sims, 2, theta.hat, upper=upper))
  }
  if(length(s.thetas) != n){
    warning("Iteration limit exceeded")
    return(NA)
  }
  return(setNames(c(theta,sd(s.thetas)), c("estimate", "SE")))
}


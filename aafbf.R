####################### CALCULATE AAFBF #####################################

# type: String that indicates the type of comparison of hypotheses. "equality" 
#       test for only equality vs. only inequality, "inequalities" test for only
#       inequality vs. only inequality.
# estimates: R object with estimates.
# n: numeric. Effective sample size.
# sigma: list with covariances.
# b: Numerical. Fraction of information to specify the prior.

calc_aafbf <- function(type, estimates, n, sigma, b) {
  if (type == "inequalities") {
    # Complexities
    comp1 <- .5
    comp2 <- .5
    # Fit
    fit1 <- pnorm(0, mean = estimates[1], sd = sqrt(sigma[[1]])) #Why is not dnorm?
    fit2 <- 1 - fit1
    # Calculation BFs
    bf.12 <- (fit1/comp1) / (fit2/comp2)
    bf.21 <- 1/bf.12
    #Calculation of PMPs
    pmp1 <- bf.12/(bf.12 + bf.21)
    pmp2 <- 1 - pmp1
  } else if (type == "equality") {
    
  }
}
####################### CALCULATE AAFBF #####################################

# type: String that indicates the type of comparison of hypotheses. "equality" 
#       test for only equality vs. only inequality, "inequalities" test for only
#       inequality vs. only inequality.
# estimates: R object with estimates.
# n: numeric. Effective sample size.
# sigma: list with covariances.
# b: Numerical. Fraction of information to specify the prior.
# n_eff: Effective sample size.

calc_aafbf <- function(type, estimates, n, sigma, b, n_eff) {
  if (type == "inequalities") {
    # Complexities
    comp1 <- .5
    comp2 <- .5
    
    # Fit
    fit1 <- pnorm(0, mean = estimates[1], sd = sqrt(sigma[[1]]))
    fit2 <- 1 - fit1
    
    # Calculation BFs
    #browser()
    AAFBF1u <- (fit1/comp1)
    AAFBF2u <- (fit2/comp2)
    AAFBF12 <- AAFBF1u/AAFBF2u
    AAFBF21 <- 1/AAFBF12
    
    #Calculation of PMPs
    pmp1 <- AAFBF1u/(AAFBF1u + AAFBF2u)
    pmp2 <- 1 - pmp1
    
    output <- list(bf.12 = AAFBF12, bf.21 = AAFBF21, pmp1 = pmp1, pmp2 = pmp2)
  } else if (type == "equality") {
    b <- 1/n_eff                   # b fraction according to minimal training sample (b=(J+1)/N)
    
    # complexities
    comp0 <- dnorm(0, mean = 0, sd = sqrt(sigma[[1]]/b))     # overlap of parameter under H0 and unconstrained prior -> density of the prior under Hu at the focal point 0
    comp1 <- pnorm(0, mean = 0, sd = sqrt(sigma[[2]]/b))         # always .5 for H1
    
    # Fit
    fit0 <- dnorm(0, mean = estimates[[1]], sd = sqrt(sigma[[1]])) # overlap of parameter under H0 and posterior -> density of the posterior at focal point 0
    fit1 <- 1 - pnorm(0, mean = estimates[[2]], sd = sqrt(sigma[[2]])) # the fit is equal to 1 - the fit of the complement
    
    # Calculation of BFs
    AAFBF0u <- fit0/comp0                                    # AAFBF of H0 vs Hu
    AAFBF1u <- fit1/comp1                                        # AAFBF of H0 vs Hu
    AAFBF01 <- AAFBF0u/AAFBF1u
    AAFBF10 <- 1/AAFBF01
    
    # Calculation of PMPs
    pmp0 <- AAFBF0u/(AAFBF0u + AAFBF1u)
    pmp1 <- 1 - pmp0
    
    output <- list(bf.10 = AAFBF10, bf.01 = AAFBF01, pmp0 = pmp0, pmp1 = pmp1)
  }
  #Output
  return(output)
}


# Test --------------------------------------------------------
## Data generation for testing ----------------
n2 <- 30
n1 <- 15
id <- rep(1:n2, each = n1)
condition <- rep(c(0, 1), each = n1 * n2 / 2)
intervention <- condition
control <- 1 - intervention
set.seed(97)
u0 <- rnorm(n2, 0, sqrt(0.1))
u0 <- rep(u0, each = n1)
e <- rnorm(n1 * n2, 0, sqrt(0.9))
resp <- 0 * control + 0.4 * intervention + u0 + e

#Data frame
data <- cbind(resp, intervention, control, id)
data <- as.data.frame(data)

# Multilevel analysis 
output_lmer <- lmer(resp ~ intervention + control - 1 + (1|id), data = data)
estimates <- fixef(output_lmer)
cov_intervention <- matrix(vcov(output_lmer)[1], nrow = 1, ncol = 1) #variance-covariance matrix
cov_control <- matrix(vcov(output_lmer)[4], nrow = 1, ncol = 1)
cov_list <- list(cov_intervention, cov_control)
variances <- as.data.frame(VarCorr(output_lmer))
var_u0_data <- variances[1, 4]
var_e_data <- variances[2, 4]
total_var_data <- var_u0_data + var_e_data
rho_data <- var_u0_data / total_var_data
hypoth <- "intervention>control; intervention<control"

# bain 
n_eff <- ((n1 * n2) / (1 + (n1 - 1) * rho_data)) / 2


## Actual test ---------------------------------
output_bain <- bain(estimates, hypothesis = hypoth,
                    n = c(n_eff, n_eff), group_parameters = 1, Sigma = cov_list,
                    joint_parameters = 0, fraction = 1)
print(output_bain)
output_bain$BFmatrix[1, 2] # Bayes factor H1 vs H2 
output_bain$BFmatrix[2, 1] # Bayes factor H2 vs H1 
output_bain$fit$PMPa[1] #posterior model probabilities of H1.
output_bain$fit$PMPa[2] #posterior model probabilities of H2
calc_aafbf(type = "inequalities", estimates = estimates, sigma = cov_list, n = c(n_eff, n_eff), b = 1)


# Conclusion of test
# The BF for informative hypotheses with only inequalities works and the conclusions are the same
# as the conclusions if we use bain. However, the values for BFs and PMPs are not the same as the results from bain. 
# Is this problematic? I am not sure, we end up wit the same conclusion but the difference between the numbers is big.


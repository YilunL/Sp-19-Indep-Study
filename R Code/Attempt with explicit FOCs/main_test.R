#####################################################################
# 1. Preliminaries
#####################################################################
require(numDeriv)

setwd("G:\\My Drive\\Ivan\\College\\2018-2019\\Indep Study\\R Code\\Attempt with explicit FOCs")

source("upstream_firms_test.R")
source("downstream_firms_test.R")
source("consumer_logit_test.R")

objective_m <- function(w){
  w_A = c(w[1], w[2])
  w_B = c(w[3], w[4])
  
  dpi_dwA <- grad(func = u_firm_unint, x = w_A, w_B = w_B, optim_A = 1)
  dpi_dwB <- grad(func = u_firm_unint, x = w_B, w_A = w_A, optim_A = 0)
  
  return(sqrt(sum(dpi_dwA^2, dpi_dwB^2)))
}

res <- optim(par = c(1,1,1,1), fn = objective_m, method = "BFGS", control = list(maxit = 1))


 w_A = c(1,1)
w_B = c(1,1)

dpi_dwA <- grad(func = u_firm_unint, x = w_A, w_B = w_B, optim_A = 1)
dpi_dwB <- grad(func = u_firm_unint, x = w_B, w_A = w_A, optim_A = 0)

dpi_dwA^2

res

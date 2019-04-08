#####################################################################
# 1. Preliminaries
#####################################################################
setwd("G:\\My Drive\\Ivan\\College\\2018-2019\\Indep Study\\R Code")

source("upstream_firms_test.R")
source("downstream_firms_test.R")
source("consumer_logit_test.R")

integrated = 1  # is Firm A and Firm 1 vertically integrated
linear = 0  # downstream firm with linear production function
cobb_douglas = 0  # downstream firm with cobb-douglas production function
down_mc_1 <- 0  # downstream marginal cost for firm 1
down_mc_2 <- 0   # downstream marginal cost for firm 2

ineff <- 0  # fixed loss of integration (Hart and Tirole 1990)
m_ineff <- 0 * integrated # increase in MC due to integration (unimplemented)
foreclosure <- 0 * integrated # dummy to see if firms 1-A stops selling to firm 2 (unimplemented)


M = 1  # market size

set.seed(100) #set a random seed

#####################################################################
# 2. Main
#####################################################################
# equilibrium parameters
# x_A <- 0  # upstream firm A output
# x_B <- 0  # upstream firm B output
# q_1 <- 0  # downstream firm 1 output
# q_2 <- 0  # downstream firm 2 output
# w_A <- 0  # upstream firm A price
# w_B <- 0  # upstream firm B price
# p_1 <- 0  # downstream firm 1 price
# p_2 <- 0  # downstream firm 2 price
# 
# eq <-
#   list(
#     w_A = w_A,
#     w_B = w_B,
#     x_A = x_A,
#     x_B = x_B
#   )
# 
# # eq profits
# pi_A <- 0  #upstream A
# pi_B <- 0  #upstream B
# pi_1 <- 0  #downstream 1
# pi_2 <- 0  #downstream 2
# eq_pi <- list(
#   pi_A = pi_A,
#   pi_B = pi_B,
#   pi_1 = pi_1,
#   pi_2 = pi_2
# )
# 
# # firms specific intermediate demads
# x_1A <- 0  # downstream firm 1's demand for intermediate good A
# x_2A <- 0  # downstream firm 2's demand for intermediate good A
# x_1B <- 0  # downstream firm 1's demand for intermediate good B
# x_2B <- 0  # downstream firm 2's demand for intermediate good B
# 
# eq_int_good <-
#   list(
#     w_1A = 2, # initial guess for intermediate good costs
#     w_1B = 2,
#     w_2A = 2,
#     w_2B = 2,
#     x_1A = x_1A,
#     x_1B = x_1B,
#     x_2A = x_2A,
#     x_2B = x_2B
#   )
# 
# eq_downstream_p <-
#   list(
#     p_1A = 4,  # initial guess for downstream prices
#     p_1B = 4,
#     p_2A = 4,
#     p_2B = 4
#   )

iter = 0
out_tol = 1
optim_A = 0
nruns = 4

if (integrated == 0) {
  for (run in 1:nruns) {
    
    cat("Optimizing unintegrated upstream firms \n")
    
    if (run == 1) {
      out_tol_limit = 1E-10
    } else if (run == 2) {
      out_tol_limit = 1E-9
    } else if (run == 3) {
      out_tol_limit = 1E-8
    } else {
      out_tol_limit = 1E-7
    }
    
    downstream_iter = 0
    
    while ((out_tol > out_tol_limit) & (downstream_iter < 100000)) {
      iter = iter + 1  # count iterations
      optim_A = 1 - optim_A  # optimizing which upstream firm's offer
      pi_A_old <- eq_pi$pi_A # keeping tabs on old profits
      pi_B_old <- eq_pi$pi_B
      
      # optimize firm A given firm B prices are fixed
      if (optim_A == 1) {
        firm_A_optim <-
          optim(
            par = c(eq_int_good$w_1A, eq_int_good$w_2A),
            fn = u_firm_unint,
            w_B = c(eq_int_good$w_1B, eq_int_good$w_2B),
            M = M,
            method = "BFGS",
            control = list(maxit = 100000, reltol = 1E-12)
          )
        out_tol = abs(1 - sqrt(eq_pi$pi_A ^ 2 + eq_pi$pi_B ^ 2) / sqrt(pi_A_old ^
                                                                         2 + pi_B_old ^ 2))
      } else {
        firm_B_optim <-
          optim(
            par = c(eq_int_good$w_1B, eq_int_good$w_2B),
            fn = u_firm_unint,
            w_A = c(eq_int_good$w_1A, eq_int_good$w_2A),
            M = M,
            method = "BFGS",
            control = list(maxit = 100000, reltol = 1E-12)
          )
        out_tol = abs(1 - sqrt(eq_pi$pi_A ^ 2 + eq_pi$pi_B ^ 2) / sqrt(pi_A_old ^
                                                                         2 + pi_B_old ^ 2))
      }
    }
    
    if(out_tol < out_tol_limit){
      break
    }
  }
}

if (integrated == 1) {
  for (run in 1:nruns) {

    cat("Optimizing integrated upstream firms \n")

    if (run == 1) {
      out_tol_limit = 1E-10
    } else if (run == 2) {
      out_tol_limit = 1E-9
    } else if (run == 3) {
      out_tol_limit = 1E-8
    } else {
      out_tol_limit = 1E-7
    }

    downstream_iter = 0

    while ((out_tol > out_tol_limit) & (downstream_iter < 100000)) {
      iter = iter + 1  # count iterations
      optim_A = 1 - optim_A  # optimizing which upstream firm's offer
      pi_A_old <- eq_pi$pi_A + eq_pi$pi_1 # keeping tabs on old profits
      pi_B_old <- eq_pi$pi_B

      # optimize firm A given firm B prices are fixed
      if (optim_A == 1) {
        firm_A_optim <-
          optim(
            par = eq_int_good$w_2A,
            fn = u_firm_int,
            w_B = c(eq_int_good$w_1B, eq_int_good$w_2B),
            M = M,
            method = "BFGS",
            control = list(maxit = 100000, reltol = 1E-12)
          )
        out_tol = abs(1 - sqrt((eq_pi$pi_A + eq_pi$pi_1)^ 2 + eq_pi$pi_B ^ 2) / sqrt(pi_A_old ^
                                                                         2 + pi_B_old ^ 2))
      } else {
        firm_B_optim <-
          optim(
            par = c(eq_int_good$w_1B, eq_int_good$w_2B),
            fn = u_firm_int,
            w_2A = eq_int_good$w_2A,
            M = M,
            method = "BFGS",
            control = list(maxit = 100000, reltol = 1E-12)
          )
        out_tol = abs(1 - sqrt((eq_pi$pi_A + eq_pi$pi_1)^ 2 + eq_pi$pi_B ^ 2) / sqrt(pi_A_old ^
                                                                         2 + pi_B_old ^ 2))
      }
    }

    if(out_tol < out_tol_limit){
      break
    }
  }
}


if (downstream_iter >= 100000) {
  cat("Warning: Upstream firms did not converge within relative tolerance. Check FOCs to ensure that you have indeed found an equilibrium")
}


#####################################################################
# 1. Preliminaries
#####################################################################
setwd("G:\\My Drive\\Ivan\\College\\2018-2019\\Indep Study\\R Code\\4-18 Nested Logit II")

source("upstream_firms.R")
source("downstream_firms.R")

logit = 0
nested_logit = 1

if (logit == 1){
  source("consumer_logit.R")
}
if (nested_logit == 1){ 
  source("consumer_nested_logit.R")
}

integrated <- 1   # is Firm A and Firm 1 vertically integrated
down_mc_1 <- 0  # downstream marginal cost for firm 1
down_mc_2 <- 0   # downstream marginal cost for firm 2

ineff <- 0  # fixed loss of integration (Hart and Tirole 1990)
m_ineff <- 0 * integrated # increase in MC due to integration (unimplemented)
foreclosure <- 0 * integrated # dummy to see if firms 1-A stops selling to firm 2 (unimplemented)


M = 1  # market size

set.seed(1003) #set a random seed

#####################################################################
# 2. Main
#####################################################################
# # eq profits
pi_A <- .1  #upstream A
pi_B <- .1  #upstream B
pi_1 <- .1  #downstream 1
pi_2 <- .1  #downstream 2
eq_pi <- list(
  pi_A = pi_A,
  pi_B = pi_B,
  pi_1 = pi_1,
  pi_2 = pi_2
)

# firms specific intermediate demads
x_1A <- 0  # downstream firm 1's demand for intermediate good A
x_2A <- 0  # downstream firm 2's demand for intermediate good A
x_1B <- 0  # downstream firm 1's demand for intermediate good B
x_2B <- 0  # downstream firm 2's demand for intermediate good B

eq_int_good <-
  list(
    w_1A = 2, # initial guess for intermediate good costs
    w_1B = 1,
    w_2A = 2,
    w_2B = 1,
    x_1A = x_1A,
    x_1B = x_1B,
    x_2A = x_2A,
    x_2B = x_2B
  )

eq_downstream_p <-
  list(
    p_1A = 3,  # initial guess for downstream prices
    p_1B = 3,
    p_2A = 3,
    p_2B = 3
  )

iter = 0
out_tol = 1
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
      optim_A = sample(c(0,1), 1)
      iter = iter + 1  # count iterations
      optim_A = 1 - optim_A  # optimizing which upstream firm's offer
      pi_A_old <- eq_pi$pi_A # keeping tabs on old profits
      pi_B_old <- eq_pi$pi_B
      
      old_dist <- sqrt(pi_A_old ^ 2 + pi_B_old ^ 2)
      
      # optimize firm A given firm B prices are fixed
      if (optim_A == 1) {
        firm_A_optim <-
          optim(
            par = c(eq_int_good$w_1A, eq_int_good$w_2A),
            fn = u_firm_unint,
            w_B = c(eq_int_good$w_1B, eq_int_good$w_2B),
            method = "BFGS",
            control = list(maxit = 100000, reltol = 1E-12)
          )
        eq_int_good$w_1A <- firm_A_optim$par[1]
        eq_int_good$w_2A <- firm_A_optim$par[2]
        new_dist = sqrt(eq_pi$pi_A ^ 2 + eq_pi$pi_B ^ 2)
        out_tol = abs(1 - new_dist / old_dist)
        
      } else {
        firm_B_optim <-
          optim(
            par = c(eq_int_good$w_1B, eq_int_good$w_2B),
            fn = u_firm_unint,
            w_A = c(eq_int_good$w_1A, eq_int_good$w_2A),
            method = "BFGS",
            control = list(maxit = 100000, reltol = 1E-12)
          )
        eq_int_good$w_1B <- firm_B_optim$par[1]
        eq_int_good$w_2B <- firm_B_optim$par[2]
        new_dist = sqrt(eq_pi$pi_A ^ 2 + eq_pi$pi_B ^ 2)
        out_tol = abs(1 - new_dist / old_dist)
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
      optim_A = 1 - sample(c(0,1), 1)
      iter = iter + 1  # count iterations
      optim_A = 1 - optim_A  # optimizing which upstream firm's offer
      pi_A_old <- eq_pi$pi_A # keeping tabs on old profits
      pi_B_old <- eq_pi$pi_B
      
      old_dist <- sqrt(pi_A_old ^ 2 + pi_B_old ^ 2)

      # optimize firm A given firm B prices are fixed
      if (optim_A == 1) {
        firm_A_optim <-
          optim(
            par = c(0, eq_int_good$w_2A),
            fn = u_firm_int,
            w_B = c(eq_int_good$w_1B, eq_int_good$w_2B),
            method = "BFGS",
            control = list(maxit = 100000, reltol = 1E-12)
          )
        eq_int_good$w_1A <- 0
        eq_int_good$w_2A <- firm_A_optim$par[2]
        new_dist = sqrt(eq_pi$pi_A ^ 2 + eq_pi$pi_B ^ 2)
        out_tol = abs(1 - new_dist / old_dist)
        
      } else {
        firm_B_optim <-
          optim(
            par = c(eq_int_good$w_1B, eq_int_good$w_2B),
            fn = u_firm_int,
            w_A = c(0, eq_int_good$w_2A),
            method = "BFGS",
            control = list(maxit = 100000, reltol = 1E-12)
          )
        eq_int_good$w_1B <- firm_B_optim$par[1]
        eq_int_good$w_2B <- firm_B_optim$par[2]
        new_dist = sqrt(eq_pi$pi_A ^ 2 + eq_pi$pi_B ^ 2)
        out_tol = abs(1 - new_dist / old_dist)
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


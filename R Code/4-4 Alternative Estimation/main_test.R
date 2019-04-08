#####################################################################
# 1. Preliminaries
#####################################################################
setwd("G:\\My Drive\\Ivan\\College\\2018-2019\\Indep Study\\R Code\\4-4 Alternative Estimation")

source("upstream_firms_test.R")
source("downstream_firms_test.R")
source("consumer_logit_test.R")

integrated = 1  # is Firm A and Firm 1 vertically integrated
# linear = 0  # downstream firm with linear production function
# cobb_douglas = 0  # downstream firm with cobb-douglas production function
# down_mc_1 <- 0  # downstream marginal cost for firm 1
# down_mc_2 <- 0   # downstream marginal cost for firm 2
# 
# ineff <- 0  # fixed loss of integration (Hart and Tirole 1990)
# m_ineff <- 0 * integrated # increase in MC due to integration (unimplemented)
# foreclosure <- 0 * integrated # dummy to see if firms 1-A stops selling to firm 2 (unimplemented)
# M = 1  # market size


tol = 1E-8

set.seed(100) #set a random seed

#####################################################################
# 2. Main
#####################################################################
# equilibrium parameters

eq_p <-
  list(
    p_1A = 3,  # initial guess for downstream prices
    p_1B = 3,
    p_2A = 3,
    p_2B = 3
  )

eq_w <-
  list(
    w_1A = 2, # initial guess for intermediate good costs
    w_1B = 2,
    w_2A = 2,
    w_2B = 2
  )

eq_q <- list(
  q_1A = 0,
  q_1B = 0,
  q_2A = 0,
  q_2B = 0
)

eq_pi <- list(
  pi_A = 0,
  pi_B = 0,
  pi_1 = 0,
  pi_2 = 0
)

iter = 0
out_tol = 1
optim_A = 0
nruns = 4

curr_tol = 1

if (integrated == 0){
  while (curr_tol > tol & iter < 500) {
    old_dist <- sqrt(eq_pi$pi_A ^ 2 + eq_pi$pi_B ^ 2)
    
    upstreamA <- optim(
      par = c(eq_w$w_1A, eq_w$w_2A),
      fn =  u_firm_unint,
      w_B = c(eq_w$w_1B, eq_w$w_2B),
      firm_A = 1,
      method = "BFGS",
      control = list(maxit = 10000, reltol = 1E-12)
    )
    
    eq_w$w_1A <- upstreamA$par[1]
    eq_w$w_2A <- upstreamA$par[2]
    
    upstreamB <- optim(
      par = c(eq_w$w_1B, eq_w$w_2B),
      fn =  u_firm_unint,
      w_A = c(eq_w$w_1A, eq_w$w_2A),
      firm_A = 0,
      method = "BFGS",
      control = list(maxit = 10000, reltol = 1E-12)
    )
    
    eq_w$w_1B <- upstreamB$par[1]
    eq_w$w_2B <- upstreamB$par[2]
    
    new_dist <- sqrt(eq_pi$pi_A ^ 2 + eq_pi$pi_B ^ 2)
    
    curr_tol = abs(new_dist - old_dist)/old_dist
    iter = iter + 1
  }
}

if (integrated == 1){
  while (curr_tol > tol & iter < 500) {
    old_dist <- sqrt(eq_pi$pi_A ^ 2 + eq_pi$pi_1 ^ 2 + eq_pi$pi_B ^ 2)
    
    upstreamA <- optim(
      par = c(0, eq_w$w_2A),
      fn =  u_firm_int,
      w_B = c(eq_w$w_1B, eq_w$w_2B),
      firm_A = 1,
      method = "BFGS",
      control = list(maxit = 10000, reltol = 1E-12)
    )
    
    eq_w$w_1A <- upstreamA$par[1]
    eq_w$w_2A <- upstreamA$par[2]
    
    upstreamB <- optim(
      par = c(eq_w$w_1B, eq_w$w_2B),
      fn =  u_firm_int,
      w_A = c(0, eq_w$w_2A),
      firm_A = 0,
      method = "BFGS",
      control = list(maxit = 10000, reltol = 1E-12)
    )
    
    eq_w$w_1B <- upstreamB$par[1]
    eq_w$w_2B <- upstreamB$par[2]
    
    new_dist <- sqrt(eq_pi$pi_A ^ 2 + eq_pi$pi_1 ^ 2 + eq_pi$pi_B ^ 2)
    
    curr_tol = abs(new_dist - old_dist)/old_dist
    iter = iter + 1
  }
}
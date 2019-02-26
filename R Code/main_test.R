#####################################################################
# 1. Preliminaries
#####################################################################

setwd("G:\\My Drive\\Ivan\\College\\2018-2019\\Indep Study\\R Code")

source("upstream_firms_test.R")
source("downstream_firms_cd_test.R")
source("consumer_logit_test.R")

integrated = 0  # is Firm A and Firm 1 vertically integrated
linear = 0  # downstream firm with linear production function
cobb_douglas = 0  # downstream firm with cobb-douglas production function
downstream_iter = 0

M = 1  # market size

set.seed(100) #set a random seed

#####################################################################
# 2. Main
#####################################################################

# equilibrium parameters
x_A <- 0  # upstream firm A output
x_B <- 0  # upstream firm B output
q_1 <- 0  # downstream firm 1 output
q_2 <- 0  # downstream firm 2 output
w_A <- 0  # upstream firm A price
w_B <- 0  # upstream firm B price
p_1 <- 0  # downstream firm 1 price
p_2 <- 0  # downstream firm 2 price
eq <-
  list(
    w_A = w_A,
    w_B = w_B,
    x_A = x_A,
    x_B = x_B
  )

# eq profits
pi_A <- 0  #upstream A
pi_B <- 0  #upstream B
pi_1 <- 0  #downstream 1
pi_2 <- 0  #downstream 2
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
    w_1A = .5,
    w_2A = .5,
    w_1B = .5,
    w_2B = .5,
    x_1A = x_1A,
    x_2A = x_2A,
    x_1B = x_1B,
    x_2B = x_2B
  )

eq_downstream_p <-
  list(
    p_1A = 1,
    p_1B = 1,
    p_2A = 1,
    p_2B = 1
  )

iter = 0
out_tol = 1
optim_A = 0

while (out_tol > 1E-9) {
  iter = iter + 1  # count iterations
  optim_A = 1 - optim_A  # optimizing which upstream firm's offer
  pi_A_old <- eq_pi$pi_A # keeping tabs on old profits
  pi_B_old <- eq_pi$pi_B

  # optimize firm A given firm B prices are fixed
  if (optim_A == 1) {
    firm_A_optim <-
      optim(
        par = c(eq_int_good$w_1A, eq_int_good$w_2A),
        fn = u_firm,
        w_B = c(eq_int_good$w_1B, eq_int_good$w_2B),
        M = M,
        method = "BFGS",
        control = list(maxit = 10000, reltol = 1E-12)
      )
    out_tol = abs(sqrt(eq_pi$pi_A ^ 2 + eq_pi$pi_B ^ 2) - sqrt(pi_A_old ^ 2 + pi_B_old ^
                                                             2))
  } else {
    firm_B_optim <-
      optim(
        par = c(eq_int_good$w_1B, eq_int_good$w_2B),
        fn = u_firm,
        w_A = c(eq_int_good$w_1A, eq_int_good$w_2A),
        M = M,
        method = "BFGS",
        control = list(maxit = 10000, reltol = 1E-12)
      )
    out_tol = abs(sqrt(eq_pi$pi_A^2 + eq_pi$pi_B^2) - sqrt(pi_A_old^2 + pi_B_old^2))
  }
}



# u_firm(c(eq_int_good$w_1A, eq_int_good$w_2A), c(eq_int_good$w_1B, eq_int_good$w_2B), M)


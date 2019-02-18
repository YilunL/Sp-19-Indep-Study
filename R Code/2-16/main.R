#####################################################################
# 1. Preliminaries
#####################################################################

setwd("G:\\My Drive\\Ivan\\College\\2018-2019\\Indep Study\\R Code\\2-16")

source("upstream_firms.R")
source("downstream_firms_cd.R")


k = 0.5 # amount captured if other firm raises price by $1 (cross-price elasticity)

integrated = 0  # is Firm A and Firm 1 vertically integrated
linear = 0  # downstream firm with linear production function
cobb_douglas = 1  # downstream firm with cobb-douglas production function

set.seed(100) #set a random seed

a = 0.7  #set cobb-douglas production params <1
b = 1 - a  #cobb-douglas CRS

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
    x_B = x_B,
    p_1 = p_1,
    p_2 = p_2,
    q_1 = q_1,
    q_2 = q_2
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
    w_1A = .2,
    w_2A = .2,
    w_1B = .4,
    w_2B = .4,
    x_1A = x_1A,
    x_2A = x_2A,
    x_1B = x_1B,
    x_2B = x_2B
  )

#####################################################################
# 2a. Downstream production - complete information
#####################################################################
tol = 1           
optim_A = 0
iter = 0

# set tolerance for difference in L2 of profits
while(tol > 1E-9){                                           
  iter = iter + 1  # count iterations
  optim_A = 1 - optim_A  # optimizing which intermediate firm's offer
  pi_A_old <- eq_pi$pi_A # keeping tabs on old profits
  pi_B_old <- eq_pi$pi_B
  
  # optimize firm A given that firm B's offer is fixed
  if (optim_A == 1){
    optim_result <-
      optim(
        par = c(eq_int_good$w_1A, eq_int_good$w_2A),  # optimizing A's offer
        fn = u_firms_unint,
        w_B = c(eq_int_good$w_1B, eq_int_good$w_2B),  # B's fixed offer
        method = "BFGS",
        control = list(maxit = 10000, reltol = 1E-12)
      )
    tol = abs(sqrt(eq_pi$pi_A^2 + eq_pi$pi_B^2) - sqrt(pi_A_old^2 + pi_B_old^2))  # update tolerance
  } else {
    optim_result <-
      optim(
        par = c(eq_int_good$w_1B, eq_int_good$w_2B),
        fn = u_firms_unint,
        w_A = c(eq_int_good$w_1A, eq_int_good$w_2A),
        method = "BFGS",
        control = list(maxit = 10000, reltol = 1E-12)
      )
    tol = abs(sqrt(eq_pi$pi_A^2 + eq_pi$pi_B^2) - sqrt(pi_A_old^2 + pi_B_old^2))
  }
}

# optim_A = 1
# 
# optim_result <-
#   optim(
#     par = c(0.33, 0.33),
#     fn = u_firms_unint,
#     w_B = c(0.3, 0.3),
#     method = "BFGS",
#     control = list(maxit = 10000, reltol = 1E-12)
#   )
# x_A <- x_1A + x_2A
# x_B <- x_1B + x_2B
# w_A <- (w_1A * x_1A + w_2A * x_2A) / x_A
# w_B <- (w_1B * x_1B + w_2B * x_2B) / x_B


eq
eq_pi
eq_int_good

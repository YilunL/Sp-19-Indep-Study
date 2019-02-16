#####################################################################
# 1. Preliminaries 
#####################################################################

setwd("G:\\My Drive\\Ivan\\College\\2018-2019\\Indep Study\\R Code")

source("upstream_firms_test.R")
source("downstream_firms_cd_test.R")
source("upstream_firms_test.R")

k = 0.5 # amount captured if other firm raises price by $1 (cross-price elasticity)

integrated = 0  # is Firm A and Firm 1 vertically integrated
linear = 0  # downstream firm with linear production function
cobb_douglas = 1  # downstream firm with cobb-douglas production function

set.seed(100) #set a random seed

a = 0.5  #set cobb-douglas production params <1
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

# eq profits
pi_A <- 0  #upstream A
pi_B <- 0  #upstream B
pi_1 <- 0  #downstream 1
pi_2 <- 0  #downstream 2

# firms specific intermediate demads
x_1A <- 0  # downstream firm 1's demand for intermediate good A
x_2A <- 0  # downstream firm 2's demand for intermediate good A
x_1B <- 0  # downstream firm 1's demand for intermediate good B
x_2B <- 0  # downstream firm 2's demand for intermediate good B

#####################################################################
# 2a. Downstream production - complete information
#####################################################################
optim_A = 1

if (cobb_douglas == 1){
  if (integrated == 0){
    # optim_result <- optim(c(0.3,0.3), u_firms_unint, method = "Nelder-Mead", control = list(maxit = 10000))
    u_firms_unint(c(0.3,0.3))
    x_A <- x_1A + x_2A
    x_B <- x_1B + x_2B
    w_A <- (w_1A * x_1A + w_2A * x_2A) / x_A
    w_B <- (w_1B * x_1B + w_2B * x_2B) / x_B
    eq_cd_unint <- c(w_A, w_B, x_A, x_B, p_1, p_2, q_1, q_2) ## TODO: Update weighted average w_A, w_B
  }
  if (integrated == 1){
    u_firms_int()
    x_A <- x_1A + x_2A
    x_B <- x_1B + x_2B
    w_A <- (w_1A * x_1A + w_2A * x_2A) / x_A
    w_B <- (w_1B * x_1B + w_2B * x_2B) / x_B
    eq_cd_int <- c(w_A, w_B, x_A, x_B, p_1, p_2, q_1, q_2) ## TODO: Update weighted average w_A, w_B
  }
}




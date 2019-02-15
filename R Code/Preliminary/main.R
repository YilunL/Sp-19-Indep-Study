#####################################################################
# 1. Preliminaries 
#####################################################################

setwd("G:\\My Drive\\Ivan\\College\\2018-2019\\Indep Study\\R Code")
require(nloptr)

source("upstream_firms.R")
source("downstream_firms_linear.R")
source("downstream_firms_cd.R")

k = 0.333 # amount captured if other firm raises price by $1 (cross-price elasticity)

integrated = 0  # is Firm A and Firm 1 vertically integrated
linear = 0  # downstream firm with linear production function
cobb_douglas = 1  # downstream firm with cobb-douglas production function

set.seed(2) #set a random seed

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
if (linear == 1){
  if (integrated == 0){
    u_firms_unint()
    x_A <- x_1A + x_2A
    x_B <- x_1B + x_2B
    eq_cd_unint <- c(w_A, w_B, x_A, x_B, p_1, p_2, q_1, q_2)
  }
}

if (cobb_douglas == 1){
  if (integrated == 0){
    u_firms_unint()
    x_A <- x_1A + x_2A
    x_B <- x_1B + x_2B
    eq_cd_unint <- c(w_A, w_B, x_A, x_B, p_1, p_2, q_1, q_2)
  }
}




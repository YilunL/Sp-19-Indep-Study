#####################################################################
# 1. Preliminaries
#####################################################################

setwd("G:\\My Drive\\Ivan\\College\\2018-2019\\Indep Study\\R Code")

source("upstream_firms_test.R")
source("downstream_firms_cd_test.R")


k_downstream = 0.2 # amount captured if other firm raises price by $1 (cross-price elasticity)

integrated = 0  # is Firm A and Firm 1 vertically integrated
linear = 0  # downstream firm with linear production function
cobb_douglas = 0  # downstream firm with cobb-douglas production function

set.seed(100) #set a random seed

#####################################################################
# 2. Main
#####################################################################

# upstream firm offer to downstream firm
w_1A <- 0.6
w_2A <- 0.5
w_1B <- 0.6
w_2B <- 0.5
w_A <- c(w_1A, w_2A)
w_B <- c(w_1B, w_2B)

p <- d_firm_price(w_A, w_B)

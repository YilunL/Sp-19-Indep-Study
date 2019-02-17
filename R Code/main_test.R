#####################################################################
# 1. Preliminaries
#####################################################################

setwd("G:\\My Drive\\Ivan\\College\\2018-2019\\Indep Study\\R Code")

source("upstream_firms_test.R")
source("downstream_firms_cd_test.R")


k = 0.9 # amount captured if other firm raises price by $1 (cross-price elasticity)

integrated = 0  # is Firm A and Firm 1 vertically integrated
linear = 0  # downstream firm with linear production function
cobb_douglas = 0  # downstream firm with cobb-douglas production function

set.seed(100) #set a random seed

#####################################################################
# 2. Main
#####################################################################


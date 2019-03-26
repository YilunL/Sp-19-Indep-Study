# Linear production
setwd("G:\\My Drive\\Ivan\\College\\2018-2019\\Indep Study\\R Code")
source("consumer_logit_test.R")

#############################################################################
# Unintegrated downstream firms
#############################################################################

d_firm_prob <- function(p_1, p_2){
  odds_1 <- cons_demand_2(p_1, 0)  # calculate exp(vj) for store 1
  odds_2 <- cons_demand_2(p_2, 1)  # calculate exp(vj) for store 2
  odds <- c(odds_1, odds_2)
  
  return(odds)
}

epsilon <- 1E-11

w_1A <- 0.5
w_1B <- 0.5
w_2A <- 0.5
w_2B <- 0.5

w_A <- c(w_1A, w_2A)
w_B <- c(w_1B, w_2B)

p_1A <- 1
p_1B <- 1
p_2A <- 1
p_2B <- 1

p_1 = c(p_1A,p_1B)
p_2 = c(p_2A,p_2B)

odds <- d_firm_prob(p_1, p_2) #1A, 1B, 2A, 2B

q_1A <- odds[1]/(1 + sum(odds))
q_1B <- odds[2]/(1 + sum(odds))
q_2A <- odds[3]/(1 + sum(odds))
q_2B <- odds[4]/(1 + sum(odds))

p_1_n <- c(p_1A + epsilon, p_1B)

new_odds <- d_firm_prob(p_1_n, p_2)
q_1A_n <- new_odds[1]/(1 + sum(new_odds))
q_1B_n <- new_odds[2]/(1 + sum(new_odds))
q_2A_n <- new_odds[3]/(1 + sum(new_odds))
q_2B_n <- new_odds[4]/(1 + sum(new_odds))
dq_1A_dp_1A <- (q_1A_n - q_1A) / epsilon 

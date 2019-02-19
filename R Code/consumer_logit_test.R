#############################################################################
# Consumer logit demand
#############################################################################

cons_demand <- function(p, x){
  beta = c(1, 1)
  alpha = c(0.5,0.5)
  
  # firm_1_p <- c(p[1], p[2])
  # firm_2_p <- c(p[3], p[4])
  # firm_1_x <- c(x[1], x[2])
  # firm_2_x <- c(x[3], x[4])
  
  # odds_1 <- exp(t(firm_1_x) %*% beta - t(firm_1_p) %*% alpha)
  # odds_2 <- exp(t(firm_2_x) %*% beta - t(firm_2_p) %*% alpha)
  
  # prob_1 <- odds_1 / (1 + odds_1 + odds_2)
  # prob_2 <- odds_2 / (1 + odds_1 + odds_2)
  
  # return(c(prob_1, prob_2))
  
  odds <- exp(t(x) %*% beta - t(p) %*% alpha)
  return(odds)
}


cons_demand(c(.8,.8), c(0.5,0.5))


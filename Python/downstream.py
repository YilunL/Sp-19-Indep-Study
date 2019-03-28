import pandas as pd
import numpy as np
import scipy as sp
from scipy.optimize import minimize

# consumer demand logit

def cons_demand(p, store):
    beta_0 = 2  # intercept
    beta_1 = 2  # price
    beta_2 = 1  # store
    beta_3 = 1  # brand effect
    
    p_A = p[0]
    p_B = p[1]
    
    odds_A = np.exp(beta_0 - p_A * beta_1 + store * beta_2)
    odds_B = np.exp(beta_0 - p_B * beta_1 + store * beta_2 + beta_3)
    
    return(odds_A, odds_B)

def quantities(p_1, p_2):
    odds_1 = cons_demand(p_1, 0)  # store 1
    odds_2 = cons_demand(p_2, 1)  # store 2
    q_1A = odds_1[0]/(1 + np.sum([odds_1, odds_2]))
    q_1B = odds_1[1]/(1 + np.sum([odds_1, odds_2]))
    q_2A = odds_2[0]/(1 + np.sum([odds_1, odds_2]))
    q_2B = odds_2[1]/(1 + np.sum([odds_1, odds_2]))
    return(q_1A, q_1B, q_2A, q_2B)


# calculates downstream partial derivatives given a peturbed vector
def downstream_partial(p_1, p_2, q, epsilon): 
    new_q = quantities(p_1, p_2)
    deriv = np.zeros(len(q))
    for i in range(len(q)):
        deriv[i] = (new_q[i] - q[i])/epsilon
    return(deriv)        


def objective(p, w, q, dq_dp_1A, dq_dp_1B, dq_dp_2A, dq_dp_2B):
    penalty = 0 
    
    # Firm 1 FOC
    dpi_1_dp_1A = (q[0] + (p[0] - w[0]) * dq_dp_1A[0] 
                   + (p[1] - w[1])*dq_dp_1A[1])
    
    dpi_1_dp_1B = (q[1] + (p[1] - w[1]) * dq_dp_1B[1] 
                   + (p[0] - w[0])*dq_dp_1B[0])
                   
    dpi_2_dp_2A = (q[2] + (p[2] - w[2]) * dq_dp_2A[2] 
                   + (p[3] - w[3])*dq_dp_2A[3])
    
    dpi_2_dp_2B = (q[3] + (p[3] - w[3]) * dq_dp_2B[3] 
                   + (p[2] - w[2])*dq_dp_2B[2])
    
    penalty = np.sqrt(
        np.sum(dpi_1_dp_1A**2 + dpi_1_dp_1B**2 
               + dpi_2_dp_2A**2 + dpi_2_dp_2B**2))
    
    return(penalty)



def main(w): 
    epsilon = 1E-10

    p_1A = 1  # prices
    p_1B = 1
    p_2A = 1
    p_2B = 1

    p = np.array([p_1A, p_1B, p_2A, p_2B])
    p_1 = np.array([p_1A, p_1B])  #as array
    p_2 = np.array([p_2A, p_2B])

    q = quantities(p_1, p_2)  #q_1A, q_1B, q_2A, q_2B

    # perturbed prices
    dp_1A = np.array([p_1A + epsilon, p_1B])
    dp_1B = np.array([p_1A, p_1B + epsilon])
    dp_2A = np.array([p_2A + epsilon, p_2B])
    dp_2B = np.array([p_2A, p_2B + epsilon])

    # calculate partials:
    dq_dp_1A = downstream_partial(dp_1A, p_2, q, epsilon) #dq_1A, dq_1B, dq_2A, dq_2B
    dq_dp_1B = downstream_partial(dp_1B, p_2, q, epsilon)
    dq_dp_2A = downstream_partial(p_1, dp_2A, q, epsilon)
    dq_dp_2B = downstream_partial(p_1, dp_2B, q, epsilon)

    res = minimize(objective, p, method = "Nelder-Mead", tol = 1E-9,
                args = (w,q, dq_dp_1A, dq_dp_1B, dq_dp_2A, dq_dp_2B),
                options = {'disp': True})

    return(res.x)

main(np.array([0.5,0.5,0.5,0.5]))


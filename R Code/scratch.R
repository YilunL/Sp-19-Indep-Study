a = 0.5
b = 1-a
w_A <- eq$w_A
w_B <- eq$w_B

a/w_A * (2*(1 + 2*sqrt(w_A * w_B))/1.5 - ((1 + 2*sqrt(w_A * w_B))/1.5)^2)
b/w_B * (2*(1 + 2*sqrt(w_A * w_B))/1.5 - ((1 + 2*sqrt(w_A * w_B))/1.5)^2)

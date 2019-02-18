library(tidyverse)
library(haven)
library(plm)

setwd('G:/My Drive/Ivan/College/2018-2019/ECON 712/PS2')

df <- read_dta("GMdata.dta") 

df <- df %>%
  mutate(d357 = if_else(sic3 == 357, 1, 0)) 

df_bal <- df %>%
  group_by(index) %>%
  filter(n() == 4)


# 1a
df1a <- pdata.frame(df_bal, index = c("index", "year"))
  
  # df_bal %>%
  # group_by(index) %>%
  # mutate_at(vars(ldsal:ldinv), funs(.-lag(.))) # take first difference

formula1a <- ldsal ~ lemp + ldnpt + ldrst | 
  lag(lemp, 2:99) + lag(ldnpt, 2:99) + lag(ldrst, 2:99)

res1a <- pgmm(formula1a, 
              data = df1a,
              effect = "individual")

res1a

# reg1a <-
#   pgmm(
#     dynformula(form),
#     data = df1a,
#     index = c("index", "yr"),
#     effect = "twoways",
#     model = "twosteps",
#     gmm.inst = ~ lemp + ldnpt + ldrst,
#     lag.gmm = list(c(2, 99))
#   )
# 
# summary(reg1a)
#         
# reg2a <- pgmm(
#   ldsal ~ lemp + ldnpt + ldrst |
#     lag(lemp, 2:99) + lag(ldnpt, 2:99) + lag(ldrst, 2:99),
#   data = df1a,
#   index = c("index", "yr"),
#   effect = "twoways",
#   model = "twosteps"
# )
# reg2a

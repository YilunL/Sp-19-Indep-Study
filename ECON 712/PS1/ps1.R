library(tidyverse)
library(haven)
library(plm)
# detach("package:tidyverse", unload=TRUE)


setwd('G:/My Drive/Ivan/College/2018-2019/ECON 712/PS1')


df <- read_dta("GMdata.dta") 

summary(df)

df_4 <- df %>%
  group_by(index) %>%
  filter(n() == 4)

summary(df_4)

df_2 <- df %>%
  group_by(index) %>%
  filter(n() >= 2)

########################### Problem 2 ###########################

total <- ldsal ~ lemp + ldnpt + ldrst + d357

df_4 <- df_4 %>%
  mutate(d357 = if_else(sic3 == 357, 1, 0)) 

pool <- plm(total, data = df_4, model = "pooling", index = 'index')
bw <- plm(total, data = df_4, model = "between", index = 'index')
within <- plm(total, data = df_4, model = "within", index = 'index')
random <- plm(total, data = df_4, model = "random", index = 'index')  

phtest(within, random)

######################## Problem 3 ###########################
fd <- plm(total, data = df_4, model = "fd", index = c('index'))  
fd

# five <- df_4 %>%
#   group_by(index) %>%
#   mutate(lagldsal = dplyr::lag(ldsal, n = 1, default = NA), laglemp = dplyr::lag(lemp, n = 1, default = NA), lagldnpt = dplyr::lag(ldnpt, n = 1, default = NA), lagldrst = dplyr::lag(ldrst, n = 1, default = NA)) %>%
#   mutate(ldsal = ldsal - lagldsal, lemp = lemp - laglemp, ldnpt = ldnpt - lagldnpt, ldrst = ldrst - lagldrst)
# 
# plm(ldsal ~ lemp + ldnpt + ldrst + 0, data = five, model = "pooling", index = 'index')  

ten <- df_4 %>%
  group_by(index) %>%
  mutate(lagldsal = dplyr::lag(ldsal, n = 2, default = NA), laglemp = dplyr::lag(lemp, n = 2, default = NA), lagldnpt = dplyr::lag(ldnpt, n = 2, default = NA), lagldrst = dplyr::lag(ldrst, n = 2, default = NA)) %>%
  mutate(ldsal = ldsal - lagldsal, lemp = lemp - laglemp, ldnpt = ldnpt - lagldnpt, ldrst = ldrst - lagldrst)

plm(ldsal ~ lemp + ldnpt + ldrst + 0, data = ten, model = "pooling", index = 'index')  

fifteen <- df_4 %>%
  filter(yr %in% c(73,88))

plm(total, data = fifteen, model = "fd", index = 'index')  
plm(total, data = fifteen, model = "within", index = 'index')  

######################### Problem 4 ########################
df <- df %>%
  mutate(d357 = if_else(sic3 == 357, 1, 0)) 

plm(total, data = df, model = "pooling", index = "index")
plm(total, data = df, model = "fd", index = "index")

df_2 <- df_2 %>%
  mutate(d357 = if_else(sic3 == 357, 1, 0)) 

plm(total, data = df_2, model = "pooling", index = "index")
plm(total, data = df_2, model = "fd", index = "index")




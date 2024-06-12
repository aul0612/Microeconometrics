#######################
## Panel Data
#######################


# install.packages("plm")

library(plm)

# A balanced panel of 10 observational units (firms) from 1935 to 1954

data("Grunfeld", package = "plm")

# Pooled OLS
pooled_ols_lm_1 <- lm(inv ~ capital, data = Grunfeld)
summary(pooled_ols_lm_1)

pooled_ols_lm_2 <- plm(inv ~ capital, data = Grunfeld,
                       index = c("firm", "year"), model = "pooling")
summary(pooled_ols_lm_2)

# Random Effects Estimator
re_ols_lm <- plm(inv ~ capital, data = Grunfeld,
                 index = c("firm", "year"), model = "random")
summary(re_ols_lm)

# Fixed Effects Estimator

# Firm Fixed Effects
fe_ols_lm_firm <- plm(inv ~ capital, data = Grunfeld,
                      index = c("firm", "year"),
                      effect = "individual", model = "within")
summary(fe_ols_lm_firm)

# Time Fixed Effects
fe_ols_lm_time <- plm(inv ~ capital, data = Grunfeld,
                      index = c("firm", "year"),
                      effect = "time", model = "within")
summary(fe_ols_lm_time)

# Two way Fixed Effects
fe_ols_lm_2way <- plm(inv ~ capital, data = Grunfeld,
                      index = c("firm", "year"),
                      effect = "twoways", model = "within")
summary(fe_ols_lm_2way)


# First-Difference Estimator
fd_ols_lm_firm <- plm(inv ~ capital, data = Grunfeld,
                      index = c("firm", "year"),
                      effect = "individual",
                      model = "fd")
summary(fd_ols_lm_firm)
## different and less efficient -> much higher standard error -> note, T != 2

# Check whether FE is equivalent to FD when T = 2

#FD
affe_als_afd_check <- plm(inv ~ capital - 1, data = Grunfeld,
                          index = c("firm", "year"),
                          effect = "individual", model = "fd")
summary(affe_als_afd_check)

#FE
afd_als_affe_check <- plm(inv ~ capital - 1, data = Grunfeld,
                          subset = year %in% c(1935, 1936),
                          index = c("firm", "year"),
                          effect = "individual", model = "within")
summary(affe_als_afd_check)

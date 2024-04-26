####### Here seem to exist issues!

### Consider the Setup of heterogeneous TE as in Ex_22_04.r
### but change W s.t. it only has discrete values
n <- 1000
sample_space <- c(-2,-1,1,2)

### Heterogenous TE
set.seed(123)
W <- sample(sample_space, n, replace = TRUE) #discrete W
#W <- rnorm(n)
D <- as.numeric(W <= rnorm(n, 0, 2.5))
# D <- as.numeric(W <= rnorm(n, 0, 2)) # discrete W
# Here D is a NOT deterministic function of W
U <- rnorm(n, 0, .7)
delta <- runif(n)

# Generate Potential Outcomes
Y_1 <- delta + W + U
Y_0 <- W + U

# Generate observable Outcomes
Y  <- D*Y_1 + (1-D)*Y_0

data <- data.frame(Y, D = as.factor(D), W)

#Does the linear regression model also work in this case?
lm_obj <- lm(Y ~ D + W)
# summary(lm_obj)
slope_coeff <- lm_obj$coefficients[2]
slope_coeff     
# here this is not a good estimate of the true ATE which is 0.5

#However consider Estimation of mu_d with d=0,1 where 
# mu_1 = E[Y|D=1,W], mu_0 = E[Y|D=0,W]
# Then estimated ATE is the average difference between these two.
# use sample means as estimators:
weights <- numeric(length(sample_space))
del_mus <- numeric(length(sample_space))
for(i in 1:length(sample_space)){
    w <- sample_space[i]
    mu_0 <- mean(Y[D==0 & W==w])
    mu_1 <- mean(Y[D==1 & W==w])
    weights[i] <- sum(W==w)/length(W)
    del_mus[i] <- mu_1 - mu_0
}
ATE_est <- 0
for (i in 1:length(sample_space)){
    ATE_est <- ATE_est + weights[i]*del_mus[i]
}
ATE_est
# this estimator is even worse! at least with n=1_000
# with n=100_000 it gets much better!
# this might be due to repeated use of the sample mean
# as an estimator of an expected value, hence larger n makes it much better!



## Now try to estimate a propensity score for the same data
# note: p(W=w) = P(D=1|W=w) = E[D|W=w]
# hence, to obtain a propensity score, we regress D on W
lm_prop <- lm(D~W)

#Now we estimate us following formula
# E[TE] = E[Y ( D/(p(W)) - (1-d)/(1-p(W)) )]
# through the corresponding sample mean
sum_prop <- 0
for (i in 1:length(W)){
    sum_prop <- sum_prop + Y[i] *
                    (D[i]/lm_prop$fitted.values[i] - (1-D[i])/(1-lm_prop$fitted.values[i]))
}
ATE_prop <- 1/n * sum_prop
ATE_prop

# doesn't work too well for the discrete case




############################# here seem to exist some issues!

#### with discrete values, one can simply estimate 
#### ATE given wi since within each subgroup treatment is effectively assigned randomly
#### then using a weighted average gives the average TE over all Wi

weights <- numeric(length(sample_space))
coeffs <- numeric(length(sample_space))
treatment_freq <- numeric(length(sample_space))
mat <- cbind(weights, coeffs)
data <- as.data.frame(cbind(Y,D,W))
for (i in 1:length(sample_space)){
    w <- sample_space[i]
    sub <- subset(data, data$W==w)
    head(sub)
    lm_obj <- lm(Y~D, sub)
    coeffs[i] <- lm_obj$coefficients[2]
    weights[i] <- nrow(sub)/nrow(data)
    treatment_freq[i] <- sum(sub$D)/nrow(sub)
}
coeffs
weighted.mean(coeffs, weights)
treatment_freq
### with smaller sample size, this is overestimating the effect for large/small W.
### This coincides with a high or low probability of treatment
### i.e. either only few units with treatment or few units without treatment exist with this w_i
### making the estimation worse?

### Most notably, this way of estimation coincides with the above used one to estimate it via conditional means
### as estimators of conditional expected values.

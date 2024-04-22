n <- 1000 # sample size

################
### Estimation of ATE under Randomn Assingnment
################

set.seed(123)
W <- rnorm(n)
D <- rbinom(n, 1, .5) # this implies random assignment, since treatment is assigned randomly
U <- rnorm(n, 0, .7)


# Generate Potential Outcomes based on (D, W, U)
Y_1 <- 1 + W + U
Y_0 <- W + U

## Y_i(1) - Y_i(0) = 1 for all i --> ATE=1

# Generate observed Outcomes
Y <- D*Y_1 + (1-D)*Y_0

# recall: If treatment effect is constant, OLS regression parameter of D is estimator of ATE

lm_obj <- lm(Y~ D + W)
summary(lm_obj)
slope_coeff <- lm_obj$coefficients[2]
slope_coeff     # this is very close to ATE=1, this is in line with slide 32,
                # since under homogeneous TE, OLS estimates the ATE.

### Heterogeneous TE

set.seed(123)
W <- rnorm(n)
D <- rbinom(n, 1, .5) # this implies random assignment, since treatment is assigned randomly
U <- rnorm(n, 0, .7)
delta <- runif(n)

# Generate Potential Outcomes
Y_1 <- delta + W + U
Y_0 <- W + U

# Generate observable Outcomes
Y  <- D*Y_1 + (1-D)*Y_0

## here, the ATE is 0.5, as E(delta) = 0.5, it is also "estimated" by
mean(delta)

# Of course, in practice we cannot do this, since only (Y,D,W) are observed.

# Can we use the linear estimator?
lm_obj <- lm(Y~ D + W)
summary(lm_obj)
slope_coeff <- lm_obj$coefficients[2]
slope_coeff     # estimated coefficient is not close to the ATE, as one knows from slide 32

# Alternatively, assume there are no control variables W. In this case, we know
# from the lecture that the OLS estimates the ATE.

### Heterogeneous TE no controls
set.seed(123)
D <- rbinom(n, 1, .5) # this implies random assignment, since treatment is assigned randomly
U <- rnorm(n, 0, .7)
delta <- runif(n)

# Generate Potential Outcomes
Y_1 <- delta + U
Y_0 <- U

# Generate observed Outcomes
Y  <- D*Y_1 + (1-D)*Y_0

# Estimate ATE
lm_obj <- lm(Y~ D)
# summary(lm_obj)
slope_coeff <- lm_obj$coefficients[2]
slope_coeff



################
### Estimation of ATE under Unconfoundedness
################

### Homogenous TE

#1. Treatment is a deterministic function of W
set.seed(123)
W <- rnorm(n) # treatment is a deterministic function of W
D <- as.numeric(W<=0)
U <- rnorm(n, 0, 0.7)

# Generate Potential Outcomes
Y_1 <- 1 + W + U
Y_0 <- W + U

# Generate observable Outcomes
Y <- D*Y_1 + (1-D)*Y_0

# linear estimator
lm_obj <- lm(Y ~ D + W)
# summary(lm_obj)
slope_coeff <- lm_obj$coefficients[2]
slope_coeff

# Here the estimated coefficient on D is very close to the ATE(which is the homogenous TE)
# This is in line with the Notes on homogenous TE (slide 32)

#2. Treatment effect is NOT a deterministic function of W

set.seed(123)
W <- rnorm(n)
D <- as.numeric(W <= rnorm(n, 0, .25))
# Here D is a NOT deterministic function of W
U <- rnorm(n, 0, .7)


# Generate Potential Outcomes
Y_1 <- 1 + W + U
Y_0 <- W + U

# Generate observable Outcomes
Y <- D*Y_1 + (1-D)*Y_0

# linear estimator
lm_obj <- lm(Y ~ D + W)
# summary(lm_obj)
slope_coeff <- lm_obj$coefficients[2]
slope_coeff

### Heterogenous TE
set.seed(123)
W <- rnorm(n)
D <- as.numeric(W <= rnorm(n, 0, .25))
# Here D is a NOT deterministic function of W
U <- rnorm(n, 0, .7)
delta <- runif(n)

# Generate Potential Outcomes
Y_1 <- delta + W + U
Y_0 <- W + U

# Generate observable Outcomes
Y  <- D*Y_1 + (1-D)*Y_0

#Does the linear regression model also work in this case?
lm_obj <- lm(Y ~ D + W)
# summary(lm_obj)
slope_coeff <- lm_obj$coefficients[2]
slope_coeff     # this estimates beta as on slide 39, where the weights omega is close to 1
# This might look surprising. THe estimated slope is close to the true ATE.
# which is 0.5. When we compare this to Angrist (1998), we know that the coefficient on D
# identifies the average of a weighted version of CATE. Apparently, in our example, the weights are
# close to one and hence do not lead to different estimation results.



################
### IV Methods
################
library(MASS)
library(AER)

set.seed(123)
help_mat <- mvrnorm(n,
                    c(0,0,0), 
                    matrix(c(1, .5, .3, .5, 1, 0, .3, 0, 1),
                            nrow = 3, ncol = 3))
X <- as.numeric(help_mat[,1]<=0)
Z <- as.numeric(help_mat[,2]<=0)
U <- help_mat[,3]
D <- as.numeric(X<= rnorm(n,0,.25))
delta <- runif(n)


# Generate Potential Outcomes
Y_1 <- 1 + U
Y_0 <- U

# Generate observed Outcomes
Y  <- D*Y_1 + (1-D)*Y_0

#
ivreg(Y~X |Z)

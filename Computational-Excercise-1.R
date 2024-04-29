n <- 1000 #sample size 


#######################################################
### Estimation of ATE under Random Assignment #########
#######################################################

set.seed(123)
W <-  rnorm(n) 
D <-  rbinom(n, 1, .5)  #Generate treatment such that P(D=1)=P(D=0)=0.5
U <- rnorm(n,0,.7) #Generate Unobservables

#Generate potential outcomes based on (D,W,U)
Y_1 <- 1 + W + U #potential outcomes for being treated
Y_0 <- W + U #potential outcomes for not being trated

#Generate observable Outcomes
Y <- D*Y_1 + (1-D)*Y_0

#Recall that only (Y,D,W) are observed

lm(Y ~ D + W)

#Here, the estimate of the slope coefficient is 1.04218. This is very close to the (A)TE
#which is equal to one. This is in line slide p. 32, since under homogenous TE, OLS estimates
#the ATE.



### Heterogenous Treatment Effects ####

set.seed(123)
W <-  rnorm(n) 
D <-  rbinom(n, 1, .5)  #Generate treatment such that P(D=1)=P(D=0)=0.5
U <- rnorm(n,0,.7) #Generate Unobservables
Delta <- runif(n) #Generate heterogenous treatment effects

#Generate potential outcomes based on (D,W,U)
Y_1 <- Delta + W + U #potential outcomes for being treated
Y_0 <- W + U #potential outcomes for not being trated

#Generate observable Outcomes
Y <- D*Y_1 + (1-D)*Y_0

#The ATE is given by 0.5. It can also be "estimated" by
mean(Delta)
#Of course, in practice we cannot do this. Since only (Y,D,W) are observed!

#Can we use the linear estimator?

lm(Y ~ D + W)

#This is an interesting special case since Y_1 and Y_0 depend on W but the treatment effect Delta does not depend on W
#Thus we may follow Slide 32 to conclude that OLS estimates the ATE



#Alternatively, assume there are no control variables W. In this case, we know from the lecture
#that the OLS estimates the ATE


### Heterogenous Treatment Effects no controls####
set.seed(123)
D <-  rbinom(n, 1, .5)  #Generate treatment such that P(D=1)=P(D=0)=0.5
U <- rnorm(n,0,.7) #Generate Unobservables
Delta <- runif(n) #Generate heterogenous treatment effects

#Generate potential outcomes based on (D,U)
Y_1 <- Delta +  U #potential outcomes for being treated
Y_0 <- U #potential outcomes for not being trated

#Generate observable Outcomes
Y <- D*Y_1 + (1-D)*Y_0

#Can we use the linear estimator?
lm(Y ~ D)

#Here, the estimated slope coefficient on D is now again very close to the true ATE




##########################################################
### Estimation under Unconfoundedness     ################
##########################################################

### Homogenous Treatment Effects #########

#1. Treatment is a deterministic function of W
set.seed(123)
W <-  rnorm(n) 
D <-  as.numeric(W <= 0)  #Generate treatment such that P(D=1)=P(D=0)=0.5
#Treatment is a dterministic function of W
U <- rnorm(n,0,.7) #Generate Unobservables

#Generate potential outcomes based on (D,W,U)
Y_1 <- 1 + W + U  #potential outcomes for being treated
Y_0 <- W + U      #potential outcomes for not being treated

#Generate observable Outcomes
Y <- D*Y_1 + (1-D)*Y_0


#Again, we run a linear regression:
lm(Y ~ D + W)

#Here the estimated coefficient on D is very close to the ATE (which is the homogenous TE)
# which is one. This is in line with the lecture notes under homogenous TE

#2. Treatment is NOT a deterministic function of W
set.seed(123)
W <-  rnorm(n) 
D <-  as.numeric(W <= rnorm(n,0,.25))  #Treatment is NOT a deterministic function of W
U <- rnorm(n,0,.7) #Generate Unobservables

#Generate potential outcomes based on (D,W,U)
Y_1 <- 1 + W + U  #potential outcomes for being treated
Y_0 <- W + U      #potential outcomes for not being treated

#Generate observable Outcomes
Y <- D*Y_1 + (1-D)*Y_0


#Again, we run a linear regression:
lm(Y ~ D + W)

#Here the estimated coefficient on D is very close to the ATE (which is the homogenous TE)
# which is one. This is in line with the lecture notes under homogenous TE


### Heterogenous Treatment Effects

set.seed(123)
W <-  rnorm(n) 
D <-  as.numeric(W <= rnorm(n,0,.25))  #Treatment is NOT a deterministic function of W
U <- rnorm(n,0,.7) #Generate Unobservables
Delta <- runif(n) #Generate heterogenous treatment effects

#Generate potential outcomes based on (D,W,U)
Y_1 <- Delta + W + U  #potential outcomes for being treated
Y_0 <- W + U      #potential outcomes for not being treated



#Generate observable Outcomes
Y <- D*Y_1 + (1-D)*Y_0

#Does the linear regression model also work in this case?

lm(Y ~ D + W)

#This might look surprising. The estimated slope coefficient is close to the true ATE, 
#which is 0.5. When we compare this to Angrist (1998), we know that the coefficient on D
# identified the average of a weighted version of CATE. Apparently, in our example,
# the weights are close to one and hence do not lead to different estimation results.



##########################################################
### IV methods    ################
##########################################################
library(MASS)


set.seed(123)
help.mat <- mvrnorm(n, c(0,0,0), matrix(c(1,.5,.3,.5,1,0,.3,0,1),3,3))
D <-  as.numeric(help.mat[,1]<=0 ) #treatment
Z <-  as.numeric(help.mat[,2]<=0 )
U <-  help.mat[,3]

#This generates IV which are correlated to Z but approx. uncorrelated to U


#Generate potential outcomes based on (D,W,U) with homogenous treatment effects
Y_1 <- 1 + U  #potential outcomes for being treated
Y_0 <-  U      #potential outcomes for not being treated


#Generate observable Outcomes
Y <- D*Y_1 + (1-D)*Y_0

library(AER)

ivreg( Y ~ D | Z)

#With homogneous treatment effects, the IV estimator is consistent for the ATE as we will see in the lecture in Section 2
#As you run the code for different starting values, you will see more variation in the estimation step
#This is common for IV estimation which typically leads to larger variances

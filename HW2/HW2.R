## ST537 Homework II: Due date - 02/04/2020
# loading in libraries: 
library(tidyverse)
library(knitr)

## Question 1: reading in the data sets
turtle_data <- as.data.frame(read.table("C:/Users/xilia/OneDrive/Desktop/2020 - Spring/ST537/Datasets/T6-9.dat"))
colnames(turtle_data) <- c("Length", "Width", "Height", "Gender")

# Filter to only include males
m_data <- filter(turtle_data, Gender == "male") 

# Filter the male data to only include values
n_data <- select(m_data, -Gender)

# Point estimate for mean (Length, Width, and Height) for male turtles
mean.est <- colMeans(n_data)
# Point estimates are:
# Length = 113.40, # Width = 88.30, # Height = 40.70

# 95% Simultaneous Confidence intervals for mean length, width, and height for male turtles. 
simul.CI <- function(data, level = 0.95) {
  xbar <- colMeans(data)
  # Sample covariance S
  S <- cov(data)
  # sample size (n) and number of variables (p)
  n <- nrow(data)
  p <- ncol(data)
  # Critical value
  crit <- (n - 1) * p/((n - p) * n) * qf(1 -level, p, n - p, lower.tail = FALSE)
  # half-width
  H <- sqrt(crit * diag(S))
  # Intervals with point estimates
  out <- data.frame(Estimate = xbar, Lower = xbar -
                      H, Upper = xbar + H)
  return(out)
}

# Outputting the simultaneous confidence intervals
smCI.m <- simul.CI(n_data)


# B). Same as part a, but for females. 

# Filter to only include females
f_data <- filter(turtle_data, Gender == "female") 

# Filter the male data to only include values
n_data2 <- select(f_data, -Gender)

# Point estimate for mean (Length, Width, and Height) for female turtles
mean.est2 <- colMeans(n_data2)

smCI.f <- simul.CI(n_data2) # Confidence intervals 

# C). Compare results in part a and b and discuss findings. 
# Based on the findings, there is high plausibility to conclude there is a difference 
# in the measurements of carapaces for males and females. Females have a higher overall point estimate
# for the mean. 


## Problem 2 
# reading in data
college_data <- as.data.frame(read.table("C:/Users/xilia/OneDrive/Desktop/2020 - Spring/ST537/Datasets/T5-2.dat"))
colnames(college_data) <- c("SSH", "Verbal", "Science")

# Part A). Creating QQ-plots and pairwise scatterplots
par(mfrow=c(2,2))
qqnorm(college_data[,1])
qqnorm(college_data[,2])
qqnorm(college_data[,3])
dev.off()
plot_scatter <- pairs(college_data, main = "Scatterplots for pairs of variables")

# the qq-plots look to have a straight, upward line. For the scatter plot, there are no unusual trends or shapes.
# These features allow us to justify normality assumption. 



# Part B). Denote mean vector by mu. Test H0: mu = [500,50,30]^T. Provide your conclusion. 
# Mean vector mu equals:

# number of variables and sample size
p1 = ncol(college_data)
n1 = nrow(college_data)
# sample mean vector and sample covariance
# matrix
xbar1 = colMeans(college_data)
S1 = cov(college_data)
# value of mean under null
mu_0 = c(500, 50, 30)
# significance level
alpha = 0.05
# Test statistic and F critical value
T2 <- n1 * (n1 - p1)/(p1 * (n1 - 1)) * t(xbar1 - mu_0) %*% solve(S1) %*% (xbar1 - mu_0)
f.crit <- qf(alpha, p1, (n1 - p1), lower.tail = FALSE)
data.frame(T2, f.crit)

# Since the T-statistic is greater than the critical value, we reject H_0 and conclude that 
# the mu vector is not equal to [500, 50, 30]^T

# Part C). Create 95% Bonferroni confidence intervals for elements of mu
bon.int0 <- cbind(xbar1 - qt(alpha/(2*p1), n1-1)*sqrt(diag(S1)/n1), xbar1 + qt(alpha/(2*p1), n1-1)*sqrt(diag(S1)/n1))
bon.int <- cbind(xbar1, bon.int0)
colnames(bon.int) <- c("Estimate", "Upper", "Lower")
bon.int


# Problem 3: Contrasts
dog_data <- as.matrix(read.table("C:/Users/xilia/OneDrive/Desktop/2020 - Spring/ST537/Datasets/T6-2.dat"))

# Part A). Write another contrast matrix different from the ones given. 
new_mat <- rbind(c(0, -1, 0, 1), c(1,0,0,-1), c(1,0,-1,0))

# Part B). From contrast matrix in part A, test H_0 and compare results to lecture notes. Explain similarities, differences. 
T2.contrast <- function(data.matrix, contrast.matrix,
                        alpha = 0.05) {
  # Input args are (a) ’data.matrix’: n x p
  # matrix, each row is one subject, each col is
  # one treatment. (b) ’contrast.matrix’: q x p
  # matrix C, each row is one contrast. (c)
  # ’alpha’: significance level, default 0.05
  Xmat <- data.matrix
  C <- contrast.matrix
  # parameters
  n <- nrow(Xmat)
  q <- nrow(C)
  # sample mean vector
  xbar <- colMeans(Xmat)
  # sample covariance matrix
  S <- cov(Xmat)
  # Intermediate quantities
  invCSC <- solve(C %*% S %*% (t(C)))
  Cxbar <- C %*% xbar
  # test statistic
  T2 <- n * (n - q)/((n - 1) * q) * (t(Cxbar)) %*%
    invCSC %*% (Cxbar)
  # critical value
  critical_F = qf(alpha, df1 = q, df2 = n -
                    q, lower.tail = F)
  # p-value
  pv <- pf(T2, df1 = q, df2 = n - q, lower.tail = F)
  # display the results
  results <- data.frame(T2 = T2, Fcritical = critical_F,
                        df1 = q, df2 = n - q, pvalue = pv)
  return(results)
}

new_contrast <- T2.contrast(dog_data, new_mat)

# Part C). Separately test for interaction effect, main effect of halothane, main effect of CO2, and interpret results. 
# Test: Interaction effect:
C <- matrix(c(1, -1, -1, 1), nrow = 1)
T2.contrast(dog_data, C)

# Test: Main effect for halothane
halo_c <- matrix(c(1,1,-1,-1), nrow = 1)
T2.contrast(dog_data, halo_c)

# Test: Main effect for CO2
co2_c <- matrix(c(1, -1, 1, -1), nrow = 1)
T2.contrast(dog_data, co2_c)

# There is no interaction effect, but the main effects are significant, and contribute to the significance. 
# Part D). H_0: mu3 - mu4 = 2(mu1 - mu2)
# Contrast Matrix:
cont.matrix1 <- matrix(c(-2,2,1,-1), nrow = 1)
cont.matrix2 <- matrix(c(2,-2,-1,1), nrow = 1)
T2.contrast(dog_data, cont.matrix1)

# Part E). Test hypothesis in (d) and interpret the results. 
# THere is plausibility to assume that the mu3-mu4 = 2(mu1-mu2)


## Problem 4: More Contrasts. 
react_data <- as.matrix(read.table("C:/Users/xilia/OneDrive/Desktop/2020 - Spring/ST537/Datasets/T6-8.dat"))
colnames(react_data) <- c("trt 1", "trt 2", "trt 3", "trt 4")

#A). Test whether there is an overall treatment effect on reaction time. 
library(ICSNP)
D_diff <- react_data[,1] - react_data[,3]
D_same <- react_data[,2] - react_data[,4]

dmat <- cbind(D_diff, D_same)
HotellingsT2(dmat) 
# Looks like there is an overall treatment effect on reaction time. 

#B). Construct 95% Simultaneous CIs representing interaction effect between format and parity, 
# Main effect of parity, and main effect of format. Interpret the results. 

C_matrix <- rbind(c(1,-1,-1,1), c(1,-1,1,-1), c(1,1,-1,-1)) # C matrix 
rownames(C_matrix) <- c("Interaction", "Parity","format")
p2 <- ncol(C_matrix) # number of variables
q2 <- nrow(C_matrix) # number of contrasts
n2 <- nrow(react_data) # sample size

xbar2 <- colMeans(react_data)
S2 <- cov(react_data)
estimate <- C_matrix %*% xbar2
M <- C_matrix %*% S2 %*% t(C_matrix)
critical_F = sqrt(((n2 - 1) * q2/(n2 - q2)) * qf(p = 0.05, df1 = q2, df2 = n2 - q2, lower.tail = F))

sdvec <- sqrt(diag(M)/n2)

lower <- estimate - critical_F * sdvec
upper <- estimate + critical_F * sdvec
result <- data.frame(estimate, lower, upper)
round(result, 2)

#C). create interaction plot and test for interaction effect. 
Y <- c(react_data)

# Define the treatment combinations
format <- c(rep("word", 32), rep("word", 32), rep("arabic", 32), rep("arabic", 32))
parity <- c(rep("different", 32), rep("same", 32), rep("different", 32), rep("same", 32))
head(data.frame(Y, format, parity))

# Interaction Plot
interaction.plot(format, parity, Y, lwd = 2, col = c(1, 2), cex.axis = 1.3, cex.lab = 1.3, cex.main = 2,
                 type = "b", main = "Interaction Plot", pch = 1:2)

# Testing for interaction effect:
C_4int <- matrix(c(1,-1,-1,1), nrow = 1)
T2.contrast(react_data, C_4int)
# There is no significant interaction effect. 

#D). Test contrast corresponding to interaction effect, but want to use one-sample t-test only. 
# Can this be done? If so, explain the procedure, perform the test, and compare with result (c). 
Dmat <- react_data[,1] - react_data[,3] - react_data[,2] + react_data[,4]
# Dmat <- cbind(D_parity, D_format)
t.test(Dmat)

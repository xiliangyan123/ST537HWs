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

pairs(college_data)

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
bon.int <- c(xbar1 - qt(alpha/(2*p1), n1-1)*sqrt(diag(S1)/n1), xbar1 + qt(alpha/(2*p1), n1-1)*sqrt(diag(S1)/n1))




# Problem 3: Contrasts


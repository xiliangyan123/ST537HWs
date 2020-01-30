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

college_data <- as.data.frame(read.table("C:/Users/xilia/OneDrive/Desktop/2020 - Spring/ST537/Datasets/T5-2.dat"))
colnames(college_data) <- c("SSH", "Verbal", "Science")


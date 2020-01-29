## ST537 Homework II: Due date - 02/04/2020
## Question 1: reading in the data sets

dat1 <- as.matrix(read.table("C:/Users/xilia/OneDrive/Desktop/2020 - Spring/ST537/Datasets/T5-2.dat"))
dat2 <- as.matrix(read.table("C:/Users/xilia/OneDrive/Desktop/2020 - Spring/ST537/Datasets/T6-2.dat"))
dat3 <- as.matrix(read.table("C:/Users/xilia/OneDrive/Desktop/2020 - Spring/ST537/Datasets/T6-8.dat"))
dat4 <- as.matrix(read.table("C:/Users/xilia/OneDrive/Desktop/2020 - Spring/ST537/Datasets/T6-9.dat"))

colnames(dat3) <- c("trt 1", "trt 2", "trt 3", "trt 4")
head(dat3)

# Denote the mean response of the 4 treatmets as mu_1, ..., mu_4

# A). Test whether there is an overall treatment effect on reaction time


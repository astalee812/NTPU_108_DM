# 1
n <- 500
p <- 10
set.seed(123456)
library(MASS)
s <- matrix(rt(p*p, df=5), ncol = p)
sigma <- crossprod(s)
x <- mvrnorm(n, mu=rep(0, p), Sigma=sigma)
missing.percentage <- 0.1
x[sample(n*p, floor(n*p*missing.percentage))] <- NA

# (1-a)
x.comleted <- x
dim(x)
nc <- dim(x)[1]
pc <- dim(x)[2]

# (1-b)
set.seed(54321)
x.comleted.na <- x
head(x.comleted.na)

# (1-c) mean substitution
mean.subset <- function(x){
  x[is.na(x)] <- mean(x,na.rm = T)
  x
}

x.completed.mean <- apply(x.comleted.na,2, mean.subset)
head(x.completed.mean)

# (1-c) KNN
# install.packages("DMwR")
library(DMwR)

x.completed.knn <- knnImputation(x.comleted.na)
head(x.completed.knn)

# (1-c) mice_pmm
# install.packages("mice")
library(mice)
x.ip <- mice(x.comleted.na, m=5, maxit=50, meth='pmm', seed=500)
summary(x.ip)
x.ip$imp
x.completed.mice.pmm <- complete(x.ip, 1)
head(x.completed.mice.pmm)

# (1-c) mice_norm
x.ip2 <- mice(x.comleted.na, m=5, maxit=50, meth='norm', seed=500)
summary(x.ip2)
x.ip2$imp
x.completed.mice.norm <- complete(x.ip2, 1)
head(x.completed.mice.norm)

# (1-d) residual_mean
n <- 500
p <- 10
set.seed(123456)
library(MASS)
s <- matrix(rt(p*p, df=5), ncol = p)
sigma <- crossprod(s)
x <- mvrnorm(n, mu=rep(0, p), Sigma=sigma)

actual <- x[is.na(x.comleted.na)]
sub_mean <- x.completed.mean[is.na(x.comleted.na)]
m <- floor(nc*pc*missing.percentage)
sum(((sub_mean-actual)^2))

# (1-d) residual_KNN
sub_knn <- x.completed.knn[is.na(x.comleted.na)]
sum(((sub_knn-actual)^2))

# (1-d) residual_mice_pmm
sub_pmm <- x.completed.mice.pmm[is.na(x.comleted.na)]
sum(((sub_pmm-actual)^2))

# (1-d) residual_mice_norm
sub_norm <- x.completed.mice.norm[is.na(x.comleted.na)]
sum(((sub_norm-actual)^2))

# 2
# (2-a)
setwd("C:/Users/ASUS/Desktop/NTPU/108-2-資料探勘/DM2020/data")
base<-read.table("Concrete_1030x9.txt", header = T)
fm <- lm(y ~ Cement+BFS+FlyA+Water+Sp+CA+FineA+Age, data = base)
summary(fm)

#(2-b) standardize
scale.subset <- function(x){
  x <- scale(x)
  x
}
x.scale <- apply(base,2, scale.subset)
x.scale <- as.data.frame(x.scale)

fm.std <- lm(y ~ Cement+BFS+FlyA+Water+Sp+CA+FineA+Age, data = x.scale)
sum.std <- summary(fm.std)
sum.std

#(2-b) Box-Cox
bc <- function(y,lambda){
  (y^lambda-1)/lambda
}

bc1 <- bc(base,0.1)
fm.box <- lm(y ~ Cement+BFS+FlyA+Water+Sp+CA+FineA+Age, data = bc1)
sum.box <- summary(fm.box)
sum.box

# (2-b) Log Transformation
logx <- function(x){
  log(x+1-min(x))
}

x.log <- apply(base,2, logx)
x.log <- as.data.frame(x.log)

fm.log <- lm(y ~ Cement+BFS+FlyA+Water+Sp+CA+FineA+Age, data = x.log)
sum.log <- summary(fm.log)
sum.log

# (2-b) sqrt
sqrtx <- function(x){
  sqrt(x)*10
}
x.sqrt <- apply(base, 2, sqrtx)
x.sqrt <- as.data.frame(x.sqrt)

fm.sqrt <- lm(y ~ Cement+BFS+FlyA+Water+Sp+CA+FineA+Age, data = x.sqrt)
sum.sqrt <- summary(fm.sqrt)
sum.sqrt

# (2-b) Hellinger Transformation
hellingerx <- function(x){
  sqrt(x/sum(x))
}

x.hellinger <- apply(base, 2, hellingerx)
x.hellinger <- as.data.frame(x.hellinger)

fm.hellinger <- lm(y ~ Cement+BFS+FlyA+Water+Sp+CA+FineA+Age, data = x.hellinger)
sum.hellinger <- summary(fm.hellinger)
sum.hellinger

# compare
name <- c('std', 'box', 'log', 'sqrt', 'hellinger')
num <- c(sum.std$adj.r.squared, sum.box$adj.r.squared, sum.log$adj.r.squared,
         sum.sqrt$adj.r.squared, sum.hellinger$adj.r.squared)
cbind(name, num)

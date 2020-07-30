## 統計模型B05
# 8/68
y <- rnorm(50)
x1 <- rnorm(50)
x2 <- rnorm(50)
x3 <- rnorm(50)
lm(y ~ x1 + x2)
lm(y ~ x1 - 1)
lm(y ~ x1 * x2)

y <- rnorm(50)
school <- as.factor(sample(c("a", "b", "c"), 50, replace=T))
gender <- as.factor(sample(c("f", "m"), 50, replace=T))
table(school, gender)
lm(y ~ school / gender)
lm(y ~ gender / school)


# 9/68
lm(y ~ x1 | x2)

lm(y ~ x1:x2:x3)

##Create a formula for a model with a large number of variables:
xnam <- paste("x", 1:25, sep="")
(fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+"))))


# 10/68
lm(y ~ A*B*C)
lm(y ~ A/B/C)

lm(y ~ (A+B+C)^3)
lm(y ~ (A+B+C)^2)


# 15/68
wind <- airquality$Wind
temp <- airquality$Temp
plot(temp, wind, main="scatterplot of wind vs temp")


# 16/68
y <- airquality$Wind
x <- airquality$Temp
xbar <- mean(x) ; xbar
ybar <- mean(y) ; ybar
beta1.num <- sum((x-xbar)*(y-ybar))
beta1.den <- sum((x-xbar)^2)
(beta1.hat <- beta1.num/beta1.den)
(beta0.hat <- ybar-beta1.hat*xbar) 
yhat <- beta0.hat + beta1.hat * x

Sxy <- sum(y*(x-xbar)) ; Sxy
Sxx <- sum((x-xbar)^2) ; Sxx
Syy <- sum((y-ybar)^2) ; Syy
beta1.hat2 <- Sxy/Sxx ; beta1.hat2


#17/68
wind <- airquality$Wind
temp <- airquality$Temp
n <- length(wind)
index <- sample(1:n, 10)
wind.subset <- wind[index]
temp.subset <- temp[index]
plot(wind.subset~temp.subset, main="subset of wind vs temp")
subset.lm <- lm(wind.subset~temp.subset)
abline(subset.lm, col="red")
segments(temp.subset, fitted(subset.lm), temp.subset, wind.subset)


# 18/68
## lsfit => 使用最小平方法做回歸
## 如果用lm則是使用最大概似法做回歸
model.fit <- lsfit(temp, wind)
plot(temp, wind, main="temp vs wind", pch=20)
abline(model.fit, col="red")
text(80,19, "Regression line:")
text(80,18, "y = 23.2337 - 0.1705 x")


# 20/68
my.model <- lm(wind ~ temp)
my.model # 這邊只有係數
summary(my.model) # summary才可以把整個報表印出來

plot(wind ~ temp, main="airquality")
abline(my.model, col="red")
text(80,19, "Regression line:")
text(80,18, "y = 23.2337 - 0.1705 x")


# 21/68
my.aov <- aov(my.model)
summary(my.aov)

n <- length(wind)
e <- y-yhat
SSE <- sum(e^2) ; SSE
MSE <- SSE/(n-2) ; MSE
SSR <- beta1.hat*Sxy ; SSR
MSR <- SSR/1 ; MSR
SST <- SSR + SSE ; SST
Syy
F0 <- MSR/MSE; F0


# 23/68
alpha <- 0.05
se.beta0 <- sqrt(MSE*(1/n+xbar^2/Sxx)) ; se.beta0
tstar <- qt(alpha/2, n-1)* se.beta0 
CI.beta0 <- beta0.hat + c(-tstar*se.beta0, tstar*se.beta0) ; CI.beta0

se.beta1 <- sqrt(MSE/Sxx) ; se.beta1
tstar <- qt(alpha/2, n-1)* se.beta1 
CI.beta1 <- beta1.hat + c(-tstar*se.beta0, tstar*se.beta1); CI.beta1


# 25/68
my.model <- lm(wind ~ temp)
summary(my.model)


# 26/68
my.model <- lm(wind ~ temp)
summary(my.model)

coef(my.model)
vcov(my.model)


#27/68
summary(my.model)[[1]]  # my.model formula
summary(my.model)[[2]]  # attributes of the objects

length(summary(my.model))
names(summary(my.model))
summary(my.model)$sigma
summary(my.model)[[6]]
length(summary(my.model)[[1]])
length(summary(my.model)[[2]])
length(summary(my.model)[[3]])


# 28/68
summary(my.model)[[3]]  # residuals for data points
summary(my.model)[[4]]  # parameters table
summary(my.model)[[4]][[1]]  # intercept
summary(my.model)[[4]][[2]]  # slope,.... summary(my.model)[[4]][[28]]

str(summary(my.model)[[4]])


# 29/68
summary(my.model)[[5]]  # whether the fit should be returned.
summary(my.model)[[6]]  # residual standard error
summary(my.model)[[7]]  # the number of rows in the summary.lm table.
summary(my.model)[[8]]  # r square, the fraction of the total variation in the response variable that is explained by the my.model.
summary(my.model)[[9]]  # adjusted r square
summary(my.model)[[10]]  # F ratio information
summary(my.model)[[11]]  # correlation matrix of the parameter estimates.


# 30/68
my.model <- lm(wind ~ temp)
names(my.model)
model$coefficients
model$fitted.values
model$residuals

summary.aov(my.model)
summary.aov(my.model)[[1]][[1]]~
  summary.aov(my.model)[[1]][[5]]


# 31/68 使用["_name_"]
(iris.aov <- aov(iris[,1]~iris[,5]))
(iris.sum.aov <- summary(iris.aov))
(iris.sum.aov2 <- unlist(iris.sum.aov))
names(iris.sum.aov2)
iris.sum.aov2["Pr(>F)1"]


# 32/68
new.model <- update(my.model, subset=(temp!=max(temp)))
summary(new.model) 


# 33/68
summary(wind)
summary(temp)
predict(my.model, list(temp=75))
predict(my.model, list(temp=c(66, 80,100)))

xx <- predict(my.model)

# 練習
plot(wind ~ temp, main="airquality")
abline(my.model, col="red")
text(80,19, "Regression line:")
text(80,18, "y = 23.2337 - 0.1705 x")
x <- predict(my.model, list(temp=c(66,75,80,100)))
x
text(c(66,75,80,100),x,x)
plot(xx)


# 35/68
?plot.lm

# 殘差圖
wind <- airquality$Wind
temp <- airquality$Temp
my.model <- lm(wind ~ temp)
plot(my.model, which=1:6) # 有六個圖

plot(fitted(my.model), residuals(my.model),xlab="Fitted values",ylab="Residuals")
abline(h=0, lty=2)

# 38/68
# QQ plot 的殘差圖一定要會

# 39/68
# 練習cook's distance的計算

# 40/68
# laverage也要算出來

# 42/68
head(swiss)


# 47/68
year <- rep(2008:2010, each=4)
quarter <- rep(1:4, 3)
cpi <- c(162.2, 164.6, 166.5, 166.0,
         166.2, 167.0, 168.6, 169.5,
         171.0, 172.1, 173.3, 174.0)
cbind(cpi, year, quarter)
plot(cpi, xaxt="n", ylab="CPI", xlab="")
axis(1, labels=paste(year, quarter, sep="Q"), at=1:12, las=3) # las=3: vertical text.

cor(year, cpi)
cor(quarter, cpi)


# 48/68
fit <- lm(cpi ~ year + quarter)
fit
attributes(fit)
fit$coefficients
residuals(fit)


# 49/68
summary(fit)


# 50/68
plot(fit)


# 51/68
library(scatterplot3d)
s3d <- scatterplot3d(year, quarter, cpi, highlight.3d=T, type="h", lab=c(2,3))
s3d$plane3d(fit)


# 52/68
data2011 <- data.frame(year=2011, quarter=1:4)
cpi2011 <- predict(fit, newdata=data2011)
cpi2011
style <- c(rep(1,12), rep(2,4))
plot(c(cpi, cpi2011), xaxt="n", ylab="CPI", xlab="", pch=style, col=style)
axis(1, at=1:16, las=3, labels=c(paste(year, quarter, sep="Q"), "2011Q1", "2011Q2", "2011Q3", "2011Q4"))


# 53/68
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
dim(mydata)
head(mydata)
summary(mydata)
sapply(mydata, sd)

xtabs(~ admit + rank, data = mydata)
mydata$rank <- factor(mydata$rank)


# 55/68
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit)


# 56/68
install.packages("aod")
library(aod) #aod: Analysis of Overdispersed Data
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)

l <- cbind(0,0,0,1,-1,0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)


# 57/68
exp(cbind(OR = coef(mylogit), confint(mylogit)))


# 58/68
newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
newdata1 
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1


# 59/68
newdata2 <- with(mydata,
                 data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100), 4),
                            gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))
dim(newdata2)
head(newdata2)
newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type="link", se=TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
head(newdata3)


# 60/68
library(ggplot2) 
ggplot(newdata3, aes(x = gre, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = .2) +
  geom_line(aes(colour = rank), size=1)


# 61/68
with(mylogit, null.deviance - deviance)
with(mylogit, df.null - df.residual)
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(mylogit)


# 62/68
anova(mylogit, test="Chisq")

drop1(mylogit, test="Chisq")


# 66/68
head(airquality)
model0 <- lm(Ozone ~ Wind + Temp + Solar.R, data=airquality)


# 67/68
cor(airquality[,1:4], use = "pairwise")
pairs(airquality[,1:4])

library(car)
vif(model0)


# 68/68
summary(model0)

library(fmsb)
model1 <- lm(Wind ~ Temp + Solar.R, data=airquality)
model2 <- lm(Temp ~ Wind + Solar.R, data=airquality)
model3 <- lm(Solar.R ~ Wind + Temp, data=airquality)
VIF(model0)
sapply(list(model1, model2, model3), VIF)







# 2020/04/18
# 遺失值補值需要去看資料的型態，有些補值會不理會資料型態
# 遺失值用mean補
x[is.na(x)] <- mean(x, na.rm = TRUE)

# KNN是比較常用在補遺失值的方式，但是資料比較大的時候，會跑比較久
# 可以用平均數、中位數等等的
install.packages("VIM")
library(VIM)
names(airquality)
airquality.imp.median <- kNN(airquality[1:4], k=5) # 內建補值預設中位數
head(airquality.imp.median)

# 補值前圖案 vs. 補值後圖案
matrixplot(airquality[1:4], interactive = F, main="airquality")
matrixplot(airquality.imp.median[1:4], interactive = F, main="imputed by median")

# 截斷平均數 - 排除極端值(+-10%)
trim_mean <- function(x){
  mean(x, trim = 0.1)
}

# 使用截斷平均數去補值
airquality.imp.tmean <- kNN(airquality[1:4], k=5, numFun=trim_mean)
airquality.imp.tmean

# 另外一個補值的方式! package = MICE

# Generate 10% missing values at Random = 先對iris做成遺失值
install.packages("missForest")
library(missForest)
iris.mis <- prodNA(iris, noNA = 0.1)

# Check missing values introduced in the data
summary(iris.mis)
iris.mis <- subset(iris.mis, select = -c(Species))
summary(iris.mis)

# A tabular form of missing value present in each variable
install.packages("mice")
library(mice)
md.pattern(iris.mis) #看一下遺失值狀況
# Visualization
library(VIM)
mice_plot <- aggr(iris.mis, col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE,
                    labels=names(iris.mis), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))
# Imputation
# 方法: pmm = predictive mean matching 預測平均值匹配
imputed.Data <- mice(iris.mis, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed.Data)
# Check imputed values
imputed.Data$imp$Sepal.Width
# Get complete data (2nd out of 5)
completeData <- complete(imputed.Data,2)

# Build predictive model
# MICE + 多重迴歸
fit <- with(data = imputed.Data, exp = lm(Sepal.Width ~ Sepal.Length + Petal.Width))

# Combine results of all 5 models
combine <- pool(fit)
summary(combine)

# 資料轉換 29/70

par(mfrow=c(1,4))
raw.data <- 0:100
pa.data <- ifelse(raw.data >= 60, 1, 0)
id <- which(pa.data==1)
plot(raw.data[id], pa.data[id], main="present-absent",
     type="l", lwd=2, col="blue", ylim=c(-1, 2), xlim=c(0, 100))
points(raw.data[-id], pa.data[-id], type="l", lwd=2, col="blue")

# 對數轉換--如果數字非常大! 可以取log把極值壓縮
log.data <- log(raw.data)
plot(raw.data, log.data, main="log", type="l", lwd=2, col="blue")

# 開根號*10
# 把計算出來的資料 - 原本的資料找出距離最大的
# asp = 1 這個指令很重要
sqrt10.data <- sqrt(raw.data)*10
plot(raw.data, sqrt10.data, main="sqrt*10", type="l", lwd=2, col="blue", asp=1)
abline(a=0, b=1)

# 截斷方法truncate : 資料大於80就算80
trun.data <- ifelse(raw.data >= 80, 80, ifelse(raw.data < 20, 20, raw.data))
plot(raw.data, trun.data, main="truncation", type="l", lwd=2, col="blue")





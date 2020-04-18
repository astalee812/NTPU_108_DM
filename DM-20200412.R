
# 45/113
(x <- matrix(1:24, nrow=4))

#1: rows, 2:columns
apply(x, 1, sum)

#apply function to the individual elements: 用於矩陣
apply(x, 1, sqrt) #row 
apply(x, 2, sqrt) #colums

# 46/113
# generate score data
math <- sample(1:100, 50, replace=T)
english <- sample(1:100, 50, replace=T)
algebra <- sample(1:100, 50, replace=T)
ScoreData <- cbind(math, english, algebra)
head(ScoreData, 5)

#sdata1 <- apply(ScoreData, 2, myfun)
head(sdata1, 5)

#要把分數開根號+10
head(apply(ScoreData, 2, function(x) sqrt(x)*10), 5)

#sdata2 <- apply(ScoreData, 2, myfun2, attend=5)
head(sdata2, 5)

# tapply在要分組的時候計算(有類別資料)
# 47/113
set.seed(12345)
scores <- sample(0:100, 50, replace=T)
grade <- as.factor(sample(c("大一", "大二", "大三", "大四"), 50, replace=T))
bloodtype <- as.factor(sample(c("A","AB","B","O"), 50, replace=T))
tapply(scores, grade, mean)

tapply(scores, bloodtype, mean)
# 按照年級跟血型來計算成績的平均
tapply(scores, list(grade,bloodtype), mean)

summary(warpbreaks[,-1])

tapply(warpbreaks$breaks, warpbreaks[,-1], sum)

# 49/113
# lapply -> list的計算
a <- c("a", "b", "c", "d")
b <- c(1, 2, 3, 4, 4, 3, 2, 1)
c <- c(T, T, F)
list.object <- list(a,b,c)
my.la1 <- lapply(list.object, length)
my.la1

my.la2 <- lapply(list.object, class)
my.la2

# (T and F 在計算的時候會自動轉成0跟1)
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
lapply(x, mean) # return list : 結果是列表

# median and quartiles for each list element
lapply(x, quantile, probs = 1:3/4)

# lapply 跟 sapply很像，只是產出結果不同
# 50/113
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
sapply(x, mean)  # return vector

# 比較一下
sapply(x, quantile)
lapply(x, quantile)


# 52/113
my.list <- list(name=c("George", "John", "Tom"),
                wife=c("Mary", "Sue", "Nico"), 
                no.children=c(3, 2, 0), 
                child.ages=list(c(4,7,9), c(2, 5), NA))
my.list

# 取出某一家庭的資訊 : 為什麼要用兩個中括號
my.list[[1]][1]
my.list[[3]][1]
my.list[[4]][1]

my.list[[1:4]][1] # Error : 這個沒辦法抓

# 要用下列的方式才抓得出來
George.family <- sapply(my.list,"[[", 1)
George.family 

# 53/113
mapply(rep, 1:4, 4:1) # rep(1,4) : 1重覆4次

# times:重覆幾次; length.out:長度限制; each: 每個重覆幾次
mapply(rep, times = 1:4, x = 4:1) 

# rapply
# 56/113
mydata <- list(list(a = pi, b = list(c = 1:1)), d = "a test")
mydata

# 資料開根號! 數值型才開! 然後把答案蓋過去原本的資料
rapply(mydata, sqrt, classes = "numeric", how = "replace")

# 結果變成表列，針對字元做計算
rapply(mydata, nchar, classes = "character",
       deflt = as.integer(NA), how = "list") # NA保留住，deflt = as.integer(NA)

rapply(mydata, nchar, classes = "character",
       deflt = as.integer(NA), how = "unlist")

# 用iris資料集4變數畫長方圖#
# 針對多變數來做運算與畫圖
hist(iris[,1])

# method-1 #
par(mfrow = c(2,2))
hist(iris[,1], main = name(iris)[1])
hist(iris[,2], main = name(iris)[2])
hist(iris[,3], main = name(iris)[3])
hist(iris[,4], main = name(iris)[4])

# method-2 #
par(mfrow = c(2,2))
for(i in 1:4){
  hist(iris[,i], main = names(iris)[i])
}

# method-3#
par(mfrow = c(2,2))
lapply(iris[,1:4],hist) # 只送資料本身，沒有名字
lapply(iris[,1:4],function(x) hist(x)) # 名字一樣畫不出來

# method-4#
par(mfrow = c(2,2))
lapply(1:4, function(x) hist(iris[,x],
                             main = names(iris[x])))

# 遺失值處理: 用平均數去補會破壞資料結構! 平均數不是一個好方法
# 遺失值要看一下遺失值比例，可以研究一下遺失值處理


# 3/28的(簡化版)
myvector <- c(10, 20, NA, 30, 40)
myvector

mycountry <- c("Austria", "Australia", NA, NA, "Germany", "NA")
mycountry

is.na(myvector)
which(is.na(myvector))

x <- c(1, 4, 7, 10)
x[4] <- NA
x

is.na(x) <- 1
x

# 10/70
set.seed(12345)
mydata <- matrix(round(rnorm(20), 2), ncol=5)
mydata[sample(1:20, 3)] <- NA
mydata
which(colSums(is.na(mydata)) > 0)

# 11/70
x <- c(1, 4, NA, 10)
summary(x)
mean(x)
sd(x)
mean(x, na.rm=TRUE)
sd(x, na.rm=TRUE)
x[!is.na(x)]

# 12/70
mydata <- as.data.frame(matrix(sample(1:20, 8), ncol = 2))
mydata[4, 2] <- NA
names(mydata) <- c("y", "x")
mydata
lm(y~x, data = mydata)
lm(y~x, data = mydata, na.action = na.omit)
lm(y~x, data = mydata, na.action = na.fail)

# 13/70
x <- c(1, 0, 10)
x/x
is.nan(x/x)
1/x
is.finite(1/x)
-10/x
is.infinite(-10/x)
exp(-Inf)
0/Inf
Inf - Inf
Inf/Inf

# 17/70
head(airquality)
dim(airquality)
mydata <- airquality
mydata[4:10, 3] <- rep(NA, 7)
mydata[1:5, 4] <- NA
summary(mydata)

# 18/70
install.packages("mice")
library(mice)
md.pattern(mydata)

install.packages("VIM")
library(VIM)
mydata.aggrplot <- aggr(mydata,
                        col=c('lightblue','red'), numbers=TRUE,
                        prop = TRUE, sortVars=TRUE,
                        labels=names(mydata), cex.axis=.7, gap=3)

# 19/70
# 用矩陣圖把原始資料弄出來!紅色部分有遺失，有依照數字大小做色階
matrixplot(mydata) 
md.pairs(mydata)
marginplot(mydata[,c("Ozone", "Solar.R")], col = c("blue", "red"))

# 22/70
# 有遺失就刪掉
mdata <- matrix(rnorm(15), nrow=5)
mdata[sample(1:15, 4)] <- NA 
mdata <- as.data.frame(mdata)
mdata
# na.omit把遺失的刪掉 : 遺失資料集
(x1 <- na.omit(mdata))
# 完整資料集
(x2 <- mdata[complete.cases(mdata),])


# 23/70 : 討論不要全刪的狀況
mdata
cov(mdata)
cov(mdata, use = "all.obs")
cov(mdata, use = "complete.obs")
cov(mdata, use = "na.or.complete")
cov(mdata, use = "pairwise")




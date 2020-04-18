getwd()
setwd("F:/108_NTPU/108_2_資料探勘/DM2020/data")
# 17/113
# 資料轉呈矩陣使用data.frame, data.frame轉成列連表用xtable
install.packages("epitools")
library(epitools)
survey <- array(0, dim=c(3, 2, 1))
survey[,1,1] <- c(2, 0, 1)  
survey[,2,1] <- c(3, 2, 4)

Satisfactory <- c("Good", "Fair", "Bad")
Sex <- c("Female", "Male")
Times <- c("First")
dimnames(survey) <- list(Satisfactory, Sex, Times)
names(dimnames(survey)) <- c("Satisfactory", "Sex", "Times")
survey
# expand.table是把資料展開變成tidy data
(survey.ex <- expand.table(survey))

# 18/113
data(HairEyeColor)
HairEyeColor

HairEyeColor.ex <- expand.table(HairEyeColor) 
HairEyeColor.ex

# 資料變型! 多了frequence
as.data.frame(HairEyeColor)

# 做堆疊 stack 一樣做資料變型
# 19/113
elections <- read.csv('elections-2000.csv')
elections
elections.stacked <- cbind(stack(elections[,-1]),   
                           county = elections$County)
elections.stacked

plot(elections.stacked[, c(2, 1)])
boxplot(elections[,-1])


# 20/113
# 文字資料做堆疊
mydata <- data.frame(Area1=c("A", "B", "B", "C"), Area2=c("A", "D", "E", "B"))
rownames(mydata) <- paste("rater", 1:4, sep="-")
mydata
stack(mydata) # 這邊會有error
# 使用lapply來做轉換
mydata.stack <- stack(lapply(mydata, as.character))
colnames(mydata.stack) <-  c("Rate", "Area")
mydata.stack


# 21/113
head(state.x77)
dim(state.x77)
state.region
aggregate(state.x77, list(Region = state.region), mean)

# 22/113
## Compute the averages according to region and the occurrence of more
## than 130 days of frost.
aggregate(state.x77,
          by = list(Region = state.region,
                    Cold = state.x77[,"Frost"] > 130),
          FUN = function(x){round(mean(x), 2)})

aggregate(state.x77,
          by = list(Region = state.region,
                    Cold = state.x77[,"Frost"] > 130),
          FUN = function(x){ round(sqrt(sum(x^2)), 2)}) # 這邊可以自定函數


# 23/113
presidents # 有關時間序列
# same as apply(presidents, 1, mean)

# nfreqency:new number of observations per unit of time; must be a divisor of the frequency of x
aggregate(presidents, nfrequency = 1, FUN = mean) 
# w是for weighted.mean
aggregate(presidents, nfrequency = 1, FUN = weighted.mean, w = c(1, 1, 0.5, 1))


# 24/113
testDF <- data.frame(v1 = c(1,3,5,7,8,3,5,NA,4,5,7,9),
                     v2 = c(11,33,55,77,88,33,55,NA,44,55,77,99))

testDF

by1 <- c("red", "blue", 1, 2, NA, "big", 1, 2, "red", 1, NA, 12)
by2 <- c("wet", "dry", 99, 95, NA, "damp", 95, 99, "red", 99, NA, NA)
# 將testDF的資料依照by1 and b2的分組方式來做mean
aggregate(x = testDF, by = list(by1, by2), FUN = "mean")
# 把NA也算成一個類別
fby1 <- factor(by1, exclude = "")
fby2 <- factor(by2, exclude = "")
aggregate(x = testDF, by = list(fby1, fby2), FUN = "mean")

# 25/113
chickwts
summary(chickwts)
aggregate(weight ~ feed, data = chickwts, mean)
summary(warpbreaks)
aggregate(breaks ~ wool + tension, data = warpbreaks, mean)


# y1 ~ x1+x2+...xi -- 多變數回歸
# y1+y2 ~ x1+x2+...xi  -- 複迴歸

# 26/113
summary(esoph)
aggregate(cbind(ncases, ncontrols) ~ alcgp + tobgp, data = esoph, sum)

# 27/113 根據因子做分組
by(iris[,1:4] , iris$Species , summary)
iris[,1:4]

# 28/113
by(iris[,1:4] ,iris$Species, sum)
by(iris[,1:4] ,iris$Species, mean) # 再這邊會有error! 要每個欄位都可以算
mean(iris[iris$Species == "setosa", 1:4])

# 29/113
by(iris[,1] , iris$Species , mean) # 再這邊會有error! 要每個欄位都可以算
by(iris[,1:4] , iris$Species , colMeans)

# 30/113
varMean <- function(x, ...) sapply(x, mean, ...)
by(iris[, 1:4], iris$Species, varMean)

with(iris, aggregate(iris[,1:4], list(Species = iris$Species), FUN = mean))


# 31/113
#cut(x, breaks, labels = NULL,
#    include.lowest = FALSE, right = TRUE, dig.lab = 3, ordered_result = FALSE, ...)

x <- rnorm(50)
(x.cut1 <- cut(x, breaks = -5:5))
table(x.cut1)
(x.cut2 <- cut(x, breaks = -5:5, labels = FALSE)) # 不要標籤
table(x.cut2)
# 直方圖會和cut有連結，plot = F 不要把圖畫出來，把個數算出來
hist(x, breaks = -5:5, plot = FALSE)$counts

# 32/113
#the outer limits are moved away by 0.1% of the range
age <- sample(0:80, 50, replace=T) # replace = T 取出放回
summary(age)
cut(age, 5)
mygroup <- c(0, 15, 20, 50, 60, 80) # 我指定分組範圍
(x.cut <- cut(age, mygroup))
table(x.cut)

# 34/113
#merge(x, y, by = intersect(names(x), names(y)),
#      by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all,
#      sort = TRUE, suffixes = c(".x",".y"),
#      incomparables = NULL, ...)

# total <- merge(data.frame.A, data.frame.B, by="ID")
# total <- merge(data.frame.A, data.frame.B, by=c("ID","Country")) 

# 37/113
authors <- data.frame(
  surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
  nationality = c("US", "Australia", "US", "UK", "Australia"),
  deceased = c("yes", rep("no", 4)))
books <- data.frame(
  name = I(c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core")),
  title = c("Exploratory Data Analysis",
            "Modern Applied Statistics ...",
            "LISP-STAT",
            "Spatial Statistics", "Stochastic Simulation",
            "Interactive Data Analysis",
            "An Introduction to R"),
  other.author = c(NA, "Ripley", NA, NA, NA, NA, "Venables & Smith"))
authors 
books 

author <- c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")
(a1 <- data.frame(surname = author))
(a2 <- data.frame(surname = I(author)))

class(a1$surname) # [1] "factor"
class(a2$surname) # [1] "AsIs"
a1$surname
a2$surname

# 38/113
(m1 <- merge(authors, books, by.x = "surname", by.y = "name"))
(m2 <- merge(books, authors, by.x = "name", by.y = "surname"))

merge(authors, books, by.x = "surname", by.y = "name", all = TRUE)

# 39/113
(x <- data.frame(k1 = c(NA,NA,3,4,5), k2 = c(1,NA,NA,4,5), data = 1:5))
(y <- data.frame(k1 = c(NA,2,NA,4,5), k2 = c(NA,NA,3,4,5), data = 1:5))
merge(x, y, by = c("k1","k2")) # NA's match 根據兩個欄位來配對
merge(x, y, by = "k1") # NA's match, so 6 rows
merge(x, y, by = "k2", incomparables = NA) # 2 rows

# 40/113
stories <- read.table(header=TRUE, text='
                      storyid  title
                      1       lions
                      2      tigers
                      3       bears
                      ')
data <- read.table(header=TRUE, text='
                   subject storyid rating
                   1       1    6.7
                   1       2    4.5
                   1       3    3.7
                   2       2    3.3
                   2       3    4.1
                   2       1    5.2
                   ')
merge(stories, data, by="storyid")

stories2 <- read.table(header=TRUE, text='
                       id       title
                       1       lions
                       2      tigers
                       3       bears
                       ')
merge(stories2, data, by.x="id", by.y="storyid")


# 41/113
animals <- read.table(header=T, text='
                      size type         name
                      small  cat         lynx
                      big  cat        tiger
                      small  dog    chihuahua
                      big  dog "great dane"
                      ')
observations <- read.table(header=T, text='
                           number  size type
                           1   big  cat
                           2 small  dog
                           3 small  dog
                           4   big  dog
                           ')
merge(observations, animals, c("size","type"))


# 42/113
#split(x, f, drop = FALSE, ...)
#split(x, f, drop = FALSE, ...) <- value
#unsplit(value, f, drop = FALSE)

n <- 10
edu <- factor(sample(1:4, n, replace=T))
score <- sample(0:100, n)
cbind(edu, score)

score.edu <- split(score, edu) # 這邊有分組
score.edu

unsplit(score.edu, edu)
sort(edu)
unsplit(score.edu, sort(edu))


# 43/113
head(airquality)
month <- airquality$Month
airquality.month <- split(airquality, month) # 根據月份做切分
mydata <- lapply(airquality.month, transform, # 要做資料轉化要寫transform參數
                 Oz.Z = scale(Ozone)) # oz.z做標準化 scale
airquality2 <- unsplit(mydata, month)
head(airquality2)

airquality.month

transform(airquality, Ozone = -Ozone)
transform(airquality, new = -Ozone, Temp = (Temp-32)/1.8)

attach(airquality)
transform(Ozone, logOzone = log(Ozone))


# 45/113
(x <- matrix(1:24, nrow=4))

#1: rows, 2:columns
apply(x, 1, sum)

#apply function to the individual elements
apply(x, 1, sqrt) 
apply(x, 2, sqrt)

# 46/113
# generate score data
math <- sample(1:100, 50, replace=T)
english <- sample(1:100, 50, replace=T)
algebra <- sample(1:100, 50, replace=T)
ScoreData <- cbind(math, english, algebra)
head(ScoreData, 5)

#sdata1 <- apply(ScoreData, 2, myfun)
head(sdata1, 5)

head(apply(ScoreData, 2, function(x) sqrt(x)*10), 5)

#sdata2 <- apply(ScoreData, 2, myfun2, attend=5)
head(sdata2, 5)


# 47/113
set.seed(12345)
scores <- sample(0:100, 50, replace=T)
grade <- as.factor(sample(c("大一", "大二", "大三", "大四"), 50, replace=T))
bloodtype <- as.factor(sample(c("A","AB","B","O"), 50, replace=T))
tapply(scores, grade, mean)

tapply(scores, bloodtype, mean)
tapply(scores, list(grade,bloodtype), mean)

summary(warpbreaks[,-1])

tapply(warpbreaks$breaks, warpbreaks[,-1], sum)

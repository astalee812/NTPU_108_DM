getwd()

head(anscombe)

#apply(), 1為橫列，2為直行
apply(anscombe, 2, mean)
apply(anscombe, 2, sd)

#計算兩兩之間的相關係數
#mapply()
mapply(cor, anscombe[,1:4], anscombe[,5:8])

#做迴歸分析把係數取出來
mapply(function(x, y) lm(y~x)$coefficients, anscombe[, 1:4], anscombe[, 5:8])

#畫散佈圖之後把迴歸線畫出來
par(mfrow=c(2, 2)) # 把畫面分割成2*2
regplot <- function(x, y){
  plot(y~x) #用x去預測y
  abline(lm(y~x), col="red") #abline()表示要加線，這個線是迴歸線lm(y~x)
}
mapply(regplot, anscombe[, 1:4], anscombe[, 5:8])

#2020/03/14
head(iris)
plot(iris$Sepal.Length, col = iris$Species, ylim = c(0,8))

par(mfrow=c(2, 2))
myplot <- function(x, y){
  plot(x, col = iris$Species, ylim = c(0,8))
}

apply(iris[,1:4],2,myplot)


#--------------------------------------------------------#
install.packages("ggplot2")
library(ggplot2)

x <- rnorm(mean=1.5, 10000)
y <- rnorm(mean=1.6, 10000)
my.data <- data.frame(x, y)

pk <- c("RColorBrewer", "hexbin", "gplots")
install.packages(pk, repos="http://cran.csie.ntu.edu.tw")
library(RColorBrewer)

col_rb <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))

plot(my.data, pch=16, col='black', cex=0.5)

#六角形的點點
library(hexbin)
h <- hexbin(my.data)
h
plot(h)
plot(h, colramp=col_rb) 

#tableplot要研究一下，下載舊的版本，格式也有改變
#考試會考想辦法跑出來!
install.packages("tabplot")
library(tabplot)
tableplot(iris, nBins=150, sortCol=5)
tableplot(iris, nBins=50, sortCol=4)

#ggplot2
install.packages("ggplot2")
library(ggplot2)
data("diamonds")
head(diamonds)
dim(diamonds)

#---tabplot solve---#
install.packages("Rcpp")
install.packages("devtools")
library(devtools)
install_github("mtennekes/tabplot")
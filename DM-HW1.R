
# ex2
par(mfrow = c(2,2))
lapply(1:4, function(x) plot(iris[,x], col = iris$Species, ylab = "cm", ylim = c(0,8), main = names(iris[x])))


# ex1-1
Weibull <- function(x,a,b){
  xx <- ((a*b)^(-a)) * ((x)^(a-1)) * exp(-((x/b)^a))
  return (xx)
}
x <- seq(0, 5, 0.1)
Weibull(x,1,2)
Weibull(x,2,2)
dweibull(x,2,2)

# ex1-2
curve(dweibull(x,1,2), 0, 5, xlab = "x", ylab = "f(x)", ylim = c(0,1.5), main = "X~Weib(alpha, beta=2)")
text(0.5,0.5 , expression(paste(alpha,"=1")), cex=1.2)
curve(dweibull(x,2,2), 0, 5, lty = 2, add = T)
text(3,0.25 , expression(paste(alpha,"=2")), cex=1.2)
curve(dweibull(x,5,2), 0, 5, lty = 3, add = T)
text(2.5,0.8 , expression(paste(alpha,"=5")), cex=1.2)

#3-1
data1 <- read.csv("C:/Users/ASUS/Desktop/NTPU/108-2-資料探勘/108-2-DM-HW1/108-2-DM-HW1/不動產實價登錄資訊2.csv")
View(data1)
floor <- gsub("層","",data1$rps10)

cN = c("一","二","三","四","五","六","七","八","九","十")
eN1 = as.character(1:10)
names(eN1)=cN
eN2a = eN2b = eN3 = eN1
eN2a[10] = "1"
eN2b[10] = "0"
eN3[10]=""

eN1
eN2a
eN2b
eN3

library(mgsub)
library(stringr)

floor2 = NULL
for (x in floor)
{
  m = str_length(x)
  if (m == 1)
    out = as.numeric(eN1[x])
  else if (m == 2)
  {
    if (grepl("^十",x))
      out = as.numeric(mgsub(x, cN, eN2a))
    else
      out = as.numeric(mgsub(x, cN, eN2b))
  } else {
    out = as.numeric(mgsub(x, cN, eN3))
  }
  floor2 = c(floor2,out)
  }
      
data1$rps10 <- floor2
dim(data1)
str(data1)
summary(data1)

#3-2
plot(data1$district, data1$rps22, cex.axis = 0.7)
plot(data1$district, data1$rps01, main = "地區*交易標的")
plot(data1$district, data1$rps11, main = "地區*建物型態")
plot(data1$district, data1$rps12, main = "地區*主要用途")


#4-1
data1 <- read.csv("C:/Users/ASUS/Desktop/NTPU/108-2-資料探勘/108-2-DM-HW1/108-2-DM-HW1/不動產實價登錄資訊2.csv")
aggregate(cbind(rps03,rps15,rps21,rps22,rps24,rps25)~district, data = data1, mean)



#4/25
#40/70
head(airquality)
r <- range(airquality[,1:4], na.rm = T)
hist(airquality$Ozone , xlim = r)
hist(airquality$Solar.R, xlim = r)
hist(airquality$Wind, xlim = r)
hist(airquality$Temp, xlim = r)

airquality.std <- as.data.frame(apply(airquality, 2, scale))
r.std <- c(-3, 3)
hist(airquality.std$Ozone, xlim = r.std)
hist(airquality.std$Solar.R, xlim = r.std)
hist(airquality.std$Wind, xlim = r.std)
hist(airquality.std$Temp, xlim = r.std)

#42/70
setwd("c:/rdata")
cell.raw <- read.table("trad_alpha103.txt", row.names=1, header=T)
head(cell.raw)
cell.xdata <- t(scale(t(cell.raw[,2:19]), center=T, scale=T))#將資料標準化
y.C <- as.integer(cell.raw[,1])
table(y.C)
no.cluster <- length(unique(y.C))
cellcycle.color <- c("darkgreen", "blue", "red", "gray50", "orange")
p <- ncol(cell.raw) -1
ycolors <- cellcycle.color[y.C+1]
my.pch <- c(1:no.cluster)[y.C+1]
phase <- c("G1", "S", "S/G2", "G2/M", "M/G1")
matplot(t(cell.xdata), pch = 1:p, lty=1, type = "l", ylab="gene expression",
        col=ycolors, xlab="time", main="Time series", xaxt="n")
time.label <- parse(text=paste("t[",0:p,"]",sep=""))
axis(1, 1:(p+1), time.label) #改變軸的單位，1是X軸
legend("bottom", legend=phase, col=cellcycle.color, lty=1, horiz = T, lwd=2)

#47/70
#資料轉換
#資料常規劃normalization 與正規劃不太相同
#資料常規劃normalization 是將資料調整到接近0的位置，且使得後續分析有意義
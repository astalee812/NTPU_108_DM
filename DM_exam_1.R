# ex1
data1 <- read.csv("C:/Users/ASUS/Desktop/NTPU/108-2-資料探勘/108-2-DM-exam1/癌症發生統計.csv")
names(data1)
names(data1)[5] <- "年齡標準化發生率"
names(data1)[9] <- "粗率"
names(data1)

head(data1)
str(data1)
dim(data1)
summary(data1)


aggregate(data1$癌症發生數~癌症診斷年+性別+縣市別+癌症別, data = data1,sum)
dd1 <- aggregate(data1$癌症發生數~癌症診斷年, data = data1,sum)



plot(data1$癌症診斷年, cex.axis = 0.7, las=2)
plot(data1$性別)
plot(data1$縣市別, cex.axis = 0.7, las=2)
plot(data1$癌症別, cex.axis = 0.7, las=2)
plot(data1$癌症發生數)








# ex2
aggregate(癌症發生數~性別+縣市別, data = data1,sum)


# ex3
list <- aggregate(癌症發生數~性別+data1$癌症別, data = data1,sum)
head(list[order(list$癌症發生數,decreasing = T),])

# ex4
list2<-data1[data1$癌症別 %in% "胃",]
aggregate(list2$年齡中位數~性別, data = list2,mean)

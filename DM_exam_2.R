getwd()
setwd("C:/Users/ASUS/Desktop/NTPU/108-2-資料探勘/DM2020/data")
# 1 敘述統計
data1 <- read.csv("癌症發生統計.csv")

str(data1)
names(data1)
colnames(data1) <- c("癌症診斷年", "性別","縣市別","癌症別","年齡標準化發生率","癌症發生數","平均年齡","年齡中位數","粗率")
head(data1)

data12 <- data1[data1$癌症別 == '肝及肝內膽管',]
names(data12)
summary(data12)

# 1-a-mean
aggregate(data12$年齡標準化發生率 , by=list(data12$性別), FUN=mean)
# 1-a-median
aggregate(data12$年齡標準化發生率, by=list(data12$性別), FUN=median)
# 1-a-mode
as.numeric(names(table(data12$年齡標準化發生率)))[which.max(table(data12$年齡標準化發生率))]

# 1-b
data13 <- data1[data1$癌症別 %in% "胃",]
data13.sum <- aggregate(data13$癌症發生數, by = list(data13$縣市別),sum)
colnames(data13.sum)[1] <- "縣市別"
colnames(data13.sum)[2] <- "發生總次數"
data13.count <- as.data.frame(table(data13$縣市別))
colnames(data13.count)[1] <- "縣市別"
colnames(data13.count)[2] <- "診斷年個數"
data13.com <- merge(data13.sum, data13.count, by = "縣市別")
data13.com$平均癌症發生數 <- data13.com$發生總次數/data13.com$診斷年個數
head(data13.com)

# 2 假設檢定
data2 <- read.csv("消費者端量測行動上網平均速率.csv",sep = ",", fileEncoding  = "UTF-8", skip = 1)
colnames(data2) <- c("縣市","第一階段", "第二階段")
head(data2)


# 2-a
boxplot(data2$第一階段)
boxplot(data2$第二階段)
boxplot(data2$第一階段, data2$第二階段)

# 2-b
alpha <- 0.05
vt <- (var.test(data2$第一階段, data2$第二階段)$p.value) <= alpha
t.test(data2$第一階段, data2$第二階段, alternative = "two.sided",paired = T, var.equal = vt)

wilcox.test(data2$第一階段, data2$第二階段, paired = TRUE)

# 3
data3 <- read.csv("每月_103938_A43_t35世界主要證券市場成交值周轉率比較(35).csv", skip = 1, header = F, stringsAsFactors = FALSE, sep = ",")
colnames(data3) <- c("年月", "台灣", "紐約", "日本", "倫敦", "香港", "韓國", "新加坡", "上海")
head(data3)
str(data3)

# 3-a
yy <- substr(data3$年月,start = 1, stop = 4)
xx <-substr(data3$年月,start = 6, stop = 8)
data3$年月份 <- paste0(yy,xx)
as.numeric(data3$年月份)

library(magrittr)
library(dplyr)
data31 <- data3 %>% 
  select(c(1:8,10)) %>% 
  filter(data3$年月份<=200212 & data3$年月份 >= 200101 & data3$年月份 != 2002)

head(data31)

# 3-b
boxplot(data31$台灣, data31$紐約, data31$日本, data31$倫敦, data31$香港, 
        data31$韓國, data31$新加坡)


# 3-c
trade <- cbind(c(data31$台灣,data31$紐約,data31$日本,data31$倫敦,data31$香港,
                 data31$韓國,data31$新加坡))

country <-c(rep("臺灣", 24), rep("紐約", 24), rep("日本", 24), rep("倫敦", 24), rep("香港", 24),
            rep("韓國", 24), rep("新加坡", 24))

data32 <- data.frame(trade, country)

data32.aov <- aov(trade ~ country, data = data32)
summary(data32.aov)
TukeyHSD(data32.aov)

# 4-a
data4 <- read.csv("qsar_aquatic_toxicity.csv", header = F, sep = ";")
colnames(data4) <- c("TPSA","SAacc","H-050","MLOGP","RDCHI","GATS1p","nN","C-040","LC50")
data4.lm <- lm(data4$LC50 ~ data4$TPSA+ data4$SAacc+ data4$`H-050`+ data4$MLOGP+ data4$RDCHI+ data4$GATS1p+ data4$nN+ data4$`C-040`, data =data4)
summary(data4.lm)

# 4-b
data4.lm2 <- step(data4.lm)
summary(data4.lm2)




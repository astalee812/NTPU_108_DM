getwd()
setwd("C:/Users/ASUS/Desktop/NTPU/108-2-資料探勘/DM2020/data/")

# 1-(a)
glass_data <- read.table("glass.txt", sep = ",")
colnames(glass_data) <- c('index','RI', 'Na', 'Mg', 'Al', 'Si', 'K', 'Ca', 'Ba', 'Fe','class')
# install.packages("FactoMineR")
library(FactoMineR)
# install.packages("factoextra")
library(factoextra)

# svg放到word圖形會不見...
svg("PCA.svg", width = 5, height = 5)
pca <- PCA(glass_data[,2:(ncol(glass_data)-1)])
dev.off()

# Eigenvalues / Variances
pca$eig

# scree plot
svg("scree_plot.svg", width = 7, height = 5)
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))
dev.off()

#  Correlation circle
svg("Correlation_circle.svg", width = 5, height = 5)
fviz_pca_var(pca, col.var = "cos2")
dev.off()

# scatter plot by the first 2 principle compoents
svg("scatter_plot.svg", width = 5, height = 5)
fviz_pca_ind(pca, geom.ind = "point", col.ind = glass_data[,ncol(glass_data)])
dev.off()

# 1-(b)
# mds
glass_mds <- cmdscale(dist(glass_data[,2:(ncol(glass_data)-1)]))
svg("mds.svg", width = 7, height = 5)
plot(glass_mds[,1], glass_mds[,2], xlab = "MDS-1", ylab = "MDS-2", col = glass_data[,ncol(glass_data)])
dev.off()

# isomap
# install.packages("vegan")
library(vegan)
glass_isomap <- isomap(dist(glass_data[, 2:(ncol(glass_data)-1)]), ndim = 2, k = 10)
svg("isomap.svg", width = 7, height = 5)
plot(glass_isomap, col = glass_data[,ncol(glass_data)])
dev.off()

# 2-(a)
data2 <- read.csv("dataR2.csv")
rownames(data2) <- c(1:nrow(data2))
data2.age <- data2[,1]
str(data2)
summary(data2)
dim(data2)
head(data2)

data2.1 <- as.matrix(data2[,c(2:9)])
colnames(data2.1)
str(data2.1)

# install.packages("pheatmap")
library(pheatmap)
data2.1.std <- t(apply(data2.1,2,scale))
colnames(data2.1.std) <- rownames(data2)
str(data2.1.std)
annotation_col = data.frame(Labels = factor(rep(c("1", "2"))), data2.age)
svg("pheatmap.svg", width = 10, height = 7)
pheatmap(data2.1.std, border_color = T, legend = T,fontsize_row=10,fontsize_col=4.5,
         annotation_col = annotation_col)
dev.off()

# 2-(b) 
E.dist <- dist(x = data2.1, method = "euclidean")
data2.hcl <- hclust(E.dist, method = "complete")
cut.h.cluster <- cutree(tree = data2.hcl, k = 2)
hlc <- as.character(cut.h.cluster)
cluster1 <- data.frame(hlc = hlc)
row.names(cluster1) <- colnames(data2.1.std)
annotation_col = data.frame(Labels = factor(rep(c("1", "2"))), data2.age,cluster1)
svg("pheatmap2.svg", width = 10, height = 7)
pheatmap(data2.1.std, border_color = T, legend = T,fontsize_row=10,fontsize_col=4.5,
         annotation_col = annotation_col, cutree_rows = 2,
         cutree_cols = 2)
dev.off()


# 2-(c)
clMethods <- c("hierarchical", "kmeans", "pam")
validation <- c("internal", "stability", "biological")

# install.packages("cluster")
library(clValid)
head(data2.1)
experess <- data2.1[1:116,]
rownames(experess) <- rownames(data2.1)
head(experess)

intern <- clValid(experess, 2:6, clMethods=c("hierarchical", "kmeans", "pam"),
                  validation="internal")
summary(intern)
svg("compare.svg", width = 10, height = 7)
par(mfrow=c(1, 3))
plot(intern)
dev.off()

# 3-(a)
# if (!requireNamespace("BiocManager", quietly = TRUE))
# install.packages("BiocManager")
# BiocManager::install("made4")
library(made4)

data(khan)
summary(khan)
str(khan)
summary(khan)

khan.frame<-t(data.frame(khan$train))
str(khan.frame)

bw.ratio <- function(x, y){tg <- table(y)
gm <- tapply(x, y, mean)
repm <- rep(gm, tg)
wss <- sum((x - repm)^2)
bss <- sum((gm-mean(x))^2)
bw <- bss/wss}

khan.values <- apply(khan.frame, 2, bw.ratio, khan$train.classes)
top <- 30
selected.genes <- order(khan.values, decreasing = TRUE)[1:top]
khan.selected  <- khan.frame[, selected.genes]
range(khan.selected)

khan.selected[khan.selected > 5] <- 5
khan.selected[khan.selected < -5] <- -5

heatmap(as.matrix(khan.selected), Rowv = NULL,
        RowSideColors = c("red", "blue", "green")[khan$train.classes+1], 
        margins = c(5, 10),
        xlab = "genes", ylab =  "subjects", main = "")

# 3-(b)
# install.packages("caret")
library(caret)
library(rpart)
# Define train control for k fold cross validation
khan.frame2<-data.frame(khan$train)
head(khan.frame2)
head(khan$train.classes)
train_control <- trainControl(method = "cv", number = 10)

data11 <- merge(khan.selected, khan$train.classes)
table(data11$y)

knn.model <- train(y ~., data = data11, method = "knn", trControl = train_control)
mean(knn.model$results[,2])

rpart.model <- train(y ~., data = data11, method = "rpart", trControl = train_control)
mean(rpart.model$results[,2])

rf.model <- train(y ~., data = data11, method = "rf", trControl = train_control)
mean(rf.model$results[,2])



# 4
# install.packages("arules")
library(arules)
data(AdultUCI)
head(AdultUCI)
data(Adult)
Adult
class(Adult)
str(Adult)
inspect(Adult[1:2])
summary(Adult)

# creating transactions form a list
a.list <- list(c("a","b","c"), c("a","b"), c("a","b","d"), c("c","e"), c("a","b","d","e"))
names(a.list) <- paste0("Customer", c(1:5))
a.list

alist.trans <- as(a.list, "transactions")
summary(alist.trans) # analyze transactions

a.matrix <- matrix(c( 1,1,1,0,0,
                      1,1,0,0,0,
                      1,1,0,1,0,
                      0,0,1,0,1), 
                   ncol = 5)
dimnames(a.matrix) <- list(
  paste("Customer", letters[1:4]),
  paste0("Item", c(1:5)))
a.matrix

amatirx.trans <- as(a.matrix, "transactions")
amatirx.trans

inspect(amatirx.trans)
summary(amatirx.trans)

image(alist.trans)

rule0 <- apriori(alist.trans)
rule1 <- apriori(alist.trans, parameter=list(support=0.005, confidence=0.64))
rule2 <- apriori(alist.trans, parameter=list(support=0.001, confidence=0.5))

rule1.sorted_sup <- sort(rule1, by="support")
inspect(rule1.sorted_sup[1:10])
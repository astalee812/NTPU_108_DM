getwd()
setwd("C:/Users/ASUS/Desktop/NTPU/108-2-資料探勘/DM2020/data")

# 1
data1 <- read.table("glass_214x9.txt", header = T)
str(data1)

# install.packages("FactoMineR")
# install.packages("factoextra")
# install.packages("corrplot")
library(FactoMineR)
library(factoextra)
library(corrplot)

# 要對data1做標準化，Si欄位數值相對其他欄位大
data1.pca <- PCA(data1[,2:11], scale.unit = T, graph = T)
class(data1.pca)
str(data1.pca)

# eigenvalue
data1.eig <- get_eigenvalue(data1.pca)
data1.eig

# scree.plot
data1.scree <- fviz_eig(data1.pca, addlabels = T, ylim = c(0,50))
data1.scree

# correlation circles
var <- get_pca_var(data1.pca)
var

head(var$coord)
head(var$cos2)
head(var$contrib)


# correlation circle, variable correlation plots
fviz_pca_var(data1.pca, col.var = "black")

# square consin(cos2 values) plot
# cos2 of variables on all the dimensions
corrplot(var$cos2, is.corr = F)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(data1.pca, choice = "var", axes = 1:2)
fviz_pca_var(data1.pca, col.var = "cos2",
             gradient.cols = c("blue", "yellow", "red"),
             repel = TRUE)

# contributions of variables to PCs
corrplot(var$contrib, is.corr = F)
# Contributions of variables to PC1
fviz_contrib(data1.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(data1.pca, choice = "var", axes = 2, top = 10)
# Contributions of variables to PC1 and PC2
fviz_contrib(data1.pca, choice = "var", axes = 1:2, top = 10)


# highlight the most important variables
fviz_pca_var(data1.pca, col.var = "contrib",
             gradient.cols = c("blue", "yellow", "red"))


# 2
# install.packages("coRanking") 
library(coRanking)
data2 <- read.table("wine_178x13.txt", header = TRUE)

# 2.MDS
x <- data2[,3:15]
y <- data2[,2]
data2.s <- t(scale(t(x)))
data2.corr <- cor(t(data2.s))
data2.dist <- sqrt(2*(1-data2.corr))
data2.mds<- cmdscale(data2.dist)
plot(data2.mds[,1:2], col = y+1, xlab = "MDS1", ylab = "MDS2")
Q.mds <- coranking(x, data2.mds[,1:2])
imageplot(Q.mds)
lcmc.mds <- LCMC(Q.mds, K = 7:11)
lcmc.mds

# 2.pca
data2.pca <- princomp(scale(x))
plot(data2.pca$scores[,1:2], col = y+1, xlab = "PCA1", ylab = "PCA2")
Q.pca <- coranking(x, data2.pca$scores[,1:2])
imageplot(Q.pca)
lcmc.pca <- LCMC(Q.pca, K = 7:11)
lcmc.pca

# 2.isomap
library(vegan)
data2.isomap <- isomap(dist(x), ndim=2, k=7)
Q.isomap <- coranking(x, scores(data2.isomap))
imageplot(Q.isomap)
plot(data2.isomap, col=y+1)


# 2.sir
# install.packages("dr")
# install.packages("MASS")
library(MASS)
library(dr)

data2.sir <- dr(class~v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12+v13,
                data = data2, nslices = 3, chi2approx = "wood",
                method = "sir")
summary(data2.sir)
comp.sir <- as.matrix(data2[,3:15]) %*% as.matrix(data2.sir$evectors[,1:2])
plot(comp.sir, col = data2$class+1, main = "SIR")

# 2.(c)
# install.packages("e1071")
library(e1071)
model <- svm(x, y)
pred <- predict(model, x)
accuracy <- sum(diag(table(pred, y)))/length(y)
accuracy

sum(diag(table(predict(svm(cmdscale(data2.dist,k =1:3), y),
                       cmdscale(data2.dist,k =1:3) ), y))) / length(y)


# 2.(d)
accuracy_df <- data.frame(row.names = 1:10)
for(i in 1:10){
  accuracy_df[i,"mds"] <- sum(diag(table(predict(svm(cmdscale(data2.dist,k = i), y),
                                                 cmdscale(data2.dist,k = i) ), y))) / length(y)

  accuracy_df[i,"pca"] <- sum(diag(table(predict(svm(data2.pca$scores[, 1:i], y),
                                                 data2.pca$scores[, 1:i]), y)))/length(y)
  
  accuracy_df[i,"iso"] <- sum(diag(table(predict(svm(scores(isomap(dist(x), ndim = i, k = 7)), y),
                                                    scores(isomap(dist(x), ndim = i, k = 7)), y))))/length(y)
  
  accuracy_df[i,"sir"] <- sum(diag(table(predict(svm(as.matrix(data2[, 3:15]) %*% as.matrix(data2.sir$evectors[, 1:i]), y),
                                                 as.matrix(data2[, 3:15]) %*% as.matrix(data2.sir$evectors[, 1:i])), y)))/length(y)
}

accuracy_df




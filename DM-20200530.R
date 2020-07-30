# Spectral Decomposition
X <- iris[,1:4]

# 共變異數
(S <- cov(X)) 
# 特徵值
(e <- eigen(S))
# 共變異數矩陣
D <- diag(e$values) 
C <- e$vectors

C%*%D%*%t(C)

# Eigen-decomposition
S%*%C[,1]
D[1]*C[,1]

# Singular Value Decomposition 奇異值分解
iris.sub <- iris[sample(1:150, 8),1:4]
iris.sub
M.svd <- svd(iris.sub)
M.svd

M.svd$u %*% (diag(M.svd$d) %*% t(M.svd$v))

# use the first two values to approximate
d.sub <- diag(M.svd$d[1:2])
u.sub <- as.matrix(M.svd$u[, 1:2])
v.sub <- as.matrix(M.svd$v[, 1:2])
iris.sub.approx <- u.sub %*% d.sub %*% t(v.sub)
iris.sub.approx

# compute the sum of squared errors
sum((iris.sub - iris.sub.approx)^2)

# require packages: locfit, tiff, fftwtools
# 研究一下要怎麼把EBImage這個套件
# lena.jpg這個圖也要找一下，在R的練習資料檔中

install.packages('locfit')
install.packages('tiff')
install.packages('fftwtools')

library(EBImage) # (Repositories: BioC Software)
lena <- readImage("lena.jpg")
dims <- dim(lena)
dims

plot(c(0, dims[1]), c(0, dims[2]), type='n', xlab="", ylab="")
rasterImage(lena, 0, 0, dims[1], dims[2])

source("https://bioconductor.org/biocLite.R")
biocLite("EBImage")

# 新的下載方式
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("EBImage")

# 另外一種處理影像方式
install.packages("jpeg")
library(jpeg)
lena <- readJPEG("lena.jpg")

lena.flip <- Image(flip(lena))

# convert RGB to grayscale
red.weight <- 0.2989
green.weight <- 0.587
blue.weight <- 0.114
image(lena.gray, col = grey(seq(0, 1, length = 256)))

# 16/144
lena.svd <- svd(lena.gray)
d <- diag(lena.svd$d)
dim(d)

u <- lena.svd$u
v <- lena.svd$v
plot(1:length(lena.svd$d), lena.svd$d, pch=19, xlab="i-th lena.svd$d", ylab="lena.svd$d")

used.no <- 20
u.sub <- as.matrix(u[, 1:used.no])
v.sub <- as.matrix(v[, 1:used.no])
d.sub <- as.matrix(d[1:used.no, 1:used.no])
lena.approx <- u.sub %*% d.sub %*% t(v.sub)
image(lena.approx, col = grey(seq(0, 1, length = 256)))

lena.gray <- red.weight * imageData(lena.flip)[,,1] +
  green.weight * imageData(lena.flip)[,,2] +
  blue.weight * imageData(lena.flip)[,,3]
dim(lena.gray)

lena.gray[1:5, 1:5]

# 21/144
x <- iris[, 1:4]
(covx <- cov(x))
e <- eigen(covx)
e
V <- e$vectors
V
V.inverse <- solve(e$vectors)
V.inverse
covx.hat <- V %*% diag(e$values) %*% V.inverse
# same with covx
covx.hat 

# PCA for iris data
z <- as.matrix(x) %*% e$vectors[, 1:2]
plot(z[, 1], z[, 2], col=iris[, 5])

# 31/144
m1 <- matrix(sample(1:16,16),4,4)
m1
m1.scale.svd <- svd(scale(m1))
m1.scale.svd
m1.pca <- prcomp(m1, scale=T)
m1.pca
pca2 <- princomp(m1, cor=T)
pca2$scores
pca2$loadings


# 33/144
cell.matrix <- read.table("YeastCellCycle_alpha.txt", header=TRUE, row.names=1)
n <- dim(cell.matrix)[1]
n
p <- dim(cell.matrix)[2]-1
p
cell.data <- cell.matrix[,2:p+1]
cell.data
gene.phase <- cell.matrix[,1]
phase <- unique(gene.phase)
phase.name <- c("G1", "S", "S/G2", "G2/M", "M/G1")
cell.sdata <- t(scale(t(cell.data)))
rc <- rainbow(5)[as.integer(gene.phase)]
cc <- rainbow(ncol(cell.sdata))

color.Palette <- function(low = "black",
                          high = c("green", "red"),
                          mid="black",
                          k =50)
{
  low <- col2rgb(low)/255
  high <- col2rgb(high)/255
  if(is.null(mid)){
    r <- seq(low[1], high[1], len = k)
    g <- seq(low[2], high[2], len = k)
    b <- seq(low[3], high[3], len = k)
  }
  if(!is.null(mid)){
    k2 <- round(k/2)
    mid <- col2rgb(mid)/255
    r <- c(seq(low[1], mid[1], len = k2),
           seq(mid[1], high[1], len = k2))
    g <- c(seq(low[2], mid[2], len = k2),
           seq(mid[2], high[2], len = k2))
    b <- c(seq(low[3], mid[3], len = k2),
           seq(mid[3], high[3], len = k2))
  }
  rgb(r, g, b)
}

GBRcol <- color.Palette(low="green", mid="black", high="red")


hv <- heatmap(cell.sdata, col = GBRcol, scale = "column", Colv=NA, Rowv=NA,
              RowSideColors = rc, ColSideColors = cc, margins = c(5,10),
              xlab = "Times", ylab = "Genes", main = "Heatmap of Microarray Data")

# 34/144
cell.pca <- princomp(cell.sdata, cor=TRUE, scores=TRUE)
# 2D plot for first two components
pca.dim1 <- cell.pca$scores[,1]
pca.dim2 <- cell.pca$scores[,2]

plot(pca.dim1, pca.dim2, main="PCA for Cell Cycle Data on Genes", xlab="1st PCA
Component", ylab="2nd PCA Component",col=c(phase), pch=c(phase))
legend(3, 4, phase.name, pch=c(phase), col=c(phase))

# shows a screeplot.
plot(cell.pca)
biplot(cell.pca)


# 35/144
# loadings plot
plot(loadings(cell.pca)[,1], loadings(cell.pca)[,2], xlab="1st PCA",
     ylab="2nd PCA", main="Loadings Plot", type="n")
text(loadings(cell.pca)[,1], loadings(cell.pca)[,2], labels=paste(1:p))
abline(h=0)
abline(v=0)

# print loadings
loadings(cell.pca)
summary(cell.pca)

# 36/144
library(MASS)
mu <- c(2, -1)
Sigma <- matrix(c(2.4, -0.5, -0.5, 1), 2)
n <- 250
X <- mvrnorm(n, mu, Sigma)

mycol <- terrain.colors(n)
sorted.x1 <- sort(X[,1])
order.x1 <- order(X[,1])
id <- 1:n
sorted.id <- id[order.x1]
x1.col <- mycol[order(sorted.id)]

par(mfrow=c(1, 2))
plot(X, col=x1.col, pch=16,
     main="simulated bivariate normal")
abline(h=0, v=0, col="gray")
X.pca <- princomp(X, cor = TRUE)
X.pca$sdev
biplot(cell.pca)

X.pca$loadings

plot(X.pca$scores, col=x1.col, pch=16, main="PCA")
abline(h=0, v=0, col="gray")

# 37/144
pca.pkg <- c("FactoMineR", "factoextra", "corrplot")
install.packages(pca.pkg)
lapply(pca.pkg, library, character.only=TRUE)
data(decathlon2) # 十項全能
head(decathlon2) # 100米, 跳遠, 鉛球, 跳高, 400米, 110米跨欄, 鐵餅, 撐竿跳高, 標槍, 1500米

dim(decathlon2)
x <- decathlon2[,1:10]

# 38/144
# PCA(X, scale.unit = TRUE, ncp = 5, graph = TRUE)
x.pca <- PCA(x, graph = FALSE)
class(x.pca)
str(x.pca)
print(x.pca)

# 39/144
# Eigenvalues/Variances
eig.val <- get_eigenvalue(x.pca)
eig.val

# scree plot
fviz_eig(x.pca, addlabels = TRUE, ylim = c(0, 50))

# 40/144
var <- get_pca_var(x.pca)
var
# Coordinates of variables
head(var$coord, 4)

# Correlation circle, variable correlation plots
fviz_pca_var(x.pca, col.var = "black")

# 41/144
head(var$cos2, 4)
corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(x.pca, choice = "var", axes = 1:2)
# variables with low/mid/high cos2 values will be colored in blue/yellow/red
fviz_pca_var(x.pca, col.var = "cos2",
             gradient.cols = c("blue", "yellow", "red"),
             repel = TRUE) # Avoid text overlapping

#42/144
head(var$contrib, 4)
corrplot(var$contrib, is.corr=FALSE) 
# Contributions of variables to PC1
fviz_contrib(x.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(x.pca, choice = "var", axes = 2, top = 10)
# The total contribution to PC1 and PC2:
fviz_contrib(x.pca, choice = "var", axes = 1:2, top = 10)

# 43/144
fviz_pca_var(x.pca, col.var = "contrib",
            gradient.cols = c("blue", "yellow", "red"))
set.seed(123)
var.kms <- kmeans(var$coord, centers = 3, nstart = 25)
kms.grp <- as.factor(var.kms$cluster)
# Color variables by kmeans' result
fviz_pca_var(x.pca, col.var = kms.grp, palette = c("blue", "green", "red"),
              legend.title = "Cluster")

# 44/144
ind <- get_pca_ind(x.pca)
ind
# Coordinates of individuals
head(ind$coord, 3)
# Quality of individuals
head(ind$cos2, 3)
# Contributions of individuals
head(ind$contrib, 3)

#45/144
# color individuals by their cos2 values
fviz_pca_ind(x.pca, col.ind = "cos2", gradient.cols = c("blue", "black", "red"),
            repel = TRUE)
# change the point size according the cos2 of the corresponding individuals
fviz_pca_ind(x.pca, pointsize = "cos2", pointshape = 21, fill = "lightblue",
            repel = TRUE)

# 46/144
fviz_pca_ind(x.pca, geom.ind = "point", col.ind = decathlon2[,13],
            palette = c("blue", "red"), legend.title = "Competition")

# quality of representation (cos2) of individuals on the factor map
fviz_cos2(x.pca, choice = "ind", top = 5)

# Total contribution of individuals on PC1 and PC2
fviz_contrib(x.pca, choice = "ind", axes = 1:2, top = 5)

# 71/144
cell.matrix <- read.table("YeastCellCycle_alpha.txt", header=TRUE, row.names=1)
n <- dim(cell.matrix)[1]
p <- dim(cell.matrix)[2]-1
cell.data <- cell.matrix[,2:p+1]
gene.phase <- cell.matrix[,1]
phase.name <- c("G1", "S", "S/G2", "G2/M", "M/G1")
cell.sdata <- t(scale(t(cell.data)))
cell.cor <- cor(t(cell.sdata))
cell.dist <- sqrt(2*(1-cell.cor))
cell.mds <- cmdscale(cell.dist)
plot(cell.mds[,1], cell.mds[,2], type="n", xlab="MDS-1", ylab="MDS-2", main="MDS for
Cell Cycle Data")
number <- c(1, 4, 5, 2, 3)[as.integer(gene.phase)]
phase.color <- c("green", "blue", "red", "gray", "yellow")
text(cell.mds[,1], cell.mds[,2], number, col= phase.color[number])
legend(-0.7, 1.0, phase.name, pch="01234", col=phase.color) 

# 73/144
library(stats)
no.group <- 5
no.iter <- 20
USArrests.kmeans <- kmeans(USArrests, no.group, no.iter)
plot(USArrests, col = USArrests.kmeans$cluster, main = "K-means: USArrests data")
# PCA
USArrests.pca <- princomp(USArrests, cor=TRUE, scores=TRUE)
pca.dim1 <- USArrests.pca$scores[,1]; pca.dim2 <- USArrests.pca$scores[,2]
plot(pca.dim1, pca.dim2, main="PCA for USArrests Data with K-means", xlab="PCA-1",
     ylab="PCA-2", col=USArrests.kmeans$cluster)
# MDS
USArrests.mds<- cmdscale(dist(USArrests))
mds.dim1 <- USArrests.mds[,1]; mds.dim2 <- USArrests.mds[,2]
plot(mds.dim1, mds.dim2, xlab="MDS-1", ylab="MDS-2", main="MDS for USArrests Data with Kmeans", col = USArrests.kmeans$cluster)


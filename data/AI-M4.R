# 吳漢銘 國立臺北大學統計學系
# http://www.hmwu.idv.tw
# R Software (R軟體教學/R語言程式設計) 
# http://www.hmwu.idv.tw/index.php/r-software
# Data download
# http://www.hmwu.idv.tw/web/R/data/
#

# AI-M4-hmwu_R_EDA_v2.pdf


# 26/47
open3d()
comet <- readOBJ("ESA_Rosetta_OSIRIS_67P_SHAP2P.obj")
class(comet)
str(comet)
shade3d(comet, col="gray")



# 27/47
source("https://bioconductor.org/biocLite.R")
biocLite("ComplexHeatmap")
library(ComplexHeatmap)
Heatmap(exprs(selected.eset))


# 32/47
install.packages(c("tiff", "jpeg", "png", "fftwtools"), repos="http://cran.csie.ntu.edu.tw")
library(EBImage) # (Repositories: BioC Software)
Transformers <- readImage("Transformers07.jpg")
(dims <- dim(Transformers))
Transformers
plot(c(0, dims[1]), c(0, dims[2]), type='n', + xlab="", ylab="")
rasterImage(Transformers, 0, 0, dims[1], dims[2])

source("https://bioconductor.org/biocLite.R")
biocLite("EBImage")

library(jpeg)
Transformers <- readJPEG("Transformers07.jpg")


# 33/47
TaiwanMap <- GetMap(center=c(lat = 23.58, lon =120.58), zoom =7, destfile = "Taiwan1.png")
TaiwanMap <- GetMap(center=c(lat = 23.58, lon =120.58), zoom = 10, destfile = "Taiwan2.png", maptype = "terrain")


# 34/47
my.lat <- c(25.177339, 25.082288, 25.042185, 25.046254)
my.lon <- c(121.450003, 121.565481, 121.614548, 121.517732)
bb = qbbox(my.lat, my.lon)
print(bb)
MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "my.png", maptype = "roadmap")

My.markers <- cbind.data.frame(lat = my.lat, lon = my.lon)
tmp <-  PlotOnStaticMap(MyMap, lat = My.markers[,"lat"], lon = My.markers[,"lon"], destfile = "my.png", cex=2.5, pch=20, col=1:4, add=F)


# 36/47
n <- 1e+02
y <- as.factor(sample(LETTERS[1:4], n, replace=T, prob=c(0.1, 0.1, 0.5, 0.3)))
x1  <- rnorm(n)
x2  <- rbeta(n, 0.5, 0.5)
xydata <- data.frame(y, x1, x2)
par(mfrow=c(1,4))
boxplot(x1~y, data=xydata, ylab="x1", main="boxplot")
hist(x2, xlab="x2", main="hist")
barplot(table(y), xlab="y", col = 2:5, main="barplot")
plot(x1, x2, main="plot", col=as.integer(y)+1)


# 38/47
x <- rnorm(mean=1.5, 10000)
y <- rnorm(mean=1.6, 10000)
my.data <- data.frame(x, y)

pk <- c("RColorBrewer", "hexbin", "gplots")
install.packages(pk, repos="http://cran.csie.ntu.edu.tw")
library(RColorBrewer)
col_rb <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
plot(my.data, pch=16, col='black', cex=0.5)
library(hexbin)
h <- hexbin(my.data) # create a hexbin object
h

plot(h) # in grey level
plot(h, colramp=col_rb) # rainbow color


# 40/47
install.packages("tabplot")
library(tabplot)
tableplot(iris, nBins=150, sortCol=5)


# 41/47
tableplot(iris, nBins=50, sortCol=4)


# 42/47
require(ggplot2)
data(diamonds)
dim(diamonds)
head(diamonds)
tableplot(diamonds)


# 53/47
library(vegan)
source("panelutils.R")
spe <- read.csv("DoubsSpe.csv", row.names=1)
env <- read.csv("DoubsEnv.csv", row.names=1)
spa <- read.csv("DoubsSpa.csv", row.names=1)

library(ade4)
data(doubs)
?doubs


# 54/47
spe
spe[1:5,1:10]
head(spe)
nrow(spe)	
ncol(spe)		
dim(spe)		
colnames(spe)		
rownames(spe)	
summary(spe)	


# 55/47
range(spe)
(ab <- table(unlist(spe)))
windows(title="Distribution of abundance classes")
barplot(ab, las=1, xlab="Abundance class", ylab="Frequency", col=gray(5:0/5))
sum(spe==0)
sum(spe==0)/(nrow(spe)*ncol(spe))


# 56/47
windows(title="Site Locations")
plot(spa, asp=1, type="n", main="Site Locations", xlab="x coordinate (km)", ylab="y coordinate (km)")
lines(spa, col="light blue")
text(spa, row.names(spa), cex=0.8, col="red")
text(50, 10, "Upstream", cex=1.2, col="red")
text(30, 120, "Downstream", cex=1.2, col="red")


# 58/47
windows(title="Species Locations", 9, 9)
par(mfrow=c(1,4))
xl <- "x coordinate (km)", 
yl <- "y coordinate (km)"
plot(spa, asp=1, col="brown", cex=spe$TRU, main="Brown trout", xlab=xl, ylab=yl) 
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, col="brown", cex=spe$OMB, main="Grayling", xlab=xl, ylab=yl)  
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, col="brown", cex=spe$BAR, main="Barbel", xlab=xl, ylab=yl)  
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, col="brown", cex=spe$BCO, main="Common bream", xlab=xl, ylab=yl) 
lines(spa, col="light blue", lwd=2)


# 59/47
spe.pres <- apply(spe > 0, 2, sum)
sort(spe.pres)
spe.relf <- 100*spe.pres/nrow(spe)
round(sort(spe.relf), 1)


# 60/47
windows(title="Frequency Histograms",8,5)
par(mfrow=c(1,2))
hist(spe.pres, main="Species Occurrences", right=FALSE, las=1, 
     xlab="Number of occurrences", ylab="Number of species", 
     breaks=seq(0,30,by=5), col="bisque")
hist(spe.relf, main="Species Relative Frequencies", right=FALSE, 
     las=1, xlab="Frequency of occurrences (%)", ylab="Number of species",
     breaks=seq(0, 100, by=10), col="bisque")


# 61/47
sit.pres <- apply(spe > 0, 1, sum)
sort(sit.pres)


# 62/47
windows(title="Species Richness", 10, 5)
par(mfrow=c(1,2))
plot(sit.pres,type="s", las=1, col="gray",
  main="Species Richness vs. \n Upstream-Downstream Gradient",
  xlab="Positions of sites along the river", ylab="Species richness")
text(sit.pres, row.names(spe), cex=.8, col="red")
plot(spa, asp=1, main="Map of Species Richness", pch=21, col="white", 
  bg="brown", cex=5*sit.pres/max(sit.pres), xlab="x coordinate (km)", 
  ylab="y coordinate (km)")
lines(spa, col="light blue")


# 63/47
?diversity
N0 <- rowSums(spe > 0)         
H <- diversity(spe)           
N1 <- exp(H)                   
N2 <- diversity(spe, "inv")  
J <- H/log(N0)                
E10 <- N1/N0                 
E20 <- N2/N0                 
(div <- data.frame(N0, H, N1, N2, E10, E20, J))


# 64/47
?decostand
spe[1:5, 2:4]
spe.pa <- decostand(spe, method="pa")
spe.pa[1:5, 2:4]


# 65/47
spe.scal <- decostand(spe, "max")
spe.scal[1:5,2:4]
apply(spe.scal, 2, max)
apply(spe.relsp, 2, sum)


# 66/47
spe.rel <- decostand(spe, "total")
spe.rel[1:5,2:4]
apply(spe.rel, 1, sum)
spe.norm <- decostand(spe, "normalize")
spe.norm[1:5,2:4]
norm <- function(x) sqrt(x%*%x)
apply(spe.norm, 1, norm)


# 67/47
spe.hel <- decostand(spe, "hellinger")
spe.hel[1:5,2:4]
apply(spe.hel, 1, norm)


# 68/47
spe.chi <- decostand(spe, "chi.square")
spe.chi[1:5,2:4]
spe.chi[7:9,]
spe.wis <- wisconsin(spe)
spe.wis[1:5,2:4]


# 69/47
windows(title="Loach")
par(mfrow=c(1,4))
boxplot(spe$LOC, sqrt(spe$LOC), log1p(spe$LOC), las=1, main="Simple transformation", names=c("raw data", "sqrt", "log"), col="bisque")
boxplot(spe.scal$LOC, spe.relsp$LOC, las=1, main="Standardization by species",
  names=c("max", "total"), col="lightgreen")
boxplot(spe.hel$LOC, spe.rel$LOC, spe.norm$LOC, las=1, main="Standardization by sites", names=c("Hellinger", "total", "norm"), col="lightblue")
boxplot(spe.chi$LOC, spe.wis$LOC, las=1, main="Double standardization",
 names=c("Chi-square", "Wisconsin"), col="orange")


# 71/47
windows(title="Species profiles", 9, 9)
plot(env$das, spe$TRU, type="l", col=4, main="Raw data",
 xlab="Distance from the source [km]", ylab="Raw abundance code")
lines(env$das, spe$OMB, col=3); lines(env$das, spe$BAR, col="orange")
lines(env$das, spe$BCO, col=2); lines(env$das, spe$LOC, col=1, lty="dotted")

plot(env$das, spe.scal$TRU, type="l", col=4, main="Species profiles (max)",
 xlab="Distance from the source [km]", ylab="Standardized abundance")
lines(env$das, spe.scal$OMB, col=3); lines(env$das, spe.scal$BAR, col="orange")
lines(env$das, spe.scal$BCO, col=2); lines(env$das, spe.scal$LOC, col=1, lty="dotted")

plot(env$das, spe.hel$TRU, type="l", col=4, main="Site profiles (Hellinger)",
 xlab="Distance from the source [km]", ylab="Standardized abundance")
lines(env$das, spe.hel$OMB, col=3); lines(env$das, spe.hel$BAR, col="orange")
lines(env$das, spe.hel$BCO, col=2); lines(env$das, spe.hel$LOC, col=1, lty="dotted")
 
plot(env$das, spe.chi$TRU, type="l", col=4, main="Double profiles (Chi-square)",
 xlab="Distance from the source [km]", ylab="Standardized abundance")
lines(env$das, spe.chi$OMB, col=3); lines(env$das, spe.chi$BAR, col="orange")
lines(env$das, spe.chi$BCO, col=2); lines(env$das, spe.chi$LOC, col=1, lty="dotted")
legend("topright", c("Brown trout", "Grayling", "Barbel", "Common bream", "Stone loach"), + col=c(4,3,"orange",2,1), lty=c(rep(1,4),3))


# 72/47
windows(title="Bubble maps", 9, 9)
par(mfrow=c(1,4))
plot(spa, asp=1, main="Altitude", pch=21, col="white", + bg="red", cex=5*env$alt/max(env$alt), xlab="x", ylab="y")
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, main="Discharge", pch=21, col="white", 
 bg="blue", cex=5*env$deb/max(env$deb), xlab="x", ylab="y")
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, main="Oxygen", pch=21, col="white", 
 bg="green3", cex=5*env$oxy/max(env$oxy), xlab="x", ylab="y")
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, main="Nitrate", pch=21, col="white", 
 bg="brown", cex=5*env$nit/max(env$nit), xlab="x", ylab="y")
lines(spa, col="light blue", lwd=2)


# 73/47
windows(title="Descriptor line plots")
par(mfrow=c(1,4))
plot(env$das, env$alt, type="l", xlab="Distance from the source (km)", 
 ylab="Altitude (m)", col="red", main="Altitude")
plot(env$das, env$deb, type="l", xlab="Distance from the source (km)", 
 ylab="Discharge (m3/s)", col="blue", main="Discharge")
plot(env$das, env$oxy, type="l", xlab="Distance from the source (km)", 
 ylab="Oxygen (mg/L)", col="green3", main="Oxygen")
plot(env$das, env$nit, type="l", xlab="Distance from the source (km)", 
 ylab="Nitrate (mg/L)", col="brown", main="Nitrate")


# 74/47
windows(title="Bivariate descriptor plots")
source("panelutils.R")
op <- par(mfrow=c(1,1), pty="s")
pairs(env, panel=panel.smooth, diag.panel=panel.hist, main="Bivariate Plots with Histograms and Smooth Curves")
par(op)


# 75/47
range(env$pen)
windows(title="Transformation and standardization of variable slope")
par(mfrow=c(1,4))
hist(env$pen, col="bisque", right=FALSE)
hist(log(env$pen), col="light green", right=F, main="Histogram of ln(env$pen)")
boxplot(env$pen, col="bisque", main="Boxplot of env$pen", ylab="env$pen")
boxplot(log(env$pen), col="light green", main="Boxplot of ln(env$pen)",
 ylab="log(env$pen)")


# 76/47
env.z <- decostand(env, "standardize")
apply(env.z, 2, mean)
apply(env.z, 2, sd)
env.z <- as.data.frame(scale(env))
env.z

# 78/47
head(anscombe, 3)
apply(anscombe, 2, mean)
apply(anscombe, 2, sd)
mapply(cor, anscombe[,1:4], anscombe[,5:8])
mapply(function(x, y) lm(y~x)$coefficients, anscombe[, 1:4], anscombe[, 5:8])

par(mfrow=c(2, 2))
regplot <- function(x, y){ 
  plot(y~x) 
  abline(lm(y~x), col="red")
}
mapply(regplot, anscombe[, 1:4], anscombe[, 5:8])


# 81/47
source("https://bioconductor.org/biocLite.R")
biocLite("ALL")
library(ALL)
data(ALL)
ALL
str(ALL)
dim(exprs(ALL))
exprs(ALL)[1:3, 1:5]
table(ALL$mol.biol)
eset <- ALL[, ALL$mol.biol %in% 
                c("BCR/ABL", "ALL1/AF4")]
dim(exprs(eset))
f <- factor(as.character(eset$mol.biol))
eset.p <- apply(exprs(eset), 1, function(x) t.test(x ~ f)$p.value)
selected.eset <- eset[eset.p < 0.00001, ]
dim(selected.eset)
ma.col <- colorRampPalette(c("green", "black", "red"))(200)
var.col <- ifelse(f=="ALL1/AF4", "blue", "red")
heatmap(exprs(selected.eset), col=ma.col, ColSideColors=var.col,  
          scale="row")

# 82/47
library(vcd)

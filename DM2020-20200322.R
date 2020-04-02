
install.packages("ade4")
library(ade4)
data(doubs)

##五數綜合 = 盒鬚圖
install.packages("vegan")
library(vegan)
source("panelutils.R")

# Import the data from CSV files
# Species (community) data frame (fish abundances)
spe <- read.csv("DoubsSpe.csv", row.names=1)
# Environmental data frame
env <- read.csv("DoubsEnv.csv", row.names=1)
# Spatial data frame
spa <- read.csv("DoubsSpa.csv", row.names=1)

# Display the whole data frame in the console
spe
# Display only 5 lines and 10 columns
spe[1:5,1:10]
# Display only the first few lines
head(spe)
# Number of rows (sites)
nrow(spe)

# Number of columns (species)
ncol(spe)

# Dimensions of the data frame (rows, columns)
dim(spe)

# Column labels (descriptors = species)
colnames(spe)

# Row labels (objects = sites)
rownames(spe)

# Descriptive statistics for columns
summary(spe) 

# Minimum and maximum of abundance values in the whole data set
range(spe)

# 將list結構的資料，變成非list數據，將list數據變成字串向量或者數字向量模式
(ab <- table(unlist(spe)))

windows(title="Distribution of abundance classes")

# las控制X跟Y軸方向
barplot(ab, las=1, xlab="Abundance class", ylab="Frequency", col=gray(5:0/5))
sum(spe==0)

# 稀疏矩陣 / 稀疏資料 --- 如果使用在多變量上會有問題
sum(spe==0)/(nrow(spe)*ncol(spe))

windows(title="Site Locations")

# Create an empty frame (proportional axes 1:1, with titles)
# Geographic coordinates x and y from the spa data frame
plot(spa, asp=1, type="n", main="Site Locations",xlab="x coordinate (km)", ylab="y coordinate (km)")

# (疊加)Add a blue line connecting the sites (Doubs river)
lines(spa, col="light blue")

# (疊加)Add site labels
text(spa, row.names(spa), cex=0.8, col="red")

# Add text blocks
text(50, 10, "Upstream", cex=1.2, col="red")
text(30, 120, "Downstream", cex=1.2, col="red")

# New graphic window (size 9x9 inches)
windows(title="Species Locations", 9, 9)
par(mfrow=c(1,4))

# Plot four species
xl <- "x coordinate (km)"
yl <- "y coordinate (km)"

plot(spa, asp=1, col="brown", cex=spe$TRU, main="Brown trout", xlab=xl, ylab=yl)
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, col="brown", cex=spe$OMB, main="Grayling", xlab=xl, ylab=yl)
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, col="brown", cex=spe$BAR, main="Barbel", xlab=xl, ylab=yl)
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, col="brown", cex=spe$BCO, main="Common bream", xlab=xl, ylab=yl)
lines(spa, col="light blue", lwd=2)

# Compute the number of sites where each species is present
# To sum by columns, the second argument of apply(), MARGIN, is set to 2
spe.pres <- apply(spe > 0, 2, sum)
# Sort the results in increasing order
sort(spe.pres)
# Compute percentage frequencies 計算比例
spe.relf <- 100*spe.pres/nrow(spe)
round(sort(spe.relf), 1)

# Plot the histograms
windows(title="Frequency Histograms",8,5)

# Divide the window horizontally
par(mfrow=c(1,2))
#次數長條圖
hist(spe.pres, main="Species Occurrences", right=FALSE, las=1,
       + xlab="Number of occurrences", ylab="Number of species",
       + breaks=seq(0,30,by=5), col="bisque")
#百分比長條圖
hist(spe.relf, main="Species Relative Frequencies", right=FALSE,
       + las=1, xlab="Frequency of occurrences (%)", ylab="Number of species",
       + breaks=seq(0, 100, by=10), col="bisque")

# Compute the number of species at each site
# To sum by rows, the second argument of apply(), MARGIN, is set to 1
sit.pres <- apply(spe > 0, 1, sum) #spe > 0，出現的結果為T & F，R會自動轉成1跟0
# Sort the results in increasing order
sort(sit.pres)

windows(title="Species Richness", 10, 5)
par(mfrow=c(1,2))
# Plot species richness vs. position of the sites along the river
#type = "S"，一步一步! step畫法
plot(sit.pres,type="s", las=1, col="gray",
     main="Species Richness vs. \n Upstream-Downstream Gradient",
     xlab="Positions of sites along the river", ylab="Species richness")
text(sit.pres, row.names(spe), cex=.8, col="red")

# Use geographic coordinates to plot a bubble map
# asp是比例固定
plot(spa, asp=1, main="Map of Species Richness", pch=21, col="white",
         bg="brown", cex=5*sit.pres/max(sit.pres), xlab="x coordinate (km)",
         ylab="y coordinate (km)")
lines(spa, col="light blue")


# Get help on the diversity() function 生態多樣性指標
# 這邊的轉換都是生態學的東西，不知道也沒關係
?diversity
# Species richness
N0 <- rowSums(spe > 0)
# Shannon entropy
H <- diversity(spe)
# Shannon diversity (number of abundant species)
N1 <- exp(H)
# Simpson diversity (number of dominant species)
N2 <- diversity(spe, "inv")
# Pielou evenness
J <- H/log(N0)
# Shannon evenness (Hill's ratio)
E10 <- N1/N0
# Simpson evenness (Hill's ratio)
E20 <- N2/N0
(div <- data.frame(N0, H, N1, N2, E10, E20, J))

# Get help on the decostand() function
?decostand
## Simple transformations
# Partial view of the raw data (abundance codes)
spe[1:5, 2:4]

# Transform abundances to presence-absence (1-0)
spe.pa <- decostand(spe, method="pa")
spe.pa[1:5, 2:4]

# Display the maximum by column
spe.scal <- decostand(spe, "max")
spe.scal[1:5,2:4]

# Scale abundances by dividing them by the species totals
# (relative abundance by species)
# Note: MARGIN=2 for this method
spe.relsp <- decostand(spe, "total", MARGIN=2)
spe.relsp[1:5,2:4]

# Display the sum by column
apply(spe.relsp, 2, sum)

## Site profiles: 3 methods; presence-absence or abundance data
## standardization by row
# Scale abundances by dividing them by the site totals
# (relative abundance, or relative frequencies, per site)
# (relative abundance by site)
# Note: MARGIN=1 (default value) for this method
spe.rel <- decostand(spe, "total")
spe.rel[1:5,2:4]

# Display the sum of row vectors to determine if the scaling worked properly
apply(spe.rel, 1, sum)

# Give a length of 1 to each row vector (Euclidean norm)
spe.norm <- decostand(spe, "normalize")
spe.norm[1:5,2:4]

# Verify the norm of row vectors
norm <- function(x) sqrt(x%*%x)
apply(spe.norm, 1, norm)

# Compute relative frequencies by rows (site profiles), then square root
# Compute square root of relative abundances by site
spe.hel <- decostand(spe, "hellinger")
spe.hel[1:5,2:4]

# Check the norm of row vectors
apply(spe.hel, 1, norm)

# Chi-square transformation
spe.chi <- decostand(spe, "chi.square")
spe.chi[1:5,2:4]

# Check what happened to site 8 where no species was found
spe.chi[7:9,]

# Wisconsin standardization
# Abundances are first ranged by species maxima and then by site totals
spe.wis <- wisconsin(spe)
spe.wis[1:5,2:4]


#
windows(title="Loach") # 泥鰍
par(mfrow=c(1,4))
boxplot(spe$LOC, sqrt(spe$LOC), log1p(spe$LOC), las=1, main="Simple transformation",
          names=c("raw data", "sqrt", "log"), col="bisque")
boxplot(spe.scal$LOC, spe.relsp$LOC, las=1, main="Standardization by species",
          names=c("max", "total"), col="lightgreen")
boxplot(spe.hel$LOC, spe.rel$LOC, spe.norm$LOC, las=1, main="Standardization by sites",
          names=c("Hellinger", "total", "norm"), col="lightblue")
boxplot(spe.chi$LOC, spe.wis$LOC, las=1, main="Double standardization",
          names=c("Chi-square", "Wisconsin"), col="orange")


# 71/47
windows(title="Species profiles", 9, 9)

# 1
plot(env$das, spe$TRU, type="l", col=4, main="Raw data",
        xlab="Distance from the source [km]", ylab="Raw abundance code")
lines(env$das, spe$OMB, col=3); lines(env$das, spe$BAR, col="orange")
lines(env$das, spe$BCO, col=2); lines(env$das, spe$LOC, col=1, lty="dotted")

# 2
plot(env$das, spe.scal$TRU, type="l", col=4, main="Species profiles (max)",
        xlab="Distance from the source [km]", ylab="Standardized abundance")
lines(env$das, spe.scal$OMB, col=3); lines(env$das, spe.scal$BAR, col="orange")
lines(env$das, spe.scal$BCO, col=2); lines(env$das, spe.scal$LOC, col=1, lty="dotted")

# 3
plot(env$das, spe.hel$TRU, type="l", col=4, main="Site profiles (Hellinger)",
        xlab="Distance from the source [km]", ylab="Standardized abundance")
lines(env$das, spe.hel$OMB, col=3); lines(env$das, spe.hel$BAR, col="orange")
lines(env$das, spe.hel$BCO, col=2); lines(env$das, spe.hel$LOC, col=1, lty="dotted")

# 4
plot(env$das, spe.chi$TRU, type="l", col=4, main="Double profiles (Chi-square)",
        xlab="Distance from the source [km]", ylab="Standardized abundance")
lines(env$das, spe.chi$OMB, col=3); lines(env$das, spe.chi$BAR, col="orange")
lines(env$das, spe.chi$BCO, col=2); lines(env$das, spe.chi$LOC, col=1, lty="dotted")
legend("topright", c("Brown trout", "Grayling", "Barbel", "Common bream", "Stone loach"),
        col=c(4,3,"orange",2,1), lty=c(rep(1,4),3))


#泡泡圖#
windows(title="Bubble maps", 9, 9)
par(mfrow=c(1,4))
plot(spa, asp=1, main="Altitude", pch=21, col="white",
       bg="red", cex=5*env$alt/max(env$alt), xlab="x", ylab="y")
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


# line plot
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


# Scatter Plots
windows(title="Bivariate descriptor plots")
source("panelutils.R")
op <- par(mfrow=c(1,1), pty="s")
pairs(env, panel=panel.smooth,
        diag.panel=panel.hist,
        main="Bivariate Plots with
Histograms and Smooth Curves")
par(op)



range(env$pen)

# Log-transformation of the slope variable (y = ln(x))
# Compare histograms and boxplots of raw and transformed values
windows(title="Transformation and standardization of variable slope")
par(mfrow=c(1,4))
hist(env$pen, col="bisque", right=FALSE)
hist(log(env$pen), col="light green", right=F, main="Histogram of ln(env$pen)")
boxplot(env$pen, col="bisque", main="Boxplot of env$pen", ylab="env$pen")
boxplot(log(env$pen), col="light green", main="Boxplot of ln(env$pen)",
          ylab="log(env$pen)")

# Center and scale = standardize variables (z-scores)
env.z <- decostand(env, "standardize")
apply(env.z, 2, mean)

# standard deviations = 1
apply(env.z, 2, sd)

# Same standardization using the scale() function (which returns a matrix)
env.z <- as.data.frame(scale(env))
env.z


# tidyverse -- 一個大包(資工方法的資料處理流程)
#B01-1-12
begin.experiment <- data.frame(name=c("A", "B", "C", "D", "E", "F"),
                                weights=c(270, 263, 294, 218, 305, 261))
middle.experiment <- data.frame(name=c("G", "H", "I"),
                                weights=c(169, 181, 201))
end.experiment <- data.frame(name=c("C", "D", "A", "H", "I"),
                               weights=c(107, 104, 104, 102, 100))

# merge the data for those who started and finished the experiment
# intersect -- 交集
(common <- intersect(begin.experiment$name, end.experiment$name))

# is.element -- 是不是裡面的元素
(b.at <- is.element(begin.experiment$name, common))
(e.at <- is.element(end.experiment$name, common))
experiment <- rbind(cbind(begin.experiment[b.at,], time="begin"),
                    cbind(end.experiment[e.at,], time="end"))
experiment

tapply(experiment$weights, experiment$time, mean)

# table
set.seed(12345)
grade <- as.factor(sample(c("大一", "大二", "大三", "大四"), 50, replace=T))
bloodtype <- as.factor(sample(c("A","AB","B","O"), 50, replace=T))
record <- data.frame(grade, bloodtype)
head(record)

# 類別型計算次數! 列連表--非tidydata
record.t <- table(record)
record.t 
# 轉成tidydata -- as.data.frame
as.data.frame(record.t)

# 邊際
margin.table(record.t, 1)
margin.table(record.t, 2)

# 列總和 / 欄總和
colSums(record.t)
rowSums(record.t)

# 列平均 / 欄平均
colMeans(record.t)
rowMeans(record.t)


prop.table(record.t)
prop.table(record.t, margin=1) # row margin
prop.table(record.t, margin=2) # column margin

set.seed(12345)
(x <- sample(1:10, 5, replace=T))
(y <- tabulate(x))
names(y) <- as.character(1:max(x))
y

# 15/113
# Titanic出來的資料非tidydata
Titanic
# 使用dataframe轉成tidydata
Titanic.df <- as.data.frame(Titanic)
Titanic.df 

# xtable 把dataframe轉成列連表，freq要謝前面，Sex(前面)是列位，Age(後面)是欄位
xtabs(Freq ~ Sex + Age, data = Titanic.df)
xtabs(Freq ~ Sex + Age, data = Titanic.df, 
      subset = Class %in% c("1st", "2nd")) # 選取資料條件class, A %in% B：A 是否在 B 中。 

# 16/113 (類似樞紐分析)
sale <- read.table("itemsale.csv", sep=",", header=T)
sale

attach(sale)
tb <- xtabs(Count ~ Item + Date)
rbind(cbind(tb, row.total=margin.table(tb, 1)), col.total=c(margin.table(tb, 2), sum(tb)))

detach(sale) 
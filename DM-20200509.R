#18/43 Student's t-Test #
x <- iris$Sepal.Length
y <- iris$Petal.Length
alpha <- 0.05
# vae.test變異數檢定，確認P值是不是<= alpha值
# 抓出p.value, 結果為TURE為顯著，拒絕H0，V1!=V2
(vt <- (var.test(x, y)$p.value <= alpha))
# "!"是表示不等於，var.equal = !vt，vt是Ture轉成False，加上!
t.test(x,y,var.equal = !vt) 

# 19/43#
# 前50筆資料我不要，取後面兩群的花的品種
myData <- data.frame(value = iris$Sepal.Width[-(1:50)],
                     group <- iris[-(1:50), 5])
alpha <- 0.05

# bartlett.test，False表示不拒絕H0
(bt <- bartlett.test(value ~ group, data=myData)$p.value <= alpha)

# 這些數字分長兩組做t-test
# var.equal 是表示變異數是否相同
t.test(value ~ group, data=myData, var.equal = !bt) 

# 27/43
# 要安裝套件made4(自己查)
source("https://bioconductor.org/biocLite.R")
biocLite("made4")
library(made4)
data(khan)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("made4")

library(made4)
data(khan)

# get the p-value from a anova table 
Anova.pvalues <- function(x){
  x <- unlist(x)
  SRBCT.aov.obj <- aov(x ~ khan$train.classes)
  SRBCT.aov.info <- unlist(summary(SRBCT.aov.obj))
  SRBCT.aov.info["Pr(>F)1"]
}
  
# perform anova for each gene
SRBCT.aov.p <- apply(khan$train, 1, Anova.pvalues)
  
# select the top 5 DE genes
order.p <- order(SRBCT.aov.p)
ranked.genes <- data.frame(pvalues=SRBCT.aov.p[order.p],
                           ann=khan$annotation[order.p, ])

top5.gene.row.loc <- rownames(ranked.genes[1:5, ])

# summarize the top5 genes
summary(t(khan$train[top5.gene.row.loc, ]))

# draw the side-by-side boxplot for top5 DE genes
par(mfrow=c(1, 5), mai=c(0.3, 0.4, 0.3, 0.3))

# get the location of xleft, xright, ybottom, ytop.
usr <- par("usr")

myplot <- function(gene){ 
  boxplot(unlist(khan$train[gene, ]) ~ khan$train.classes,
          ylim=c(0, 6), main=ranked.genes[gene, 4])
  text(2, usr[4]-1, labels=paste("p=", ranked.genes[gene, 1], 
                                 sep=""), col="blue")
  ranked.genes[gene,]
}



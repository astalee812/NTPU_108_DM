## ANOVA+Post hoc test
pain <- c(4, 5, 4, 3, 2, 4, 3, 4, 4, 6, 8, 4, 5,
          4, 6, 5, 8, 6, 6, 7, 6, 6, 7, 5, 6, 5, 5)
drug <- c(rep("A", 9), rep("B", 9), rep("C", 9))
migraine <- data.frame(pain, drug)
plot(pain ~ drug, data=migraine)
migraine.aov <- aov(pain ~ drug, data=migraine)
summary(migraine.aov)

## 無母數分析
kruskal.test(pain ~ drug, data=migraine)
pairwise.t.test(pain, drug, p.adjust="bonferroni") ## p.adjust P值調整向，修正錯誤

TukeyHSD(migraine.aov)

## 期望值
# 先弄一個列連表(table)
M <- as.table(rbind(c(762, 327, 468),c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),party = c("Democrat","Independent", "Republican"))
M
(res <- chisq.test(M))

# setdiff 是可以檢查兩個集合是不是相同

# 統計模型與迴歸分析
y <- rnorm(50)
x1 <- rnorm(50)
x2 <- rnorm(50)
x3 <- rnorm(50)
lm(y ~ x1 + x2)

lm(y ~ x1 - 1)

lm(y ~ x1 * x2)


y <- rnorm(50)
school <- as.factor(sample(c("a", "b", "c"), 50, replace=T))
gender <- as.factor(sample(c("f", "m"), 50, replace=T))
table(school, gender)

lm(y ~ school / gender)

lm(y ~ gender / school)

lm(y ~ x1 | x2)

# like full model
lm(y ~ x1:x2:x3)


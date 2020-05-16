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
M <- as.table(rbind(c(762, 327, 468),c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),party = c("Democrat","Independent", "Republican"))
M
(res <- chisq.test(M))

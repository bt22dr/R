# http://nlp.stanford.edu/manning/courses/ling289/logistic.pdf

cedegren <- read.table("http://nlp.stanford.edu/manning/courses/ling289/cedegren.txt", header=T)
head(cedegren);tail(cedegren)
#   sDel sNoDel cat follows class
# 1   38    120   m       C     1
# 2    0      6   v       C     1
# 3   18     90   d       C     1
# 4    7     44   a       C     1
# 5   34     40   n       C     1
# 6   30     42   m       V     1
attach(cedegren)
ced.del <- cbind(sDel, sNoDel)
ced.logr <- glm(ced.del ~ cat + follows + factor(class), family=binomial("logit"))
ced.logr <- glm(ced.del ~ cat + follows + factor(class), family=binomial)
ced.logr
summary(ced.logr)
anova(ced.logr, test="Chisq")
drop1(ced.logr, test="Chisq")
glm(ced.del ~ cat + follows + I(class == 1), family=binomial("logit"))
pchisq(232.72-198.63, 2)
glm(ced.del ~ cat + follows + I(class == 1) + I(class==3), family=binomial("logit"))
glm(ced.del ~ I(cat=="n") + I(cat=="v") + follows + factor(class), family=binomial("logit"))
pchisq(229.11-198.63, 2)

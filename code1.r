pathRoot <- "J:/HT"
setwd(pathRoot)
# Data:
exptA <- c(210,180,240,210,210)
exptB <- c(150,180,210,240,240,120,180,240,120,150)
exptC <- c(330,300,300,420,360,270,360,360,300,120)

exptdata <- stack(list(exptA=exptA, exptB=exptB,exptC=exptC))

##################
# Visualization
##################
library(ggplot2)
png(file.path(pathRoot,"boxplots.png"))
ggplot(exptdata, aes(x=ind, y=values, fill=ind)) + geom_boxplot() +
    scale_x_discrete(name ="Experiment") + scale_y_continuous(name ="survival time(min)")+
    ggtitle("Boxplot")
dev.off()

png(file.path(pathRoot,"violin_boxplots.png"))
ggplot(exptdata, aes(x=ind, y=values, fill=ind)) + geom_violin() +
    # stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
    geom_boxplot(width=0.1) +
    scale_x_discrete(name ="Experiment") + scale_y_continuous(name ="survival time(min)") +
    ggtitle("Violin-boxplot")
dev.off()
##################
# Normality test:
##################
# Shapiro-Wilk test:
exptnames <- unique(as.character(exptdata[,"ind"]))
shapiro_res <- sapply(exptnames, function(x){shapiro.test(exptdata[exptdata[,"ind"]==x,"values"])})
colnames(shapiro_res) <- exptnames
shapiro_res <- t(shapiro_res[-c(3,4),])
write.csv(shapiro_res, file.path(pathRoot,"shapiro_result.csv"))

# Combining exptA & B:
shapiro.test(c(exptA, exptB))

# Visualization:
library(car)

sapply(exptnames, function(x){png(file.path(pathRoot,paste(x,"_qqplots.png",sep="")))
                              qqPlot(exptdata[exptdata[,"ind"]==x,"values"],
                                     ylab="");
                              title(ylab = paste(x," quantiles",sep=""));
                              dev.off()})

png(file.path(pathRoot,"qqplots_A_B.png"))
qqPlot(c(exptA,exptB),ylab="exptA+exptB quantiles")
dev.off()
########################
# Equality of variance
########################
# Use Levene's test to test the equality of variances:
leveneTest(values~ind, exptdata)


########################
# t-test, ANOVA, etc
########################
# Add option to combine exptA and exptB:
ind2 <- as.character(exptdata[,"ind"])
ind2[c(1:15)] <- "exptA_B"
exptdata <- cbind(exptdata,ind2)

# Differences of mean among three experiments?
# Anova and Kruskal-Wallis:
kruskal.test(values~ind, data=exptdata) #non parametric
Anova(lm(values~ind, data=exptdata)) #parametric

# Differences of mean between two experiments?
# Compare expt A & exptB:
wilcox.test(values~ind, data=exptdata[c(1:15),]) #non parametric
t.test(values~ind, data=exptdata[c(1:15),]) #parametric

# Compare expt B & exptC:
wilcox.test(values~ind, data=exptdata[c(6:25),])
t.test(values~ind, data=exptdata[c(6:25),])

# Compare expt A & exptC:
wilcox.test(values~ind, data=exptdata[c(1:5,16:25),])
t.test(values~ind, data=exptdata[c(1:5,16:25),])

# Compare group[exptA+B] and [exptC]
wilcox.test(values~ind2, data=exptdata)
t.test(values~ind2, data=exptdata)


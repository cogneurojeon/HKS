#1) Reshaping the data -----------------------------------------------
#After setting directory for a new R project, import totaldata.csv. For now, we need only Subject, Condition, RT for each words, and length for each words."BOM" should be removed. If not, wierd character is added at the first row.
#If you run it in Windows, you should have runned R or Rstudio as an administrator. Also, do not forget to set working directory.
data = read.csv("pseudototaldata.csv", header = TRUE, fileEncoding = "UTF-8-BOM")
RT <- data[c("Subject","Condition","word1","word2","word3","word4","word5","word6")]
install.packages("reshape2")
library(reshape2)


#Then melt them for RT using melt() function.
RTlong <- melt(RT[,c("Subject", "Condition", "word1","word2","word3","word4","word5","word6")], id=c("Subject", "Condition"), variable.name = "Item", value.name ="RT")

#calculating mean RT by Item(word1 ~ word6) or Subejct
subjectmeanRT <- dcast(RTlong, Subject~Item, mean, value.var= "RT")
conditionmeans <- dcast(RTlong, Condition~Item, mean, value.var= "RT")

#2) Removing outliers -------------------------------------------------
b <- RTlong[RTlong$RT>100 & RTlong$RT<3500, ]
install.packages("plyr")
library(plyr)
RTtrimmed <- ddply(b, .(Subject, Condition, Item), function(z){
  idx <- abs(z$RT-mean(z$RT)) < 3*sd(z$RT)
  z[idx, ]})

#3) Drawing graphs ----------------------------------------------------
install.packages("Hmisc")
library(Hmisc)
install.packages("Rmisc")
library(Rmisc)
install.packages("ggplot2")
library(ggplot2)

Condition1 <- RTtrimmed[RTtrimmed$Condition==1, c("Subject","Item","RT")]
Condition1$Subject <- factor(Condition1$Subject) 
Condition1 <- summarySEwithin(Condition1,measurevar="RT", withinvars="Item", idvar="Subject", na.rm=FALSE, conf.interval=.95)
Condition2 <- RTtrimmed[RTtrimmed$Condition==2, c("Subject","Item","RT")]
Condition2$Subject <- factor(Condition2$Subject)
Condition2 <- summarySEwithin(Condition2,measurevar="RT", withinvars="Item", idvar="Subject", na.rm=FALSE, conf.interval=.95)
Condition3 <- RTtrimmed[RTtrimmed$Condition==3, c("Subject","Item","RT")]
Condition3$Subject <- factor(Condition3$Subject)
Condition3 <- summarySEwithin(Condition3,measurevar="RT", withinvars="Item", idvar="Subject", na.rm=FALSE, conf.interval=.95)
Condition4 <- RTtrimmed[RTtrimmed$Condition==4, c("Subject","Item","RT")]
Condition4$Subject <- factor(Condition4$Subject)
Condition4 <- summarySEwithin(Condition4,measurevar="RT", withinvars="Item", idvar="Subject", na.rm=FALSE, conf.interval=.95)

Condition1$Condition <- rep(1)
Condition2$Condition <- rep(2)
Condition3$Condition <- rep(3)
Condition4$Condition <- rep(4)
Conditiontotal <- rbind(Condition1, Condition2, Condition3, Condition4)

Conditiontotal$RC[Conditiontotal$Condition==1 | Conditiontotal$Condition==3] <- "RC"
Conditiontotal$RC[Conditiontotal$Condition==2 | Conditiontotal$Condition==4] <- "NonRC"
Conditiontotal$CE[Conditiontotal$Condition==3 | Conditiontotal$Condition==4] <- "CE"
Conditiontotal$CE[Conditiontotal$Condition==1 | Conditiontotal$Condition==2] <- "NonCE"
Conditiontotal$Condition <- factor(Conditiontotal$Condition)
Conditiontotal$CE <- factor(Conditiontotal$CE)
Conditiontotal$RC <- factor(Conditiontotal$RC)



#using ggplot()
exp2result1 <- ggplot(Conditiontotal, aes(x=Item, y=RT, group=Condition)) + geom_line(size=1,aes(colour=CE)) + 
  geom_errorbar(width=.1, aes(ymin=RT-ci, ymax=RT+ci)) + geom_point(size=3, fill="white",stroke=1.3,aes(shape=RC,color=CE)) +
  ylim(600, 1200) + scale_colour_manual(values=c("coral","deepskyblue")) + scale_shape_manual(values=c(21,24)) +
  theme(panel.grid.major=element_line(colour="Grey", linetype="dotted"), panel.grid.minor=element_blank(), panel.background=element_rect(fill="transparent",colour=NA), axis.line = element_line(colour="Black"), legend.background = element_blank()) + 
  labs(x = "Word Position", y= "Reading Time (ms)", shape = "Relative Clause(RC)", color="Center-Embedding(CE)")  
exp2result1


#saving plot as .tiff file
ggsave("exp2result1.tif", plot = exp2result1, device = "tiff",
       scale = 1, width = 26.4, height = 15.64, units = "cm", dpi = 300, limitsize = TRUE)


#drawing plots comparing only two Conditions
Condition1Condition2 <- rbind(Condition1,Condition2)
Condition1Condition2$Condition <- factor(Condition1Condition2$Condition)
plot12 <- ggplot(Condition1Condition2, aes(x=Item, y=RT, group=Condition)) + geom_line(aes(colour=Condition)) + geom_errorbar(width=.1, aes(ymin=RT-ci, ymax=RT+ci)) + geom_point(shape=21, size=2, fill="Black", aes(colour=Condition)) + ylim(600, 1200) + scale_colour_manual(values=c("#E73F1E", "#FFD216"), labels=c("RC/NonCE", "NonRC/NonCE")) + theme(panel.grid.major=element_line(colour="Grey", linetype="dotted"), panel.grid.minor=element_blank(), panel.background=element_rect(fill="transparent", colour=NA), axis.line=element_line(colour="Black")) + labs(x = "Word Position", y = "Reading Time (ms)")
ggsave("pseudoCon1Con2.tif", plot = plot12, device = "tiff",
       scale = 1, width = 26.4, height = 15.64, units = "cm", dpi = 300, limitsize = TRUE)


Condition3Condition4 <- rbind(Condition3,Condition4)
Condition3Condition4$Condition <- factor(Condition3Condition4$Condition)
plot34 <- ggplot(Condition3Condition4, aes(x=Item, y=RT, group=Condition)) + geom_line(aes(colour=Condition)) + geom_errorbar(width=.1, aes(ymin=RT-ci, ymax=RT+ci)) + geom_point(shape=21, size=2, fill="Black", aes(colour=Condition)) + ylim(600, 1200) + scale_colour_manual(values=c("#047F36", "#015DAB"), labels=c("RC/CE", "NonRC/CE")) + theme(panel.grid.major=element_line(colour="Grey", linetype="dotted"), panel.grid.minor=element_blank(), panel.background=element_rect(fill="transparent", colour=NA), axis.line=element_line(colour="Black")) + labs(x = "Word Position", y = "Reading Time (ms)")
ggsave("pseudoCon3Con4.tif", plot = plot34, device = "tiff",
       scale = 1, width = 26.4, height = 15.64, units = "cm", dpi = 300, limitsize = TRUE)


#To draw graphs for the main effect of CE, we should rearrange the x variable as Constituents and not Word positions.
Condition1$Item <- as.character(Condition1$Item)
Condition1$Item[Condition1$Item == "word1"] <- "SS"
Condition1$Item[Condition1$Item == "word2"] <- "SO"
Condition1$Item[Condition1$Item == "word3"] <- "SV"
Condition1$Item[Condition1$Item == "word4"] <- "MS"
Condition1$Item[Condition1$Item == "word5"] <- "MO"
Condition1$Item[Condition1$Item == "word6"] <- "MV"
Condition1$Item <- as.factor(Condition1$Item)
Condition2$Item <- as.character(Condition2$Item)
Condition2$Item[Condition2$Item == "word1"] <- "SS"
Condition2$Item[Condition2$Item == "word2"] <- "SO"
Condition2$Item[Condition2$Item == "word3"] <- "SV"
Condition2$Item[Condition2$Item == "word4"] <- "MS"
Condition2$Item[Condition2$Item == "word5"] <- "MO"
Condition2$Item[Condition2$Item == "word6"] <- "MV"
Condition2$Item <- as.factor(Condition2$Item)
Condition3$Item <- as.character(Condition3$Item)
Condition3$Item[Condition3$Item == "word1"] <- "MS"
Condition3$Item[Condition3$Item == "word2"] <- "SS"
Condition3$Item[Condition3$Item == "word3"] <- "SO"
Condition3$Item[Condition3$Item == "word4"] <- "SV"
Condition3$Item[Condition3$Item == "word5"] <- "MO"
Condition3$Item[Condition3$Item == "word6"] <- "MV"
Condition3$Item <- as.factor(Condition3$Item)
Condition4$Item <- as.character(Condition4$Item)
Condition4$Item[Condition4$Item == "word1"] <- "MS"
Condition4$Item[Condition4$Item == "word2"] <- "SS"
Condition4$Item[Condition4$Item == "word3"] <- "SO"
Condition4$Item[Condition4$Item == "word4"] <- "SV"
Condition4$Item[Condition4$Item == "word5"] <- "MO"
Condition4$Item[Condition4$Item == "word6"] <- "MV"
Condition4$Item <- as.factor(Condition4$Item)


Condition1Condition3 <- rbind(Condition1,Condition3)
Condition1Condition3$Item <- factor(Condition1Condition3$Item, c("SS","SO","SV","MS","MO","MV")) #This is to reorder the x varaibles.
Condition1Condition3$Condition <- factor(Condition1Condition3$Condition)
plot13 <- ggplot(Condition1Condition3, aes(x=Item, y=RT, group=Condition)) + geom_line(aes(colour=Condition)) + geom_errorbar(width=.1, aes(ymin=RT-ci, ymax=RT+ci)) + geom_point(shape=21, size=2, fill="Black", aes(colour=Condition)) + ylim(600, 1200) + scale_colour_manual(values=c("#E73F1E", "#047F36"), labels=c("RC/NonCE", "RC/CE")) + theme(panel.grid.major=element_line(colour="Grey", linetype="dotted"), panel.grid.minor=element_blank(), panel.background=element_rect(fill="transparent", colour=NA), axis.line=element_line(colour="Black")) + labs(x = "Constituents", y = "Reading Time (ms)")
ggsave("pseudoCon1Con3.tif", plot = plot13, device = "tiff",
       scale = 1, width = 13.2, height = 7.82, units = "cm", dpi = 300, limitsize = TRUE)

Condition2Condition4 <- rbind(Condition2,Condition4)
Condition2Condition4$Item <- factor(Condition2Condition4$Item, c("SS","SO","SV","MS","MO","MV")) #This is to reorder the x varaibles.
Condition2Condition4$Condition <- factor(Condition2Condition4$Condition)
plot24 <- ggplot(Condition2Condition4, aes(x=Item, y=RT, group=Condition)) + geom_line(aes(colour=Condition)) + geom_errorbar(width=.1, aes(ymin=RT-ci, ymax=RT+ci)) + geom_point(shape=21, size=2, fill="Black", aes(colour=Condition)) + ylim(600, 1200) + scale_colour_manual(values=c("#FFD216", "#015DAB"), labels=c("NonRC/NonCE", "NonRC/CE")) + theme(panel.grid.major=element_line(colour="Grey", linetype="dotted"), panel.grid.minor=element_blank(), panel.background=element_rect(fill="transparent", colour=NA), axis.line=element_line(colour="Black")) + labs(x = "Constituents", y = "Reading Time (ms)")
ggsave("pseudoCon2Con4.tif", plot = plot24, device = "tiff",
       scale = 1, width = 13.2, height = 7.82, units = "cm", dpi = 300, limitsize = TRUE)



#4)Statistical analyses -------------------------------------------------------
#Before we apply mixed effect analysis, we can investigate how individual reaction pattern vary. 

subjectvariance <- ddply(RTtrimmed, .(Subject, Condition, Item), summarise, value=mean(RT))
subjectvariance$Condition <- factor(subjectvariance$Condition)
plotbysubject <- ggplot(RTtrimmed, aes(x=Item, y=value, group=Condition)) + geom_line(data=subjectvariance, aes(colour=Condition)) + theme(axis.text.x=element_blank())+ facet_wrap(~Subject, nrow=5) + ylim(500, 1700) + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), panel.background=element_rect(fill="transparent", colour=NA)) + xlab("Word") + ylab("Reading Time (ms)") + labs(color = "Conditions")
plotbysubject
ggsave("pseudosubjectdiff.tif", plot = plotbysubject, device = "tiff",
       scale = 1, width = 26.4, height = 15.64, units = "cm", dpi = 300, limitsize = TRUE)


#we reorganize the data with No. variable before we do the analysis
RT$No. <- (1:2424)
RTlong <- melt(RT[,c("Subject", "Condition", "word1","word2","word3","word4","word5","word6","No.")], id=c("Subject", "Condition","No."), variable.name = "Item", value.name ="RT")
RTlong <- RTlong[RTlong$RT>100 & RTlong$RT<3500, ]
RTtrimmed <- ddply(RTlong, .(Subject, Condition, Item), function(z){
  idx <- abs(z$RT-mean(z$RT)) < 3*sd(z$RT)
  z[idx, ]})
RTtrimmed <- dcast(RTtrimmed, Subject + Condition + No. ~ Item, value.var="RT")


RTtrimmed$RC <- rep("RC")
RTtrimmed$CE <- rep("CE")
RTtrimmed <- within(RTtrimmed, RC[Condition==2 | Condition==4] <- "NonRC")
RTtrimmed <- within(RTtrimmed, CE[Condition==1 | Condition==2] <- "NonCE")

#Now we install packages for statistical analysis

install.packages("lme4")
install.packages("lmerTest")
library(lme4)
library(lmerTest)
options(contrasts = c("contr.sum","contr.poly")) #This code changes the global contrasts in R, and eventually prevent discrepency between lmerTest::anova() and summary(). For further details: https://stats.stackexchange.com/questions/249884/conflicting-results-of-summary-and-anova-for-a-mixed-model-with-interactions 

#Now we go for 2X2 mixed effect linear regression analyses first
twobytwo1 <- lmer(word1 ~ RC + CE + RC*CE + (1|Subject), RTtrimmed)
twobytwo2 <- lmer(word2 ~ RC + CE + RC*CE + word1 + (1|Subject), RTtrimmed)
twobytwo3 <- lmer(word3 ~ RC + CE + RC*CE + word1 + word2 + (1|Subject), RTtrimmed)
twobytwo4 <- lmer(word4 ~ RC + CE + RC*CE + word1 + word2 + word3 + (1|Subject), RTtrimmed)
twobytwo5 <- lmer(word5 ~ RC + CE + RC*CE + word1 + word2 + word3 + word4 + (1|Subject), RTtrimmed)
twobytwo6 <- lmer(word6 ~ RC + CE + RC*CE + word1 + word2 + word3 + word4 + word5 + (1|Subject), RTtrimmed)
summary(twobytwo1)
summary(twobytwo2)
summary(twobytwo3)
summary(twobytwo4)
summary(twobytwo5)
summary(twobytwo6)

#Below is for paired data analyses (splitting 2 by 2 lmer into many single comparisons)
#We divide them to four different dataframes because we will make 4 different comparisons

RC1 <- subset(RTtrimmed, Condition == 1 | Condition == 2)
RC2 <- subset(RTtrimmed, Condition == 3 | Condition == 4)
CE1 <- subset(RTtrimmed, Condition == 1 | Condition == 3)
CE2 <- subset(RTtrimmed, Condition == 2 | Condition == 4)

#Now we install packages for statistical analysis

install.packages("lme4")
library(lme4)
#Only word3 and word4 includes fixed effect wl3 and wl4 because all other word length is equal.
RC1word1 <- lmer(word1 ~ RC + (1|Subject), RC1) 
RC1word2 <- lmer(word2 ~ RC + word1 + (1|Subject), RC1)
RC1word3 <- lmer(word3 ~ RC + word1 + word2 + (1 + RC|Subject), RC1)
RC1word4 <- lmer(word4 ~ RC + word1 + word2 + word3 + (1 + RC|Subject), RC1)
RC1word5 <- lmer(word5 ~ RC + word1 + word2 + word3 + word4 + (1 + RC|Subject), RC1)
RC1word6 <- lmer(word6 ~ RC + word1 + word2 + word3 + word4 + word5 + (1 + RC|Subject), RC1)

RC2word1 <- lmer(word1 ~ RC + (1|Subject), RC2)
RC2word2 <- lmer(word2 ~ RC + word1 + (1|Subject), RC2)
RC2word3 <- lmer(word3 ~ RC + word1 + word2 + (1|Subject), RC2)
RC2word4 <- lmer(word4 ~ RC + word1 + word2 + word3 + (1 + RC|Subject), RC2)
RC2word5 <- lmer(word5 ~ RC + word1 + word2 + word3 + word4 + (1 + RC|Subject), RC2)
RC2word6 <- lmer(word6 ~ RC + word1 + word2 + word3 + word4 + word5 + (1 + RC|Subject), RC2)
#for t-values, use function summary(RC1word1)


#Now for the effect of CE, we change the labels as we did to draw graphs
d <- subset(CE1, Condition==1)
colnames(d) <- c("Subject", "Condition","No.",  "SS", "SO", "SV", "MS", "MO", "MV","RC","CE")
e <- subset(CE1, Condition==3)
colnames(e) <- c("Subject", "Condition","No.",  "MS", "SS", "SO", "SV", "MO", "MV","RC","CE")
CE1 <- rbind(d,e)

f <- subset(CE2, Condition==2)
colnames(f) <- c("Subject", "Condition","No.",  "SS", "SO", "SV", "MS", "MO", "MV","RC","CE")
g <- subset(CE2, Condition==4)
colnames(g) <- c("Subject", "Condition","No.",  "MS", "SS", "SO", "SV", "MO", "MV","RC","CE")
CE2 <- rbind(f,g)

CE1SS <- lmer(SS ~ CE + (1 + CE|Subject), CE1)
CE1SO <- lmer(SO ~ CE + SS + (1 + CE|Subject), CE1)
CE1SV <- lmer(SV ~ CE + SS + SO + (1 + CE|Subject), CE1)
CE1MS <- lmer(MS ~ CE + (1 + CE|Subject), CE1)
CE1MO <- lmer(MO ~ CE + SS + SO + SV + MS + (1 + CE|Subject), CE1)
CE1MV <- lmer(MV ~ CE + SS + SO + SV + MS + MO + (1 + CE|Subject), CE1)


CE2SS <- lmer(SS ~ CE + (1 + CE|Subject), CE2)
CE2SO <- lmer(SO ~ CE + SS + (1 + CE|Subject), CE2) 
CE2SV <- lmer(SV ~ CE + SS + SO + (1 + CE|Subject), CE2)
CE2MS <- lmer(MS ~ CE + (1 + CE|Subject), CE2)
CE2MO <- lmer(MO ~ CE + SS + SO + SV + MS + (1 + CE|Subject), CE2)
CE2MV <- lmer(MV ~ CE + SS + SO + SV + MS + MO + (1 + CE|Subject), CE2)

#Now analysis for probe question data
questiondatap = read.csv("questiondatap.csv", header = TRUE, fileEncoding = "UTF-8-BOM")
questiondata$Condition <- factor(questiondata$Condition)

#Drawing barplots for questiondata

ACCplot <- ggplot(questiondata, aes(x=Condition, y=probeACC, group=Condition)) + 
  geom_bar(stat='summary',fun.y='mean', width=0.5, fill = "deepskyblue") +
  theme(panel.grid.major=element_line(colour="Grey", linetype="dotted"), panel.grid.minor=element_blank(), panel.background=element_rect(fill="transparent",colour=NA), axis.line = element_line(colour="Black"), legend.background = element_blank()) + 
  labs(x = "Conditions", y= "Accuracy") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width=0.2) +
  scale_x_discrete(labels=c("NonCE/RC","NonCE/NonRC","CE/RC","CE/NonRC"), position="bottom")
ACCplot

ggsave("exp2ACCplot.tif", plot = ACCplot, device = "tiff",
       scale = 1, width = 26.4, height = 15.64, units = "cm", dpi = 300, limitsize = TRUE)


#Using lmer() to do logistic mixed-effect analysis
accpseudo <- lmer(probeACC ~ RC + CE + RC*CE + ( 1 + RC + CE + RC*CE|Subject), questiondatap)
summary(accpseudo)

#Now traditional 2X2 RM ANOVA

aovaccpseudo <- with(questiondatap, aov(probeACC ~ RC + CE + RC * CE + Error(Subject / (RC * CE))))
summary(aovacc)  


#RT  of probe questions (repeated measure two way ANOVA)
probeRTpseudo <- questiondatap[c("Subject","Condition","RC", "CE", "probeRT")]
probeRTpseudo <- probeRTpseudo[probeRTpseudo$probeRT!=0, ] # You only use correct ones for RT analysis.

probeRTpseudo$Condition <- factor(probeRTpseudo$Condition)
RTplot <- ggplot(probeRTpseudo, aes(x=Condition, y=probeRT, group=Condition)) + 
  geom_boxplot(size=1) +
  stat_summary(fun.y=mean, geom="point", shape=16, size=4) +
  theme(panel.grid.major=element_line(colour="Grey", linetype="dotted"), panel.grid.minor=element_blank(), panel.background=element_rect(fill="transparent",colour=NA), axis.line = element_line(colour="Black"), legend.background = element_blank()) + 
  labs(x = "Conditions", y= "Reaction Time (ms)") +
  scale_x_discrete(labels=c("NonCE/RC","NonCE/NonRC","CE/RC","CE/NonRC"), position="bottom")
RTplot

ggsave("exp2RTplot.tif", plot = RTplot, device = "tiff",
       scale = 1, width = 26.4, height = 15.64, units = "cm", dpi = 300, limitsize = TRUE)



#First, with lmer.
pseudopRT1 <- lmer(probeRT ~ RC + CE + RC*CE + (1 + RC + CE + RC*CE|Subject), data=probeRTpseudo)
pseudopRT1 <- lmer(probeRT ~ RC + CE + RC*CE + (1 + RC*CE|Subject), data=probeRTpseudo)
# Fails. So we go for more simple models.
pseudopRT2 <- lmer(probeRT ~ RC + CE + RC*CE + (1 + RC + CE|Subject), data=probeRTpseudo)

summary(pseudopRT2)

2*(1-pnorm(abs((fixef(pseudopRT2)/sqrt(diag(vcov(pseudopRT2))))[2])))
2*(1-pnorm(abs((fixef(pseudopRT2)/sqrt(diag(vcov(pseudopRT2))))[3])))
2*(1-pnorm(abs((fixef(pseudopRT2)/sqrt(diag(vcov(pseudopRT2))))[4])))


#Now traditional 2X2 RM ANOVA

aovRTpseudo <- with(probeRTpseudo, aov(probeRT ~ RC + CE + RC * CE + Error(Subject / (RC * CE))))
summary(aovRTpseudo) 




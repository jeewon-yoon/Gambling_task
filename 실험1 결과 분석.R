library(tidyverse)
library(brms)

setwd("C:/Users/elley/Desktop/MS/SGT_exp/exp_1_SGT_Seg")
getwd()

#load  data
SGTData <- read.csv("CombinedSGTSegDataNewTrialsOnly100T.csv", header = TRUE)
head(SGTData, 5)

#get RT in seconds, rather than milliseconds
SGTData$RT <- SGTData$React/1000


#change condition named
SGTData$Condition <- ifelse(SGTData$Condition=="SEG","Segmented","Original")
SGTData$Condition <- as.factor(SGTData$Condition)
SGTData$Ethnicity <- as.factor(SGTData$Ethnicity)
SGTData$Race <- as.factor(SGTData$Race)
levels(SGTData$Condition)
levels(SGTData$Ethnicity)
levels(SGTData$Race)

SGTData$Subnum <- ifelse(SGTData$Condition=="Segmented",SGTData$Subnum+200,
                         SGTData$Subnum)
head(SGTData, 5)

# Proportion Optimal Analysis
# Group by condition, subnum, block
SummaryPropOptimal <- SGTData %>%
  group_by(Condition,Subnum,Block) %>%
  summarise(PropOptimal = mean(OptimChoice))
SummaryPropOptimal


# Regroup by condition and block -> We get to have all subjects' average 
GroupedPropOptimal <- SummaryPropOptimal %>%
  group_by(Condition,Block) %>%
  summarise(ProportionOptimal = mean(PropOptimal),
            sd_PR = sd(PropOptimal),
            n_PR =n(),
            se_PE = sd(PropOptimal)/sqrt(n()))
GroupedPropOptimal

# ggplot of GroupedPropOptimal 
PlotPropOptimal<- ggplot(GroupedPropOptimal, aes(x=Block,y=ProportionOptimal,color=Condition)) + geom_line() + geom_errorbar(aes(ymin=ProportionOptimal-se_PE, ymax=ProportionOptimal+se_PE), width=.1, position=position_dodge(0.00)) + ggtitle("Optimal Choices By Condition") + ylab("Proportion Optimal") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
PlotPropOptimal <- PlotPropOptimal + scale_color_manual(values=c("blue","red"))
PlotPropOptimal


# SummaryPropOptimal is a summary of every subjects and averaged over all 4 blocks
# Proportion Optimal Overall - for BayesFactor test in JASP
SummaryPropOptimal <- SGTData %>%
  group_by(Condition,Subnum) %>%
  summarise(PropOptimal = mean(OptimChoice))
SummaryPropOptimal

OverallAccuracy <- SummaryPropOptimal$PropOptimal
FTrialData <- subset(SGTData,Trial==1)
PropOptMat <- cbind(FTrialData,OverallAccuracy)
write.csv(PropOptMat,file = "AccuracyDataSGTSeg.csv")

#histogram of overallaccuracy (each subjects + block average)
plothist <- ggplot(PropOptMat, aes(x=OverallAccuracy, color=Condition)) + geom_histogram(fill="white", alpha=0.5, position="identity")
plothist

#Dividing plothist2
plothist2 <- ggplot(PropOptMat, aes(x=OverallAccuracy)) + geom_histogram(color="green",binwidth = .04) + theme_bw() + xlab("Overall Accuracy") + facet_wrap(~Condition)
plothist2


# Bayesian analysis
# First, checking interaction analysis
Mbrm1 <- brm(OptimChoice ~ Condition*Block + (1|Subnum),
             data = SGTData, family = bernoulli)
print(summary(Mbrm1),digits=4)

# Second, checking each condition and block 
brm2 <- brm(OptimChoice ~ Condition + Block + (1|Subnum),
             data = SGTData,family = bernoulli)
print(summary(Mbrm2),digits=4)
conditional_effects(Mbrm1)


head(SGTData)


# Here we redefine the column names to interpret them easily
SGTData$choice <- SGTData$keyResponse
SGTData$outcome <- SGTData$Reward
SGTData$AdvChoice <- SGTData$OptimChoice


### summary with gender

SummaryPropOptimalS <- SGTData %>%
  group_by(Condition,Sex,Subnum,Block) %>%
  summarise(PropOptimal = mean(OptimChoice))
SummaryPropOptimalS

GroupedPropOptimalS <- SummaryPropOptimalS %>%
  group_by(Condition,Sex,Block) %>%
  summarise(ProportionOptimal = mean(PropOptimal),
            sd_PR = sd(PropOptimal),
            n_PR =n(),
            se_PE = sd(PropOptimal)/sqrt(n()))
GroupedPropOptimalS

GroupedPropOptimalS <- as.data.frame(GroupedPropOptimalS)
head(GroupedPropOptimalS)

#remove person who selected other in the segmented condition
GroupedPropOptimalS <- subset(GroupedPropOptimalS,Sex!="Other")


PlotPropOptimalS<- ggplot(GroupedPropOptimalS, aes(x=Block,y=ProportionOptimal,group=Condition,color=Condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=ProportionOptimal-se_PE, ymax=ProportionOptimal+se_PE), width=.1,
                position=position_dodge(0.00)) +
  ggtitle("Optimal Choices By Condition and Sex") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Proportion Optimal") +
  facet_wrap(~Sex)
PlotPropOptimalS <- PlotPropOptimalS + scale_color_manual(values=c("blue","red"))
PlotPropOptimalS

#collapsed across trials
GroupedPropOptimalS <- SummaryPropOptimalS %>%
  group_by(Condition,Sex) %>%
  summarise(ProportionOptimal = mean(PropOptimal),
            sd_PR = sd(PropOptimal),
            n_PR =n(),
            se_PE = sd(PropOptimal)/sqrt(n()))
GroupedPropOptimalS

SGTDataS <- subset(SGTData,Sex!="Other")

head(SGTDataS)
SummaryPropOptimalS2 <- SGTDataS %>%
  group_by(Subnum,Condition,Sex) %>%
  summarise(PropOptimal = mean(OptimChoice))
SummaryPropOptimalS2

#model using sex as a predictor
MbrmS <- brm(OptimChoice ~ Condition*Sex + (1|Subnum),
             data = SGTDataS,family = bernoulli)
print(summary(MbrmS),digits=4)
conditional_effects(MbrmS)


#model using sex as a predictor
MbrmS2 <- brm(OptimChoice ~ Condition+Sex + (1|Subnum),
             data = SGTDataS,family = bernoulli)
print(summary(MbrmS2),digits=4)
conditional_effects(MbrmS2)


head(SGTData)
##analysis by Race/Ethnicity
SummaryPropOptimalR <- SGTData %>%
  group_by(Condition,Race,Ethnicity,Subnum,Block) %>%
  summarise(PropOptimal = mean(OptimChoice))
SummaryPropOptimalR

GroupedPropOptimalR <- SummaryPropOptimalR %>%
  group_by(Condition,Race,Block) %>%
  summarise(ProportionOptimal = mean(PropOptimal),
            sd_PR = sd(PropOptimal),
            n_PR =n(),
            se_PE = sd(PropOptimal)/sqrt(n()))
GroupedPropOptimalR

GroupedPropOptimalR <- as.data.frame(GroupedPropOptimalR)
head(GroupedPropOptimalR)

PlotPropOptimalR<- ggplot(GroupedPropOptimalR, aes(x=Block,y=ProportionOptimal,group=Condition,color=Condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=ProportionOptimal-se_PE, ymax=ProportionOptimal+se_PE), width=.1,
                position=position_dodge(0.00)) +
  ggtitle("Optimal Choices By Condition and Sex") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Proportion Optimal") +
  facet_wrap(~Race)
PlotPropOptimalR <- PlotPropOptimalR + scale_color_manual(values=c("blue","red"))
PlotPropOptimalR



#stay switch analysis
## First, make empty cells as many as number of rows
PrevOutcome <- rep(0,nrow(SGTData))
StaySwitch <- rep(-1,nrow(SGTData))

## Make a for loop for going through every row. 
## The trial starts from the 2nd row: 
for (i in 1:nrow(SGTData)) {
  if (SGTData$Trial[i]>1) {
    PrevOutcome[i] <- SGTData$outcome[i-1]
    StaySwitch[i] <- ifelse(SGTData$choice[i]==SGTData$choice[i-1],1,0)
  }
}

SGTData$PreviousOutcome <- PrevOutcome
SGTData$StayProbability <- StaySwitch # should be negative 1 for trial 1
SGTData$StaySwitch <- ifelse(SGTData$StayProbability==1,"Stay","Switch")

#save prev reward and stay switch for fMRI analysis
dataBehReg <- as.data.frame(cbind(SGTData$AdvChoice, SGTData$PreviousOutcome, SGTData$StayProbability))
colnames(dataBehReg) <- c("AdvChoice","PreviousOutcome","StaySwitch")
head(dataBehReg)
#write.csv(dataBehReg,file="StaySwitchRegressors.csv")


#take out the first trials (erase the first trials)
SGTDatab <- subset(SGTData,Trial>1)
head(SGTDatab)
SGTDatab$Condition <- as.factor(SGTDatab$Condition)

#View(SGTDatab)

#use dplyr to do stay-switch analysis
head(SGTDatab)
SummaryStaySwitch <- SGTDatab %>%
  group_by(Condition, Subnum, StaySwitch) %>%
  summarize(MeanPriorOutcome = mean(PreviousOutcome))
SummaryStaySwitch

SaveStaySwitch <- as.data.frame(SummaryStaySwitch)

StaySave <- subset(SaveStaySwitch,StaySwitch=="Stay")
head(StaySave)
SwitchSave <- subset(SaveStaySwitch,StaySwitch=="Switch")
head(SwitchSave)

write.csv(StaySave,file="StayPrevRewards.csv")
write.csv(SwitchSave,file="SwitchPrevRewards.csv")

head(SaveStaySwitch)
write.csv(SaveStaySwitch,file = "StaySwitchOutcomes.csv")

GroupedStaySwitch <- SummaryStaySwitch %>%
  group_by(Condition, StaySwitch) %>%
  summarize(PrevOutcome = mean(MeanPriorOutcome),
            sd_PR = sd(MeanPriorOutcome),
            n_PR =n(),
            se_PE = sd(MeanPriorOutcome)/sqrt(n()))
GroupedStaySwitch #could save this to run in JASP? - need to separate stay probability

PlotStaySwitch<- ggplot(GroupedStaySwitch, aes(x=StaySwitch,y=PrevOutcome,group=Condition,color=Condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=PrevOutcome-se_PE, ymax=PrevOutcome+se_PE), width=.1,
                position=position_dodge(0.00)) +
  ggtitle("Average Reward Preceding Stay/Switch Trials") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Average Previous Reward")
PlotStaySwitch <- PlotStaySwitch + scale_color_manual(values=c("blue","red"))
PlotStaySwitch

head(SGTDatab)
SGTDatab$StayProbability <- as.factor(SGTDatab$StayProbability)
levels(SGTDatab$StayProbability)

head(SaveStaySwitch)
SaveStaySwitch$Condition <- as.factor(SaveStaySwitch$Condition)
SaveStaySwitch$StaySwitch <- as.factor(SaveStaySwitch$StaySwitch)

Mbrm33 <- brm(MeanPriorOutcome ~ StaySwitch*Condition + (1|Subnum),
             data = SaveStaySwitch,family = "gaussian")
print(summary(Mbrm33),digits=4)
conditional_effects(Mbrm3)


#Stay switch model
Mbrm3 <- brm(StayProbability ~ PreviousOutcome*Condition + (1|Subnum),
             data = SGTDatab,family = "bernoulli")
print(summary(Mbrm3), digits=4)
conditional_effects(Mbrm3) #maybe plot this

#Stay switch model lower model
Mbrm4 <- brm(StayProbability ~ PreviousOutcome + Condition + (1|Subnum),
             data = SGTDatab,family = "bernoulli")
print(summary(Mbrm4),digits=4)
#extract plot objects
gg <- plot(marginal_effects(Mbrm3))

#get each plot separately
plot1 <- gg[[1]]
plot2 <- gg[[2]]
plot3 <- gg[[3]]


#edit plots

## colorblind friendly palettes:
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot3 <- plot3 +
  labs(x = "Previous Outcome", y = "p(Stay)") +
  scale_fill_manual(name = "Age Group", values=c(cbPalette[3], cbPalette[2])) +
  scale_color_manual(name = "Age Group", values=c(cbPalette[3],cbPalette[2]))+
   #scale_color_manual(values=c("blue","red")) +

  theme_bw() +
  theme(panel.grid = element_blank(), 
        text = element_text(size=12), 
        axis.title.x = element_text(margin = margin(t=10,r=0,b=0,l=0)),
        axis.title.y = element_text(margin = margin(t=0,r=10,b=0,l=0))) +  
  ggtitle("Probability of Repeating Choices") +
  theme(plot.title = element_text(hjust = 0.5)) 
plot3

head(SGTDatab)
#total switching
SGTDatab$StayValue <- as.numeric(SGTDatab$StayProbability) -1


SwitchAnalysis <- SGTDatab %>%
  group_by(Condition,Subnum) %>%
  summarize(PropStay = mean(StayValue))
SwitchAnalysis
write.csv(SwitchAnalysis,file = "SGTSegStayProbabilites.csv")


SwitchTotals<- SwitchAnalysis %>%
  group_by(Condition) %>%
  summarize(PropStayT = mean(PropStay),
            sd_PR = sd(PropStay),
            n_PR =n(),
            se_PE = sd(PropStay)/sqrt(n()))
SwitchTotals




#RT analysis
PlotHistRT <- ggplot(SGTData, aes(x=RT)) +
  geom_histogram(binwidth = .5)
PlotHistRT
View(SGTData)

SummaryAvgRT <- SGTData %>%
  group_by(Condition,Subnum,Block) %>%
  summarise(AvgRT = mean(RT))
SummaryAvgRT

GroupedRT <- SummaryAvgRT %>%
  group_by(Condition,Block) %>%
  summarise(AvgResponseTime = mean(AvgRT),
            sd_PR = sd(AvgRT),
            n_PR =n(),
            se_PE = sd(AvgRT)/sqrt(n()))
GroupedRT

PlotRT <- ggplot(GroupedRT, aes(x=Block,y=AvgResponseTime,group=Condition,color=Condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=AvgResponseTime-se_PE, ymax=AvgResponseTime+se_PE), width=.1,
                position=position_dodge(0.00)) +
  ggtitle("Average Response Time By Condition") +
  ylab("Average Response Time (s)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
PlotRT <- PlotRT + scale_color_manual(values=c("blue","red"))
PlotRT

head(SGTData)
#model for predicting RTs from condition and block
Mbrm1RT <- brm(RT ~ Condition + Block + (1|Subnum),
             data = SGTData,family = "gaussian")
print(summary(Mbrm1RT),digits=4)
saveRDS(Mbrm1RT,file = "RTFromCoditionPlusBlock.rds")
conditional_effects(Mbrm1RT)

Mbrm2RT <- brm(RT ~ Condition * Block + (1|Subnum),
               data = SGTData,family = "gaussian")
print(summary(Mbrm2RT),digits=4)

#RT to predict optimal choices
Mbrm3RT <- brm(OptimChoice ~ Condition * RT + (1|Subnum),
               data = SGTData,family = "bernoulli")
print(summary(Mbrm3RT),digits=4)
conditional_effects(Mbrm3RT)


head(SGTData)
###  RT analyzed by  sex
SummaryAvgRTS <- SGTData %>%
  group_by(Condition,Sex,Subnum,Block) %>%
  summarise(AvgRT = mean(RT))
SummaryAvgRTS

GroupedAvgRTS <- SummaryAvgRTS %>%
  group_by(Condition,Sex,Block) %>%
  summarise(MeanRT = mean(AvgRT),
            sd_PR = sd(AvgRT),
            n_PR =n(),
            se_PE = sd(AvgRT)/sqrt(n()))
GroupedAvgRTS

GroupedAvgRTS <- as.data.frame(GroupedAvgRTS)
head(GroupedAvgRTS)
#remove person who selected other in the segmented condition
GroupedAvgRTS <- subset(GroupedAvgRTS,Sex!="Other")


PlotAvgRTS<- ggplot(GroupedAvgRTS, aes(x=Block,y=MeanRT,group=Condition,color=Condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=MeanRT-se_PE, ymax=MeanRT+se_PE), width=.1,
                position=position_dodge(0.00)) +
  ggtitle("Average Response Time By Condition and Sex") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Average Response Time (s)") +
  facet_wrap(~Sex)
PlotAvgRTS <- PlotAvgRTS + scale_color_manual(values=c("blue","red"))
PlotAvgRTS




library(tidyverse)
library(brms)

setwd("E:/RCode/SGTSegmented")
getwd()

#load control data
AllSGTOrigData <- read.csv("SGTOriginalOnlyData_9_9_2022_FirstSubsTest.csv", header = TRUE)
head(AllSGTOrigData)

AllSGTSegData <- read.csv("SGTSegGainsOnlyData__9_9_22.csv", header = TRUE)
head(AllSGTSegData)

NewTrialsData <- subset(AllSGTSegData,NewTrial==1)
head(NewTrialsData)
nrow(NewTrialsData)/100

nAChoice <- 0 
nBChoice <- 0 
nCChoice <- 0 
nDChoice <- 0

## change the reward variable for the New Trials data to make it correct (only gives the first reward
#that was give on each trial; needs to be changed)
for (i in 1:nrow(NewTrialsData)){
  if (NewTrialsData$keyResponse[i]==1){
    nAChoice <- nAChoice + 1
    if (nAChoice<5){
      NewTrialsData$Reward[i]=500
      NewTrialsData$gain[i]=500
      NewTrialsData$loss[i]=0
    } else {
      nAChoice = 0
      NewTrialsData$Reward[i]=0
      NewTrialsData$gain[i]=0
      NewTrialsData$loss[i]=0    }
  } else if (NewTrialsData$keyResponse[i]==2){
    nBChoice <- nBChoice + 1
    if (nBChoice<5){
      NewTrialsData$Reward[i]=450
      NewTrialsData$gain[i]=450
      NewTrialsData$loss[i]=0
    } else {nBChoice = 0
      NewTrialsData$Reward[i]=200
      NewTrialsData$gain[i]=200
      NewTrialsData$loss[i]=0}    
  } else if (NewTrialsData$keyResponse[i]==3){
    nCChoice <- nCChoice + 1
    if (nCChoice<5){
      NewTrialsData$Reward[i]=300
      NewTrialsData$gain[i]=300
      NewTrialsData$loss[i]=0
    } else {nCChoice = 0
      NewTrialsData$Reward[i]=1300
      NewTrialsData$gain[i]=1300
      NewTrialsData$loss[i]=0}    
  } else if (NewTrialsData$keyResponse[i]==4){
    nDChoice <- nDChoice + 1
    if (nDChoice<5){
      NewTrialsData$Reward[i]=250
      NewTrialsData$gain[i]=250
      NewTrialsData$loss[i]=0} 
  } else {nDChoice = 0
    NewTrialsData$Reward[i]=1500
    NewTrialsData$gain[i]=1500
    NewTrialsData$loss[i]=0}    
  }

head(NewTrialsData)


BlockSequence <- rep(1:4,each=25)
#View(BlockSequence)

NewTrialsData$Block <- rep(BlockSequence,nrow(NewTrialsData)/100)


AllSGTOrigData$Block <- rep(BlockSequence,nrow(AllSGTOrigData)/100)

write.csv(NewTrialsData,"SGTSegGainsNewTrialsOnlyData_9_9_22.csv")

##combines the data to compare
head(AllSGTOrigData)
head(NewTrialsData)

NewTrialsData <- subset(NewTrialsData,select = -NewTrial)
NewTrialsData <- subset(NewTrialsData,select = -gain)
NewTrialsData <- subset(NewTrialsData,select = -loss)

# ### modify OrigData
# for (i in 1:nrow(AllSGTOrigData)){
#   if (AllSGTOrigData$keyResponse[i]==1){
#     if (AllSGTOrigData$Reward[i]<0){
#       AllSGTOrigData$Reward[i]=-1050
#       AllSGTOrigData$gain[i]=0
#       AllSGTOrigData$loss[i]=1050
#     } else {AllSGTOrigData$Reward[i]=200
#     AllSGTOrigData$gain[i]=200
#     AllSGTOrigData$loss[i]=0    }
#   } else if (AllSGTOrigData$keyResponse[i]==2){
#     if (AllSGTOrigData$Reward[i]<0){
#       AllSGTOrigData$Reward[i]=-650
#       AllSGTOrigData$gain[i]=0
#       AllSGTOrigData$loss[i]=650
#     } else {AllSGTOrigData$Reward[i]=100
#     AllSGTOrigData$gain[i]=100
#     AllSGTOrigData$loss[i]=0}    
#   } else if (AllSGTOrigData$keyResponse[i]==3){
#     if (AllSGTOrigData$Reward[i]<0){
#       AllSGTOrigData$Reward[i]=-200
#       AllSGTOrigData$gain[i]=0
#       AllSGTOrigData$loss[i]=200
#     } else {AllSGTOrigData$Reward[i]=1050
#     AllSGTOrigData$gain[i]=1050
#     AllSGTOrigData$loss[i]=0}    
#   } else if (AllSGTOrigData$keyResponse[i]==4){
#     if (AllSGTOrigData$Reward[i]<0){
#       AllSGTOrigData$Reward[i]=-100
#       AllSGTOrigData$gain[i]=0
#       AllSGTOrigData$loss[i]=100} 
#   } else {AllSGTOrigData$Reward[i]=650
#   AllSGTOrigData$gain[i]=650
#   AllSGTOrigData$loss[i]=0}    
# }


ncol(AllSGTOrigData)
ncol(NewTrialsData)

#now combine them
CombDataOrigSeg <- rbind(AllSGTOrigData,NewTrialsData)

write.csv(CombDataOrigSeg,file = "CombinedSGTSegGains_Formatted_9_9_22.csv")

#get just the first 100 trials
CombData100Trials <- subset(CombDataOrigSeg,Trial<=100)
write.csv(CombData100Trials,file = "CombinedSGTSegDataNewTrialsOnly100T.csv")

head(CombData100Trials)
CombData100Trials$choice <- CombData100Trials$keyResponse
CombData100Trials$outcome <- CombData100Trials$Reward
CombData100Trials$subjID <- CombData100Trials$Subnum


SegOnly100TData <- subset(CombData100Trials,Condition=="SEG")
head(SegOnly100TData)
ControlOnly100TData <- subset(CombData100Trials,Condition=="Control")
head(ControlOnly100TData)

#write files formatted for hBayesDM
write.table(SegOnly100TData, file = "SGTSegmentedOnly100TData.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

write.table(ControlOnly100TData, file = "SGTControlOnly100TData.txt", sep = "\t",
            row.names = TRUE, col.names = NA)









SummaryPropOptimal <- CombDataOrigSeg %>%
  group_by(Condition,Subnum,Block) %>%
  summarise(PropOptimal = mean(OptimChoice))
SummaryPropOptimal

GroupedPropOptimal <- SummaryPropOptimal %>%
  group_by(Condition,Block) %>%
  summarise(ProportionOptimal = mean(PropOptimal),
            sd_PR = sd(PropOptimal),
            n_PR =n(),
            se_PE = sd(PropOptimal)/sqrt(n()))
GroupedPropOptimal

PlotPropOptimal<- ggplot(GroupedPropOptimal, aes(x=Block,y=ProportionOptimal,group=Condition,color=Condition)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=ProportionOptimal-se_PE, ymax=ProportionOptimal+se_PE), width=.1,
                position=position_dodge(0.00))
PlotPropOptimal <- PlotPropOptimal + scale_color_manual(values=c("black","red"))
PlotPropOptimal



Mbrm1 <- brm(OptimChoice ~ Condition*Block + (1|Subnum),
             data = CombDataOrigSeg,family = bernoulli)
print(summary(Mbrm1),digits=4)
conditional_effects(Mbrm1)

#main effects only model
Mbrm2 <- brm(OptimChoice ~ Condition + Block + (1|Subnum),
             data = CombDataOrigSeg,family = bernoulli)
print(summary(Mbrm2),digits=4)


#plot by trial
SummaryPropOptimalT <- CombDataOrigSeg %>%
  group_by(Condition,Subnum,Trial) %>%
  summarise(PropOptimal = mean(OptimChoice))
SummaryPropOptimalT

GroupedPropOptimalT <- SummaryPropOptimalT %>%
  group_by(Condition,Trial) %>%
  summarise(ProportionOptimal = mean(PropOptimal),
            sd_PR = sd(PropOptimal),
            n_PR =n(),
            se_PE = sd(PropOptimal)/sqrt(n()))
GroupedPropOptimalT

PlotPropOptimalT<- ggplot(GroupedPropOptimalT, aes(x=Trial,y=ProportionOptimal,group=Condition,color=Condition)) +
  geom_line() +
  geom_point() 
  # geom_errorbar(aes(ymin=ProportionOptimal-se_PE, ymax=ProportionOptimal+se_PE), width=.1,
  #               position=position_dodge(0.00))
PlotPropOptimalT <- PlotPropOptimalT + scale_color_manual(values=c("black","red"))
PlotPropOptimalT

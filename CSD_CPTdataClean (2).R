###libs
library("dplyr")
library("reshape")
library("lme4")
library("ggplot2")


###Read in CPT Data, Trim the Outliers of the normalized variables, and make some 
###Pertinent plots. 

#read in the CSV file for the CSD transformed data
csdCPTdata<-read.csv("C:/Users/Chivon/Documents/Downloads/CSD_AllCPTdataNORMed.csv", header=TRUE, sep = ",")

Aprime_csdCPTdata<-read.csv("C:/Users/Chivon/Documents/Downloads/CSD_CPTdata_NORMed_withAprime.csv", header=TRUE, sep = ",")

csdCPTdata$Aprime<-Aprime_csdCPTdata$Aprime
csdCPTdata$RTCV<-Aprime_csdCPTdata$RTCV
Aprime_csdCPTdata<-NULL
rm(Aprime_csdCPTdata)

#Set up factors
csdCPTdata$PtID<-substr(csdCPTdata$Filename,4,6)
csdCPTdata$PtID<-factor(csdCPTdata$PtID)
csdCPTdata$Group<-factor(csdCPTdata$Group, levels = c("0","1","2"),labels = c("Control","R1Trainee","R2Trainee"))
csdCPTdata$Timepoint<-factor(csdCPTdata$Timepoint,levels = c("1","2","3"), labels = c("Pre","Mid","Post"))
csdCPTdata$Retreat <-factor(csdCPTdata$Retreat,levels = c("1","2"), labels = c("R1","R2"))
csdCPTdata$Type<-factor(csdCPTdata$Type,levels=c("2","4"), labels = c("Nontarget","Target"))
csdCPTdata$Block<-factor(csdCPTdata$Block,levels=c("1","2","3","4","5","6","7","8"))
csdCPTdata$BCond<-factor(csdCPTdata$BCond, levels=c("1","2","3","4"), labels = c("Hit", "CorrRej","Miss","FA"))
csdCPTdata$PrevTrialType<-factor(csdCPTdata$PrevTrialType,levels=c("1","2","3","4"),labels = c("Hit", "CorrRej","Miss","FA"))
csdCPTdata$RT[csdCPTdata$RT==0]<-NA

#Set up Block factor based on order of the trial
####Set up the blocks based on the order number
csdCPTdata$Block[csdCPTdata$Order >120 & csdCPTdata$Order < 241] <- 2
csdCPTdata$Block[csdCPTdata$Order >240 & csdCPTdata$Order < 361] <- 3
csdCPTdata$Block[csdCPTdata$Order >360 & csdCPTdata$Order < 481] <- 4
csdCPTdata$Block[csdCPTdata$Order >480 & csdCPTdata$Order < 601] <- 5
csdCPTdata$Block[csdCPTdata$Order >600 & csdCPTdata$Order < 721] <- 6
csdCPTdata$Block[csdCPTdata$Order >720 & csdCPTdata$Order < 841] <- 7
csdCPTdata$Block[csdCPTdata$Order >840 & csdCPTdata$Order < 961] <- 8

###standardize Alpha Values Across participants
MeanOVAlpha<-mean(csdCPTdata$OV1sPreAlpha)
sdOVAlpha<-sd(csdCPTdata$OV1sPreAlpha)
csdCPTdata$normedAlpha<-(csdCPTdata$OV1sPreAlpha-MeanOVAlpha)/sdOVAlpha

MeanRHPeakAmp<-mean(csdCPTdata$AllcrsPeakAmp)
sdRHPeakAmp<-sd(csdCPTdata$AllcrsPeakAmp)
csdCPTdata$normedRHPeakAmp<-(csdCPTdata$AllcrsPeakAmp-MeanRHPeakAmp)/sdRHPeakAmp

#create level 2 predictors recentered variables
RT1Subset<-subset(csdCPTdata,Retreat=="R1")
RT2Subset<-subset(csdCPTdata,Retreat=="R2")

PtIDSubsets<-split(RT1Subset,RT1Subset$PtID, drop=TRUE)
OVAlphadata<-factor()
IAPdata<-factor()
AvgIAPdata<-factor()
AvgOVAlphadata<-factor()

for(i in 1:length(PtIDSubsets)){
  PtIDdata<-data.frame(PtIDSubsets[i])
  PtIDdata$AvgAlpha<-mean(PtIDdata[,13])
  PtIDdata$AvgIAP<-mean(PtIDdata[,16])
  PtIDdata$AlphafromAvg<-PtIDdata[,13]-PtIDdata$AvgAlpha
  PtIDdata$IAPfromAvg<-PtIDdata[,16]-PtIDdata$AvgIAP
  OVAlphadata<-c(OVAlphadata,PtIDdata$AlphafromAvg)
  IAPdata<-c(IAPdata,PtIDdata$IAPfromAvg)
  AvgIAPdata<-c(AvgIAPdata,PtIDdata$AvgIAP)
  AvgOVAlphadata<-c(AvgOVAlphadata,PtIDdata$AvgAlpha)
}
rm(PtIDdata)
RT1Subset$OVAlphafromAvg<-OVAlphadata
RT1Subset$IAPfromAvg<-IAPdata
RT1Subset$AvgFileIAP<-AvgIAPdata
RT1Subset$AvgFileOVAlpha<-AvgOVAlphadata

PtIDSubsets<-split(RT2Subset,RT2Subset$PtID, drop=TRUE)
length(PtIDSubsets)
OVAlphadata<-factor()
IAPdata<-factor()
AvgIAPdata<-factor()
AvgOVAlphadata<-factor()

for(i in 1:length(PtIDSubsets)){
  PtIDdata<-data.frame(PtIDSubsets[i])
  PtIDdata$AvgAlpha<-mean(PtIDdata[,13])
  PtIDdata$AvgIAP<-mean(PtIDdata[,16])
  PtIDdata$AlphafromAvg<-PtIDdata[,13]-PtIDdata$AvgAlpha
  PtIDdata$IAPfromAvg<-PtIDdata[,16]-PtIDdata$AvgIAP
  OVAlphadata<-c(OVAlphadata,PtIDdata$AlphafromAvg)
  IAPdata<-c(IAPdata,PtIDdata$IAPfromAvg)
  AvgIAPdata<-c(AvgIAPdata,PtIDdata$AvgIAP)
  AvgOVAlphadata<-c(AvgOVAlphadata,PtIDdata$AvgAlpha)
}
rm(PtIDdata)
RT2Subset$OVAlphafromAvg<-OVAlphadata
RT2Subset$IAPfromAvg<-IAPdata
RT2Subset$AvgFileIAP<-AvgIAPdata
RT2Subset$AvgFileOVAlpha<-AvgOVAlphadata


##Subset the data by behavioral condition: CR, Hit, FA, Miss 
BCondSubsets<-split(csdCPTdata,csdCPTdata$BCond, drop = TRUE)
AllHit<-data.frame((BCondSubsets[["Hit"]]))

#Plot the RT data for outlier examination
#RT data only includes target stimulus presentations
##This cutoff decided based on boxplot visualizations of RT by block
#Before
# plot(AllHit$Block,AllHit$RT,main = "Hit Response Times by Block")
# plot(AllHit$Block,AllHit$RHhitsPeakAmp, main = "Peak Amplitudes in the RH electrode with Highest Average Hit Amp")
# plot(AllHit$Block,AllHit$LHhitsPeakAmp,main = "Peak Amplitudes in the LH electrode with Highest Average Hit Amp")
# plot(AllHit$Block,AllHit$AllhitsPeakAmp,main = "Peak Amplitudes in the RH electrode with Highest Average Amp of all electrodes")
# plot(AllHit$Block,AllHit$OV1sPreAlpha, main = "Full range PreStim Alpha for Hits by block")
# plot(AllHit$Block,AllHit$Hi1sPreAlpha, main = "Hi Prestim Alpha for Hits by block")
# plot(AllHit$Block,AllHit$Low1sPreAlpha, main = "Low Prestim Alpha for Hits by block")

HiOutlier<-mean(AllHit$RT) + (2.5* sd(AllHit$RT))
LoOutlier<-mean(AllHit$RT) - (2.5* sd(AllHit$RT))

##cut outliers in normalized data at 2sd
AllHit.trim<-subset(AllHit,RT>LoOutlier & RT<HiOutlier)
AllHit.trim<-subset(AllHit.trim,RHhitsPeakAmp > -2.5 & RHhitsPeakAmp < 2.5)
AllHit.trim<-subset(AllHit.trim,LHhitsPeakAmp > -2.5 & LHhitsPeakAmp < 2.5)
AllHit.trim<-subset(AllHit.trim,AllhitsPeakAmp > -2.5 & AllhitsPeakAmp < 2.5)
AllHit.trim<-subset(AllHit.trim,OV1sPreAlpha > -2.5 & OV1sPreAlpha < 2.5)
AllHit.trim<-subset(AllHit.trim,Hi1sPreAlpha > -2.5 & Hi1sPreAlpha < 2.5)
AllHit.trim<-subset(AllHit.trim,Low1sPreAlpha > -2.5 & Low1sPreAlpha < 2.5)


#After
# plot(AllHit.trim$Block,AllHit.trim$RT,main = "Hit Response Times by Block")
# 
# plot(AllHit.trim$Block,AllHit.trim$RHhitsPeakAmp, main = "Peak Amplitudes in the RH electrode with Highest Average Hit Amp")
# plot(AllHit.trim$Block,AllHit.trim$LHhitsPeakAmp,main = "Peak Amplitudes in the LH electrode with Highest Average Hit Amp")
# plot(AllHit.trim$Block,AllHit.trim$AllhitsPeakAmp,main = "Peak Amplitudes in the RH electrode with Highest Average Amp of all electrodes")
# plot(AllHit.trim$Block,AllHit.trim$OV1sPreAlpha, main = "Full range PreStim Alpha for Hits by block")
# plot(AllHit.trim$Block,AllHit.trim$Hi1sPreAlpha, main = "Hi Prestim Alpha for Hits by block")
# plot(AllHit.trim$Block,AllHit.trim$Low1sPreAlpha, main = "Low Prestim Alpha for Hits by block")

AllCR<-data.frame((BCondSubsets[["CorrRej"]]))

##Cutting outliers based on 2 standard deviations above the overall mean
##This cutoff decided based on boxplot visualizations of RT by block
#Before
# plot(AllCR$Block,AllCR$RHcrsPeakAmp, main = "Peak Amplitudes in the RH electrode with Highest Average CR Amp")
# plot(AllCR$Block,AllCR$LHcrsPeakAmp,main = "Peak Amplitudes in the LH electrode with Highest Average CR Amp")
# plot(AllCR$Block,AllCR$AllcrsPeakAmp,main = "Peak Amplitudes in the RH electrode with Highest Average Amp of all electrodes")
# plot(AllCR$Block,AllCR$OV1sPreAlpha, main = "Full range PreStim Alpha for CRs by block")
# plot(AllCR$Block,AllCR$Hi1sPreAlpha, main = "Hi Prestim Alpha for CRs by block")
# plot(AllCR$Block,AllCR$Low1sPreAlpha, main = "Low Prestim Alpha for CRs by block")
##cut outliers in normalized data at 2sd

AllCR.trim<-subset(AllCR,RHcrsPeakAmp > -2.5 & RHcrsPeakAmp < 2.5)
AllCR.trim<-subset(AllCR.trim,LHcrsPeakAmp > -2.5 & LHcrsPeakAmp < 2.5)
AllCR.trim<-subset(AllCR.trim,AllcrsPeakAmp > -2.5 & AllcrsPeakAmp < 2.5)
AllCR.trim<-subset(AllCR.trim,OV1sPreAlpha> -2.5 & OV1sPreAlpha < 2.5)
AllCR.trim<-subset(AllCR.trim,Hi1sPreAlpha> -2.5 & Hi1sPreAlpha < 2.5)
AllCR.trim<-subset(AllCR.trim,Low1sPreAlpha> -2.5 & Low1sPreAlpha < 2.5)


#After
# plot(AllCR.trim$Block,AllCR.trim$RHcrsPeakAmp, main = "Peak Amplitudes in the RH electrode with Highest Average CR Amp")
# plot(AllCR.trim$Block,AllCR.trim$LHcrsPeakAmp,main = "Peak Amplitudes in the LH electrode with Highest Average CR Amp")
# plot(AllCR.trim$Block,AllCR.trim$AllcrsPeakAmp,main = "Peak Amplitudes in the RH electrode with Highest Average Amp of all electrodes")
# plot(AllCR.trim$Block,AllCR.trim$OV1sPreAlpha, main = "Full range PreStim Alpha for CRs by block")
# plot(AllCR.trim$Block,AllCR.trim$Hi1sPreAlpha, main = "Hi Prestim Alpha for CRs by block")
# plot(AllCR.trim$Block,AllCR.trim$Low1sPreAlpha, main = "Low Prestim Alpha for CRs by block")


AllMiss<-data.frame((BCondSubsets[["Miss"]]))

##Cutting outliers based on 2 standard deviations above the overall mean
##This cutoff decided based on boxplot visualizations of RT by block
#Before
#plot(AllMiss$Block,AllMiss$RT)


##cut outliers in normalized data at 2sd

AllMiss.trim<-subset(AllMiss,RHcrsPeakAmp > -2.5 & RHcrsPeakAmp < 2.5)
AllMiss.trim<-subset(AllMiss.trim,LHcrsPeakAmp > -2.5 & LHcrsPeakAmp < 2.5)
AllMiss.trim<-subset(AllMiss.trim,AllcrsPeakAmp > -2.5 & AllcrsPeakAmp < 2.5)
AllMiss.trim<-subset(AllMiss.trim,OV1sPreAlpha> -2.5 & OV1sPreAlpha < 2.5)
AllMiss.trim<-subset(AllMiss.trim,Hi1sPreAlpha> -2.5 & Hi1sPreAlpha < 2.5)
AllMiss.trim<-subset(AllMiss.trim,Low1sPreAlpha> -2.5 & Low1sPreAlpha < 2.5)


#After
#plot(AllMiss.trim$Block,AllMiss.trim$RT)

AllFA<-data.frame((BCondSubsets[["FA"]]))

##Cutting outliers based on 2 standard deviations above the overall mean
##This cutoff decided based on boxplot visualizations of RT by block
#Before
#plot(AllFA$Block,AllFA$RT, main = "False Alarm Response Times by Block")
HiOutlier<-mean(AllFA$RT) + (1.5* sd(AllFA$RT))
LoOutlier<-mean(AllFA$RT) - (1.5* sd(AllFA$RT))
##cut outliers in normalized data at 2sd
AllFA.trim<-subset(AllFA,RT>LoOutlier & RT<HiOutlier)
AllFA.trim<-subset(AllFA.trim,RHcrsPeakAmp > -2.5 & RHcrsPeakAmp < 2.5)
AllFA.trim<-subset(AllFA.trim,LHcrsPeakAmp > -2.5 & LHcrsPeakAmp < 2.5)
AllFA.trim<-subset(AllFA.trim,AllcrsPeakAmp > -2.5 & AllcrsPeakAmp < 2.5)
AllFA.trim<-subset(AllFA.trim,OV1sPreAlpha > -2.5 & OV1sPreAlpha < 2.5)
AllFA.trim<-subset(AllFA.trim,Hi1sPreAlpha > -2.5 & Hi1sPreAlpha < 2.5)
AllFA.trim<-subset(AllFA.trim,Low1sPreAlpha > -2.5 & Low1sPreAlpha < 2.5)


#After
#plot(AllFA.trim$Block,AllFA.trim$RT, main = "False Alarm Response Times by Block")

#Create Block-based version of the data
#CRs
CROVAlphablocks<-aggregate(OV1sPreAlpha ~ Retreat + Group + Timepoint + Block + PtID, data=AllCR.trim, mean)
CRHiAlphablocks<-aggregate(Hi1sPreAlpha ~ Retreat + Group + Timepoint + Block + PtID, data=AllCR.trim, mean)
CRLoAlphablocks<-aggregate(Low1sPreAlpha ~ Retreat + Group + Timepoint + Block + PtID, data=AllCR.trim, mean)
CRIAPblocks<-aggregate(IAP ~ Retreat + Group + Timepoint + Block + PtID, data=AllCR.trim, mean)
CRAprimeblocks<-aggregate(Aprime ~ Retreat + Group + Timepoint + Block + PtID, data=AllCR.trim, mean)

CRRHampblocks<-aggregate(RHcrsPeakAmp ~ Retreat + Group + Timepoint + Block + PtID, data=AllCR.trim, mean)
CRLHampblocks<-aggregate(LHcrsPeakAmp ~ Retreat + Group + Timepoint + Block + PtID, data=AllCR.trim, mean)
CRAllampblocks<-aggregate(AllcrsPeakAmp ~ Retreat + Group + Timepoint + Block + PtID, data=AllCR.trim, mean)

CRRHLatblocks<-aggregate(RHcrsPeakLat ~ Retreat + Group + Timepoint + Block + PtID, data=AllCR.trim, mean)
CRLHLatblocks<-aggregate(LHcrsPeakLat ~ Retreat + Group + Timepoint + Block + PtID, data=AllCR.trim, mean)
CRAllLatblocks<-aggregate(AllcrsPeakLat ~ Retreat + Group + Timepoint + Block + PtID, data=AllCR.trim, mean)

# CROVAlphafromAvgblocks<-aggregate(OVAlphafromAvg ~ Retreat + Group + Timepoint + Block + PtID, data=AllCR.trim, mean)
# CRIAPfromAvgblocks<-aggregate(IAPfromAvg ~ Retreat + Group + Timepoint + Block + PtID, data=AllCR.trim, mean)
# CRAvgOVAlphablocks<-aggregate(AvgFileOVAlpha ~ Retreat + Group + Timepoint + Block + PtID, data=AllCR.trim, mean)
# CRAvgIAPblocks<-aggregate(AvgFileIAP ~ Retreat + Group + Timepoint + Block + PtID, data=AllCR.trim, mean)

#bind the aggregated variables.
#CRblockData<-cbind(CROVAlphablocks,CRHiAlphablocks[,6],CRLoAlphablocks[,6],CRIAPblocks[,6],CRRHampblocks[,6],CRLHampblocks[,6],CRAllampblocks[,6],CRRHLatblocks[,6],CRLHLatblocks[,6],CRAllLatblocks[,6],CROVAlphafromAvgblocks[,6],CRIAPfromAvgblocks[,6],CRAvgOVAlphablocks[,6],CRAvgIAPblocks[,6])
#names(CRblockData)<-c("Retreat","Group","Timepoint","Block","PtID","OV1sPreAlpha","CRHiAlphablocks","CRLoAlphablocks","CRIAPblocks","CRRHampblocks","CRLHampblocks","CRAllampblocks","CRRHLatblocks","CRLHLatblocks","CRAllLatblocks","CROVAlphafromAvg","CRIAPfromAvg","CRAvgFileOVAlpha","CRAvgFileIAP")


CRblockData<-cbind(CROVAlphablocks,CRHiAlphablocks[,6],CRLoAlphablocks[,6],CRIAPblocks[,6],CRRHampblocks[,6],CRLHampblocks[,6],CRAllampblocks[,6],CRRHLatblocks[,6],CRLHLatblocks[,6],CRAllLatblocks[,6],CRAprimeblocks[,6])
names(CRblockData)<-c("Retreat","Group","Timepoint","Block","PtID","OV1sPreAlpha","CRHiAlphablocks","CRLoAlphablocks","CRIAPblocks","CRRHampblocks","CRLHampblocks","CRAllampblocks","CRRHLatblocks","CRLHLatblocks","CRAllLatblocks","CRAprimeblocks")

#Clear the redundant data from the workspace
#rm(CROVAlphablocks,CRHiAlphablocks,CRLoAlphablocks,CRIAPblocks,CRRHampblocks,CRLHampblocks,CRAllampblocks,CRRHLatblocks,CRLHLatblocks,CRAllLatblocks, CROVAlphafromAvgblocks,CRIAPfromAvgblocks,CRAvgOVAlphablocks,CRAvgIAPblocks)

rm(CRAprimeblocks,CROVAlphablocks,CRHiAlphablocks,CRLoAlphablocks,CRIAPblocks,CRRHampblocks,CRLHampblocks,CRAllampblocks,CRRHLatblocks,CRLHLatblocks,CRAllLatblocks)

CRR1Groups<-subset(CRblockData, Group != "R2Trainee")
CRR2Groups<-subset(CRblockData, Group=="R2Trainee")

#Hits
HitRTblocks<-aggregate(RT ~ Retreat + Group + Timepoint + Block + PtID, data = AllHit.trim, mean)

HitOVAlphablocks<-aggregate(OV1sPreAlpha ~ Retreat + Group + Timepoint + Block + PtID, data=AllHit.trim, mean)
HitHiAlphablocks<-aggregate(Hi1sPreAlpha ~ Retreat + Group + Timepoint + Block + PtID, data=AllHit.trim, mean)
HitLoAlphablocks<-aggregate(Low1sPreAlpha ~ Retreat + Group + Timepoint + Block + PtID, data=AllHit.trim, mean)
HitIAPblocks<-aggregate(IAP ~ Retreat + Group + Timepoint + Block + PtID, data=AllHit.trim, mean)
HitAprimeblocks<-aggregate(Aprime ~ Retreat + Group + Timepoint + Block + PtID, data=AllHit.trim, mean)
HitRTCVblocks<-aggregate(RTCV ~ Retreat + Group + Timepoint + Block + PtID, data = AllHit.trim,mean)

HitRHampblocks<-aggregate(RHhitsPeakAmp ~ Retreat + Group + Timepoint + Block + PtID, data=AllHit.trim, mean)
HitLHampblocks<-aggregate(LHhitsPeakAmp ~ Retreat + Group + Timepoint + Block + PtID, data=AllHit.trim, mean)
HitAllampblocks<-aggregate(AllhitsPeakAmp ~ Retreat + Group + Timepoint + Block + PtID, data=AllHit.trim, mean)

HitRHLatblocks<-aggregate(RHhitsPeakLat ~ Retreat + Group + Timepoint + Block + PtID, data=AllHit.trim, mean)
HitLHLatblocks<-aggregate(LHhitsPeakLat ~ Retreat + Group + Timepoint + Block + PtID, data=AllHit.trim, mean)
HitAllLatblocks<-aggregate(AllhitsPeakLat ~ Retreat + Group + Timepoint + Block + PtID, data=AllHit.trim, mean)

# HitOVAlphafromAvgblocks<-aggregate(OVAlphafromAvg ~ Retreat + Group + Timepoint + Block + PtID, data=AllHit.trim, mean)
# HitIAPfromAvgblocks<-aggregate(IAPfromAvg ~ Retreat + Group + Timepoint + Block + PtID, data=AllHit.trim, mean)
# HitAvgOVAlphablocks<-aggregate(AvgFileOVAlpha ~ Retreat + Group + Timepoint + Block + PtID, data=AllHit.trim, mean)
# HitAvgIAPblocks<-aggregate(AvgFileIAP ~ Retreat + Group + Timepoint + Block + PtID, data=AllHit.trim, mean)

#bind the aggregated variables.
#HitblockData<-cbind(HitOVAlphablocks,HitHiAlphablocks[,6],HitLoAlphablocks[,6],HitIAPblocks[,6],HitRHampblocks[,6],HitLHampblocks[,6],HitAllampblocks[,6],HitRHLatblocks[,6],HitLHLatblocks[,6],HitAllLatblocks[,6], HitOVAlphafromAvgblocks[,6],HitIAPfromAvgblocks[,6],HitRTblocks[,6],HitAvgOVAlphablocks[,6],HitAvgIAPblocks[,6])
#names(HitblockData)<-c("Retreat","Group","Timepoint","Block","PtID","OV1sPreAlpha","HitHiAlphablocks","HitLoAlphablocks","HitIAPblocks","HitRHampblocks","HitLHampblocks","HitAllampblocks","HitRHLatblocks","HitLHLatblocks","HitAllLatblocks","HitOVAlphafromAvg","HitIAPfromAvg","RT","HitAvgFileOVAlpha","HitAvgFileIAP")
HitblockData<-cbind(HitOVAlphablocks,HitHiAlphablocks[,6],HitLoAlphablocks[,6],HitIAPblocks[,6],HitRHampblocks[,6],HitLHampblocks[,6],HitAllampblocks[,6],HitRHLatblocks[,6],HitLHLatblocks[,6],HitAllLatblocks[,6], HitAprimeblocks[,6],HitRTCVblocks[,6])
names(HitblockData)<-c("Retreat","Group","Timepoint","Block","PtID","OV1sPreAlpha","HitHiAlphablocks","HitLoAlphablocks","HitIAPblocks","HitRHampblocks","HitLHampblocks","HitAllampblocks","HitRHLatblocks","HitLHLatblocks","HitAllLatblocks","HitAprimeblocks","HitRTCVblocks")


#Clear the redundant data from the workspace
#rm(HitOVAlphablocks,HitHiAlphablocks,HitLoAlphablocks,HitIAPblocks,HitRHampblocks,HitLHampblocks,HitAllampblocks,HitRHLatblocks,HitLHLatblocks,HitAllLatblocks, HitOVAlphafromAvgblocks, HitIAPfromAvgblocks, HitAvgOVAlphablocks,HitAvgIAPblocks)
rm(HitAprimeblocks, HitRTCVblocks, HitOVAlphablocks,HitHiAlphablocks,HitLoAlphablocks,HitIAPblocks,HitRHampblocks,HitLHampblocks,HitAllampblocks,HitRHLatblocks,HitLHLatblocks,HitAllLatblocks)

HitR1Groups<-subset(HitblockData, Group != "R2Trainee")
HitR2Groups<-subset(HitblockData, Group=="R2Trainee")

#Subset the data by stimulus type: Target or NonTarget
TypeSubsets<-split(csdCPTdata,csdCPTdata$Type, drop = TRUE)
AllNonTargets<-data.frame((TypeSubsets[[1]]))
AllTargets<-data.frame((TypeSubsets[[2]]))

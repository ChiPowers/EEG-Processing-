#libs
library(lme4)



##script to run Analyses of variance on the CPTdata and variables of interest.
#Correct Rejections - Do Group and Timepoint influence Overall Prestimulus Alpha? 

CR.OVAlpha.aov<-aov(OV1sPreAlpha~(Block*Timepoint*Group)+Error(PtID/Block*Timepoint)+Group, data = AllCR.trim)


CR.HiAlpha.aov<-aov(Hi1sPreAlpha~Group+Timepoint+Group*Timepoint, data = CRHiAlphablocks)
CR.LoAlpha.aov<-aov(Low1sPreAlpha~Group+Timepoint+Group*Timepoint, data = CRLoAlphablocks)
CR.LHcrsLat.aov<-aov(LHcrsPeakLat~Group+Timepoint+Group*Timepoint, data = CRLHLatblocks)
CR.RHcrsLat.aov<-aov(RHcrsPeakLat~Group+Timepoint+Group*Timepoint, data = CRRHLatblocks)
CR.ofAllcrsLat.aov<-aov(AllcrsPeakLat~Group+Timepoint+Group*Timepoint, data = CRAllLatblocks)
CR.LHcrsAmp.aov<-aov(LHcrsPeakAmp~Group+Timepoint+Group*Timepoint, data = CRLHampblocks)
CR.RHcrsAmp.aov<-aov(RHcrsPeakAmp~Group+Timepoint+Group*Timepoint, data = CRRHampblocks)
CR.ofAllcrsAmp.aov<-aov(AllcrsPeakAmp~Group+Timepoint+Group*Timepoint, data = CRAllampblocks)


boxplot(RHcrsPeakAmp~Group*Timepoint*Block,data=CRRHampblocks)
boxplot(RHcrsPeakLat~Group*Timepoint,data=AllCR.trim)

#get a summary of the model
summary(CR.OVAlpha.aov)

##print a table of means about the variables in the model

summary(CRaov)

#Hits - Do Group and timepoint influence Overall Response times?
Hit.OVAlpha.aov<-aov(OV1sPreAlpha~Group+Timepoint+Group*Timepoint, data = AllHit.trim)
Hit.HiAlpha.aov<-aov(Hi1sPreAlpha~Group+Timepoint+Group*Timepoint, data = AllHit.trim)
Hit.LoAlpha.aov<-aov(Low1sPreAlpha~Group+Timepoint+Group*Timepoint, data = AllHit.trim)
Hit.LHcrsLat.aov<-aov(LHcrsPeakLat~Group+Timepoint+Group*Timepoint, data = AllHit.trim)
Hit.RHcrsLat.aov<-aov(RHcrsPeakLat~Group+Timepoint+Group*Timepoint, data = AllHit.trim)
Hit.ofAllcrsLat.aov<-aov(AllcrsPeakLat~Group+Timepoint+Group*Timepoint, data = AllHit.trim)



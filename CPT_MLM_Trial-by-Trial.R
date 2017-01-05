#Load the libs
library(lme4)
library(nlme)

#is OVerall PRstimulus alpha a predictor of RH amp based on trial by trial fluctuations?
RHamp.CRR1model.byTrial<-lme(RHcrsPeakAmp ~ AvgFileOVAlpha+OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT1Subset)
anova(RHamp.CRR1model.byTrial)

RHamp.CRR1model_int.byTrial<-lme(RHcrsPeakAmp ~ AvgFileOVAlpha+OVAlphafromAvg+AvgFileOVAlpha*OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT1Subset)
anova(RHamp.CRR1model.byTrial)

RHamp.CRR1model_gt.byTrial<-lme(RHcrsPeakAmp ~ Group+Timepoint+AvgFileOVAlpha+OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT1Subset)
anova(RHamp.CRR1model.byTrial)

RHamp.CRR1model_gtint.byTrial<-lme(RHcrsPeakAmp ~ Group+Timepoint+AvgFileOVAlpha+OVAlphafromAvg+Group*Timepoint+Group*AvgFileOVAlpha+Group*OVAlphafromAvg+Timepoint*AvgFileOVAlpha+Timepoint*OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT1Subset)
anova(RHamp.CRR1model.byTrial)

RHamp.CRR1model_gtint3w.byTrial<-lme(RHcrsPeakAmp ~ Group+Timepoint+AvgFileOVAlpha+OVAlphafromAvg+Group*Timepoint+Group*AvgFileOVAlpha+Group*OVAlphafromAvg+Timepoint*AvgFileOVAlpha+Timepoint*OVAlphafromAvg+Group*Timepoint*OVAlphafromAvg+Group*Timepoint*AvgFileOVAlpha,random=~OVAlphafromAvg|PtID, data =RT1Subset)
anova(RHamp.CRR1model.byTrial)

#R2 RHamps predicted by Trial-to-Trial Alpha
RHamp.CRR2model.byTrial<-lme(RHcrsPeakAmp ~ AvgFileOVAlpha+OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT2Subset)
anova(RHamp.CRR2model.byTrial)

RHamp.CRR2model_int.byTrial<-lme(RHcrsPeakAmp ~ AvgFileOVAlpha+OVAlphafromAvg+AvgFileOVAlpha*OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT2Subset)
anova(RHamp.CRR2model_int.byTrial)

RHamp.CRR2model_gt.byTrial<-lme(RHcrsPeakAmp ~ Timepoint+AvgFileOVAlpha+OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT2Subset)
anova(RHamp.CRR2model_gt.byTrial)

RHamp.CRR2model_gtint.byTrial<-lme(RHcrsPeakAmp ~ Timepoint+AvgFileOVAlpha+OVAlphafromAvg+Timepoint+AvgFileOVAlpha+OVAlphafromAvg+Timepoint*AvgFileOVAlpha+Timepoint*OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT2Subset)
anova(RHamp.CRR2model.byTrial)

#is OVerall PRstimulus alpha a predictor of LH amp based on trial by trial fluctuations?
LHamp.CRR1model.byTrial<-lme(LHcrsPeakAmp ~ AvgFileOVAlpha+OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT1Subset)
anova(LHamp.CRR1model.byTrial)

LHamp.CRR1model_int.byTrial<-lme(LHcrsPeakAmp ~ AvgFileOVAlpha+OVAlphafromAvg+AvgFileOVAlpha*OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT1Subset)
anova(LHamp.CRR1model_int.byTrial)

LHamp.CRR1model_gt.byTrial<-lme(LHcrsPeakAmp ~ Group+Timepoint+AvgFileOVAlpha+OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT1Subset)
anova(LHamp.CRR1model_gt.byTrial)

LHamp.CRR1model_gtint.byTrial<-lme(LHcrsPeakAmp ~ Group+Timepoint+AvgFileOVAlpha+OVAlphafromAvg+Group*AvgFileOVAlpha+Group*OVAlphafromAvg+Timepoint*AvgFileOVAlpha+Timepoint*OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT1Subset)
anova(LHamp.CRR1model.byTrial)

LHamp.CRR1model_gtint3w.byTrial<-lme(LHcrsPeakAmp ~ Group+Timepoint+AvgFileOVAlpha+OVAlphafromAvg+Group*Timepoint+Group*AvgFileOVAlpha+Group*OVAlphafromAvg+Timepoint*AvgFileOVAlpha+Timepoint*OVAlphafromAvg+Group*Timepoint*OVAlphafromAvg+Group*Timepoint*AvgFileOVAlpha,random=~OVAlphafromAvg|PtID, data =RT1Subset)
anova(LHamp.CRR1model.byTrial)

#R2 LH
#is OVerall PRstimulus alpha a predictor of LH amp based on trial by trial fluctuations?
LHamp.CRR2model.byTrial<-lme(LHcrsPeakAmp ~ AvgFileOVAlpha+OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT2Subset,control=lmeControl(opt = "optim"))
anova(LHamp.CRR1model.byTrial)

LHamp.CRR2model_int.byTrial<-lme(LHcrsPeakAmp ~ AvgFileOVAlpha+OVAlphafromAvg+AvgFileOVAlpha*OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT2Subset)
anova(LHamp.CRR1model.byTrial)

LHamp.CRR2model_gt.byTrial<-lme(LHcrsPeakAmp ~ Timepoint+AvgFileOVAlpha+OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT2Subset,control=lmeControl(opt = "optim"))
anova(LHamp.CRR1model.byTrial)

LHamp.CRR2model_gtint.byTrial<-lme(LHcrsPeakAmp ~ Timepoint+AvgFileOVAlpha+OVAlphafromAvg+Timepoint*AvgFileOVAlpha+Timepoint*OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT2Subset)
anova(LHamp.CRR1model.byTrial)

LHamp.CRR2model_gtint3w.byTrial<-lme(LHcrsPeakAmp ~ Timepoint+AvgFileOVAlpha+OVAlphafromAvg+Timepoint*AvgFileOVAlpha+Timepoint*OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT2Subset)
anova(LHamp.CRR1model.byTrial)

#is OVerall PRstimulus alpha a predictor of RH Lat based on trial by trial fluctuations?
RHLat.CRR1model.byTrial<-lme(RHcrsPeakLat ~ AvgFileOVAlpha+OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT1Subset)
anova(RHLat.CRR1model.byTrial)

RHLat.CRR1model_int.byTrial<-lme(RHcrsPeakLat ~ AvgFileOVAlpha+OVAlphafromAvg+AvgFileOVAlpha*OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT1Subset)
anova(RHLat.CRR1model.byTrial)

RHLat.CRR1model_gt.byTrial<-lme(RHcrsPeakLat ~ Group+Timepoint+AvgFileOVAlpha+OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT1Subset,control=lmeControl(opt = "optim"))
anova(RHLat.CRR1model.byTrial)

RHLat.CRR1model_gtint.byTrial<-lme(RHcrsPeakLat ~ Group+Timepoint+AvgFileOVAlpha+OVAlphafromAvg+Group*Timepoint+Group*AvgFileOVAlpha+Group*OVAlphafromAvg+Timepoint*AvgFileOVAlpha+Timepoint*OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT1Subset)
anova(RHLat.CRR1model.byTrial)

RHLat.CRR1model_gtint3w.byTrial<-lme(RHcrsPeakLat ~ Group+Timepoint+AvgFileOVAlpha+OVAlphafromAvg+Group*Timepoint+Group*AvgFileOVAlpha+Group*OVAlphafromAvg+Timepoint*AvgFileOVAlpha+Timepoint*OVAlphafromAvg+Group*Timepoint*OVAlphafromAvg+Group*Timepoint*AvgFileOVAlpha,random=~OVAlphafromAvg|PtID, data =RT1Subset)
anova(RHLat.CRR1model.byTrial)

#R2 RHLats predicted by Trial-to-Trial Alpha
RHLat.CRR2model.byTrial<-lme(RHcrsPeakLat ~ AvgFileOVAlpha+OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT2Subset,control=lmeControl(opt = "optim"))
anova(RHLat.CRR2model.byTrial)

RHLat.CRR2model_int.byTrial<-lme(RHcrsPeakLat ~ AvgFileOVAlpha+OVAlphafromAvg+AvgFileOVAlpha*OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT2Subset)
anova(RHLat.CRR2model_int.byTrial)

RHLat.CRR2model_gt.byTrial<-lme(RHcrsPeakLat ~ Timepoint+AvgFileOVAlpha+OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT2Subset)
anova(RHLat.CRR2model_gt.byTrial)

RHLat.CRR2model_gtint.byTrial<-lme(RHcrsPeakLat ~ Timepoint+AvgFileOVAlpha+OVAlphafromAvg+Timepoint+AvgFileOVAlpha+OVAlphafromAvg+Timepoint*AvgFileOVAlpha+Timepoint*OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT2Subset)
anova(RHLat.CRR2model.byTrial)

#is OVerall PRstimulus alpha a predictor of LH Lat based on trial by trial fluctuations?
LHLat.CRR1model.byTrial<-lme(LHcrsPeakLat ~ AvgFileOVAlpha+OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT1Subset,control=lmeControl(opt = "optim"))
anova(LHLat.CRR1model.byTrial)

LHLat.CRR1model_int.byTrial<-lme(LHcrsPeakLat ~ AvgFileOVAlpha+OVAlphafromAvg+AvgFileOVAlpha*OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT1Subset)
anova(LHLat.CRR1model_int.byTrial)

LHLat.CRR1model_gt.byTrial<-lme(LHcrsPeakLat ~ Group+Timepoint+AvgFileOVAlpha+OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT1Subset)
anova(LHLat.CRR1model_gt.byTrial)

LHLat.CRR1model_gtint.byTrial<-lme(LHcrsPeakLat ~ Group+Timepoint+AvgFileOVAlpha+OVAlphafromAvg+Group*AvgFileOVAlpha+Group*OVAlphafromAvg+Timepoint*AvgFileOVAlpha+Timepoint*OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT1Subset)
anova(LHLat.CRR1model.byTrial)

LHLat.CRR1model_gtint3w.byTrial<-lme(LHcrsPeakLat ~ Group+Timepoint+AvgFileOVAlpha+OVAlphafromAvg+Group*Timepoint+Group*AvgFileOVAlpha+Group*OVAlphafromAvg+Timepoint*AvgFileOVAlpha+Timepoint*OVAlphafromAvg+Group*Timepoint*OVAlphafromAvg+Group*Timepoint*AvgFileOVAlpha,random=~OVAlphafromAvg|PtID, data =RT1Subset,control=lmeControl(opt = "optim"))
anova(LHLat.CRR1model.byTrial)

#R2 LH
#How predictive of LH Lat are trial-by-trial fluctuations in OV prestimulus alpha?
LHLat.CRR2model.byTrial<-lme(LHcrsPeakLat ~ AvgFileOVAlpha+OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT2Subset,control=lmeControl(opt = "optim"))
anova(LHLat.CRR1model.byTrial)

LHLat.CRR2model_int.byTrial<-lme(LHcrsPeakLat ~ AvgFileOVAlpha+OVAlphafromAvg+AvgFileOVAlpha*OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT2Subset)
anova(LHLat.CRR1model.byTrial)

LHLat.CRR2model_gt.byTrial<-lme(LHcrsPeakLat ~ Timepoint+AvgFileOVAlpha+OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT2Subset,control=lmeControl(opt = "optim"))
anova(LHLat.CRR1model.byTrial)

LHLat.CRR2model_gtint.byTrial<-lme(LHcrsPeakLat ~ Timepoint+AvgFileOVAlpha+OVAlphafromAvg+Timepoint*AvgFileOVAlpha+Timepoint*OVAlphafromAvg,random=~OVAlphafromAvg|PtID, data =RT2Subset)
anova(LHLat.CRR1model.byTrial)



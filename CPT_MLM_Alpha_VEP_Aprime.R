#Load the libs
library(lme4)
library(nlme)

#Is Overall Alpha predicted by Group, timepoint, and block as a fixed and random factor among participants?
#R1 Participants
# The lme4 version of the model equation:
#OVAlpha.CRR1model<-lmer(CRRHampblocks ~ Group + Timepoint + Block+(Block|PtID)+Group*Timepoint, data =CRR1Groups, REML = TRUE)
#but the nlme version of the model equation provides p-values so we'll use it for easier interpretation:

OVAlpha.CRR1model_gt<-lme(OV1sPreAlpha ~ Group + Timepoint,random=~1|PtID, data =CRR1Groups)
anova(OVAlpha.CRR1model_gt)

OVAlpha.CRR1model_gtint<-lme(OV1sPreAlpha ~ Group + Timepoint+Group*Timepoint,random=~1|PtID, data =CRR1Groups)
anova(OVAlpha.CRR1model_gtint)

OVAlpha.CRR1model_gtb<-lme(OV1sPreAlpha ~ Group + Timepoint + as.numeric(Block),random=~1|PtID, data =CRR1Groups)
anova(OVAlpha.CRR1model_gtb)

OVAlpha.CRR1model_gtbint<-lme(OV1sPreAlpha ~ Group + Timepoint + as.numeric(Block)+Group*Timepoint*Block,random=~1|PtID, data =CRR1Groups,control=lmeControl(opt = "optim"))
anova(OVAlpha.CRR1model_gtbint)
#R2 Participants lme4 version:
#OVAlpha.CRR2model<-lmer(OV1sPreAlpha ~ Timepoint + Block+(Block|PtID), data =CRR2Groups, REML = TRUE)
#nlme version of the R2 model:
OVAlpha.CRR2model_gt<-lme(OV1sPreAlpha ~ Timepoint, random = ~1|PtID,data=CRR2Groups)
anova(OVAlpha.CRR2model_gt)

OVAlpha.CRR2model_gtb<-lme(OV1sPreAlpha ~ Timepoint + as.numeric(Block), random = ~1|PtID,data=CRR2Groups)
anova(OVAlpha.CRR2model_gtb)

OVAlpha.CRR2model_gtbint<-lme(OV1sPreAlpha ~ Timepoint + as.numeric(Block)+Timepoint*Block, random = ~1|PtID,data=CRR2Groups)
anova(OVAlpha.CRR2model_gtbint)


#Does OVerall RH ERP amps for CRs change across the retreat period?
RHamp.CRR1model_gt<-lme(CRRHampblocks ~ Group + Timepoint,random=~1|PtID, data =CRR1Groups)
anova(RHamp.CRR1model_gt)

RHamp.CRR1model_gtint<-lme(CRRHampblocks ~ Group + Timepoint+Group*Timepoint,random=~1|PtID, data =CRR1Groups)
anova(RHamp.CRR1model_gtint)


RHamp.CRR1model_gtb<-lme(CRRHampblocks ~ Group + Timepoint + as.numeric(Block),random=~1|PtID, data =CRR1Groups)
anova(RHamp.CRR1model_gtb)

RHamp.CRR1model_gtbint<-lme(CRRHampblocks ~ Group + Timepoint + as.numeric(Block)+Group*Timepoint+Group*Block+Timepoint*Block+Group*Timepoint*Block,random=~1|PtID, data =CRR1Groups)
anova(RHamp.CRR1model_gtbint)

#nlme version of the R2 model:
RHamp.CRR2model_gt<-lme(CRRHampblocks ~ Timepoint, random = ~1|PtID,data=CRR2Groups)
anova(RHamp.CRR2model_gt)

RHamp.CRR2model_gtb<-lme(CRRHampblocks ~ Timepoint + as.numeric(Block), random = ~1|PtID,data=CRR2Groups)
anova(RHamp.CRR2model_gtb)

RHamp.CRR2model_gtbint<-lme(CRRHampblocks ~ Timepoint + as.numeric(Block)+Timepoint*Block, random = ~1|PtID,data=CRR2Groups)
anova(RHamp.CRR2model_gtbint)

#Does OVerall LH ERP amps for CRs change across the retreat period?
LHamp.CRR1model_gt<-lme(CRLHampblocks ~ Group + Timepoint,random=~1|PtID, data =CRR1Groups)
anova(LHamp.CRR1model_gt)

LHamp.CRR1model_gtint<-lme(CRLHampblocks ~ Group + Timepoint+Group*Timepoint,random=~1|PtID, data =CRR1Groups)
anova(LHamp.CRR1model_gtint)

LHamp.CRR1model_gtb<-lme(CRLHampblocks ~ Group + Timepoint + as.numeric(Block),random=~1|PtID, data =CRR1Groups)
anova(LHamp.CRR1model_gtb)

LHamp.CRR1model_gtbint<-lme(CRLHampblocks ~ Group + Timepoint + as.numeric(Block)+Group*Timepoint+Group*Block+Timepoint*Block+Group*Timepoint*Block,random=~1|PtID, data =CRR1Groups)
anova(LHamp.CRR1model_gtbint)

#nlme version of the R2 model:
LHamp.CRR2model_gt<-lme(CRLHampblocks ~ Timepoint, random = ~1|PtID,data=CRR2Groups)
anova(LHamp.CRR2model_gt)

LHamp.CRR2model_gtb<-lme(CRLHampblocks ~ Timepoint + as.numeric(Block), random = ~1|PtID,data=CRR2Groups)
anova(LHamp.CRR2model_gtb)

LHamp.CRR2model_gtbint<-lme(CRLHampblocks ~ Timepoint + as.numeric(Block)+Timepoint*Block, random = ~1|PtID,data=CRR2Groups)
anova(LHamp.CRR2model_gtbint)

#Does OVerall Aprime change across the retreat period?
Aprime.CRR1model_gt<-lme(CRAprimeblocks ~ Group + Timepoint,random=~1|PtID, data =CRR1Groups)
anova(Aprime.CRR1model_gt)

Aprime.CRR1model_gtint<-lme(CRAprimeblocks ~ Group + Timepoint+Group*Timepoint,random=~1|PtID, data =CRR1Groups)
anova(Aprime.CRR1model_gtint)

Aprime.CRR1model_gtb<-lme(CRAprimeblocks ~ Group + Timepoint + as.numeric(Block),random=~1|PtID, data =CRR1Groups)
anova(Aprime.CRR1model_gtb)

Aprime.CRR1model_gtbint<-lme(CRAprimeblocks ~ Group + Timepoint + as.numeric(Block)+Group*Timepoint+Group*Block+Timepoint*Block+Group*Timepoint*Block,random=~1|PtID, data =CRR1Groups)
anova(Aprime.CRR1modelgtbint)

#nlme version of the R2 model:
Aprime.CRR2model_gt<-lme(CRAprimeblocks ~ Timepoint, random = ~1|PtID,data=CRR2Groups)
anova(Aprime.CRR2model_gt)

Aprime.CRR2model_gtb<-lme(CRAprimeblocks ~ Timepoint + as.numeric(Block), random = ~1|PtID,data=CRR2Groups)
anova(Aprime.CRR2model_gtb)

Aprime.CRR2model_gtbint<-lme(CRAprimeblocks ~ Timepoint + as.numeric(Block)+Timepoint*Block, random = ~1|PtID,data=CRR2Groups)
anova(Aprime.CRR2model)

#Does Alpha Predict RH VEP? Yes, in both retreats
Alpha.RHVEP.R1<-lme(CRRHampblocks ~ OV1sPreAlpha, random=~1|PtID, data=CRR1Groups)
Alpha.RHVEP.R2<-lme(CRRHampblocks ~ OV1sPreAlpha, random=~1|PtID, data=CRR2Groups)

Alpha.RHVEP.R1_gt<-lme(CRRHampblocks ~ Group + Timepoint + OV1sPreAlpha, random=~1|PtID, data=CRR1Groups)
Alpha.RHVEP.R2_gt<-lme(CRRHampblocks ~ Timepoint + OV1sPreAlpha, random=~1|PtID, data=CRR2Groups)

Alpha.RHVEP.R1_gtint<-lme(CRRHampblocks ~ Group + Timepoint + OV1sPreAlpha+Group*Timepoint*OV1sPreAlpha, random=~1|PtID, data=CRR1Groups)
Alpha.RHVEP.R2_gtint<-lme(CRRHampblocks ~ Timepoint + OV1sPreAlpha+Timepoint*OV1sPreAlpha, random=~1|PtID, data=CRR2Groups)

#Does Alpha Predict LH VEP?
Alpha.LHVEP.R1<-lme(CRLHampblocks ~ OV1sPreAlpha, random=~1|PtID, data=CRR1Groups)
Alpha.LHVEP.R2<-lme(CRLHampblocks ~ OV1sPreAlpha, random=~1|PtID, data=CRR2Groups)

Alpha.LHVEP.R1_gt<-lme(CRLHampblocks ~ Group + Timepoint + OV1sPreAlpha, random=~1|PtID, data=CRR1Groups)
Alpha.LHVEP.R2_gt<-lme(CRLHampblocks ~ Timepoint + OV1sPreAlpha, random=~1|PtID, data=CRR2Groups)

Alpha.LHVEP.R1_gtint<-lme(CRLHampblocks ~ Group + Timepoint + OV1sPreAlpha+Group*Timepoint*OV1sPreAlpha, random=~1|PtID, data=CRR1Groups)
Alpha.LHVEP.R2_gtint<-lme(CRLHampblocks ~ Timepoint + OV1sPreAlpha+Timepoint*OV1sPreAlpha, random=~1|PtID, data=CRR2Groups)

#Does Alpha Predict Aprime? Yes-ish (p=.056) in R1. Not in R2
Alpha.Aprime.R1<-lme(CRAprimeblocks ~ OV1sPreAlpha, random=~1|PtID, data=CRR1Groups)
Alpha.Aprime.R2<-lme(CRAprimeblocks ~ OV1sPreAlpha, random=~1|PtID, data=CRR2Groups)

Alpha.Aprime.R1_gt<-lme(CRAprimeblocks ~ Group + Timepoint + OV1sPreAlpha, random=~1|PtID, data=CRR1Groups)
Alpha.Aprime.R2_gt<-lme(CRAprimeblocks ~ Timepoint + OV1sPreAlpha, random=~1|PtID, data=CRR2Groups)

Alpha.Aprime.R1_gtint<-lme(CRAprimeblocks ~ Group + Timepoint +OV1sPreAlpha +Group*Timepoint*OV1sPreAlpha, random=~1|PtID, data=CRR1Groups)
Alpha.Aprime.R2_gtint<-lme(CRAprimeblocks ~ Timepoint + OV1sPreAlpha+Timepoint*OV1sPreAlpha, random=~1|PtID, data=CRR2Groups)

#Does RH VEP Predict Aprime? No. In Neither Retreat
RHVEP.Aprime.R1<-lme(CRAprimeblocks ~ CRRHampblocks, random=~1|PtID, data=CRR1Groups)
RHVEP.Aprime.R2<-lme(CRAprimeblocks ~ CRRHampblocks, random=~1|PtID, data=CRR2Groups)

RHVEP.Aprime.R1_gt<-lme(CRAprimeblocks ~ Group + Timepoint + CRRHampblocks, random=~1|PtID, data=CRR1Groups)
RHVEP.Aprime.R2_gt<-lme(CRAprimeblocks ~ Timepoint + CRRHampblocks, random=~1|PtID, data=CRR2Groups)

RHVEP.Aprime.R1_gtint<-lme(CRAprimeblocks ~ Group + Timepoint + CRRHampblocks + Group*Timepoint*CRRHampblocks, random=~1|PtID, data=CRR1Groups)
RHVEP.Aprime.R2_gtint<-lme(CRAprimeblocks ~ Timepoint + CRRHampblocks + Timepoint*CRRHampblocks, random=~1|PtID, data=CRR2Groups)

#Does LH VEP Predict Aprime? Maybe in R1 (p=.07), definitely not in R2.
LHVEP.Aprime.R1<-lme(CRAprimeblocks ~ CRLHampblocks, random=~1|PtID, data=CRR1Groups)
LHVEP.Aprime.R2<-lme(CRAprimeblocks ~ CRLHampblocks, random=~1|PtID, data=CRR2Groups)

LHVEP.Aprime.R1_gt<-lme(CRAprimeblocks ~ Group + Timepoint + CRLHampblocks, random=~1|PtID, data=CRR1Groups)
LHVEP.Aprime.R2_gt<-lme(CRAprimeblocks ~ Timepoint + CRLHampblocks, random=~1|PtID, data=CRR2Groups)

LHVEP.Aprime.R1_gtint<-lme(CRAprimeblocks ~ Group + Timepoint + CRLHampblocks + Group*Timepoint*CRLHampblocks, random=~1|PtID, data=CRR1Groups)
LHVEP.Aprime.R2_gtint<-lme(CRAprimeblocks ~ Timepoint + CRLHampblocks + Timepoint*CRLHampblocks, random=~1|PtID, data=CRR2Groups)


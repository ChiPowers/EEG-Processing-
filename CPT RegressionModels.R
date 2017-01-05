## These commands run mixed factor regression models of the CPTdata

##load the libs
library(lme4)
library(arm)
library(lattice)

#Final Aprime Regression model with Group, Timepoint, and Block as fixed factors, and block included as a random factor in participants.
OVAlpha.model<-lmer(OV1sPreAlpha ~ Group + Timepoint + Block + (1|PtID) + (1|Block) + Group * Timepoint + Group * Block + Timepoint * Block + Group*Timepoint*Block, data = AllCR.trim, REML = TRUE)

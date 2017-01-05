#Code for setting up and analyzing CPT data.
#Set up factors

CPTdata$Group<-factor(CPTdata$Group, levels = c("0","1","2"),labels = c("Control","R1Trainee","R2Trainee"))
CPTdata$Timepoint<-factor(CPTdata$Timepoint,levels = c("1","2","3"), labels = c("Pre","Mid","Post"))
CPTdata$Retreat <-factor(CPTdata$Retreat,levels = c("1","2"), labels = c("R1","R2"))
CPTdata$Type<-factor(CPTdata$Type,levels=c("2","4"), labels = c("Nontarget","Target"))
CPTdata$Block<-factor(CPTdata$Block,levels=c("1","2","3","4","5","6","7","8"))
CPTdata$BCond<-factor(CPTdata$Bcond, levels=c("1","2","3","4"), labels = c("Hit", "CorrRej","Miss","FA"))
CPTdata$PrevTrialType<-factor(CPTdata$PrevTrialType,levels=c("1","2","3","4"),labels = c("Hit", "CorrRej","Miss","FA"))


OVAlpha.model<-lmer(OV1sPreAlpha ~ Group + Timepoint + (1|Filename) +Group * Timepoint, data = CPTdata, REML = TRUE)
HiAlpha.model<-lmer(Hi1sPreAlpha~ Group + Timepoint + (1|Filename) +Group *Timepoint, data = CPTdata, REML = TRUE)
#load libs
library(ggplot2)

##Box Plots of all of the variables by timepoint and Group.
#Correct Rejections
#Overall Alpha
OVAlphaMeans<-aggregate(OV1sPreAlpha ~ Group + Timepoint + Block + Retreat, data = CRblockData, mean )
OVAlphaplot<-ggplot(OVAlphaMeans) +geom_boxplot(aes(Group, OV1sPreAlpha, color = Group)) + facet_grid(. ~ Timepoint)
OVAlphaplot + ggtitle("CR Overall Alpha by Group and Timepoint")
OVAlphaByBlock<-ggplot(CROVAlphablocks)+geom_boxplot(aes(Block, OV1sPreAlpha, color = Group)) + facet_grid(Timepoint~.)
OVAlphaByBlock + ggtitle("CR OVerall Alpha by Group By Timepoint By Block")
#Line Graph
OVAlphaByBlock.1<-ggplot(OVAlphaMeans, aes(Block, OV1sPreAlpha, group = Group, color = Group))+ geom_line() + geom_point()+facet_grid(Timepoint~.)
OVAlphaByBlock.1 + ggtitle("CR Overall Alpha by Group By Timepoint By Block")

#hi Alpha
hiAlphaMeans<-aggregate(CRHiAlphablocks ~ Group + Timepoint + Block + Retreat, data = CRblockData, mean )
hiAlphaplot<-ggplot(hiAlphaMeans) +geom_boxplot(aes(Group, CRHiAlphablocks, color = Group)) + facet_grid(. ~ Timepoint)
hiAlphaplot + ggtitle("CR Hi Alpha by Group and Timepoint")
HiAlphaByBlock<-ggplot(CRHiAlphablocks)+geom_boxplot(aes(Block, Hi1sPreAlpha, color = Group)) + facet_grid(Timepoint~.)
HiAlphaByBlock + ggtitle("CR Hi Alpha by Group By Timepoint By Block")
#Line Graph
HiAlphaByBlock.1<-ggplot(hiAlphaMeans, aes(Block, CRHiAlphablocks, group = Group, color = Group))+ geom_line() + geom_point()+facet_grid(Timepoint~.)
HiAlphaByBlock.1 + ggtitle("CR Hi Alpha by Group By Timepoint By Block")

#low Alpha
lowAlphaMeans<-aggregate(CRLoAlphablocks ~ Group + Timepoint + Block + Retreat, data = CRblockData, mean )
lowAlphaplot<-ggplot(lowAlphaMeans) +geom_boxplot(aes(Group, CRLoAlphablocks, color = Group)) + facet_grid(. ~ Timepoint)
lowAlphaplot + ggtitle("CR Low Alpha by Group and Timepoint")
LoAlphaByBlock<-ggplot(CRLoAlphablocks)+geom_boxplot(aes(Block, Low1sPreAlpha, color = Group)) + facet_grid(Timepoint~.)
LoAlphaByBlock + ggtitle("CR Low Alpha by Group By Timepoint By Block")
#Line Graph
LoAlphaByBlock.1<-ggplot(lowAlphaMeans, aes(Block, CRLoAlphablocks, group = Group, color = Group))+ geom_line() + geom_point()+facet_grid(Timepoint~.)
LoAlphaByBlock.1 + ggtitle("CR Hi Alpha by Group By Timepoint By Block")

#Overall Alpha
IAPMeans<-aggregate(CRIAPblocks ~ Group + Timepoint + Block + Retreat, data = CRblockData, mean )
IAPplot<-ggplot(IAPMeans) +geom_boxplot(aes(Group, CRIAPblocks, color = Group)) + facet_grid(. ~ Timepoint)
IAPplot + ggtitle("CR IAP by Group and Timepoint")
IAPByBlock<-ggplot(CRIAPblocks)+geom_boxplot(aes(Block, IAP, color = Group)) + facet_grid(Timepoint~.)
IAPByBlock + ggtitle("CR IAP by Group By Timepoint By Block")
#Line Graph
IAPByBlock.1<-ggplot(IAPMeans, aes(Block, CRIAPblocks, group = Group, color = Group))+ geom_line() + geom_point()+facet_grid(Group~.)
IAPByBlock.1 + ggtitle("CR IAP by Group By Timepoint By Block")

#Amplitudes
#RH
RHampMeans<-aggregate(CRRHampblocks ~ Group + Timepoint + Block + Retreat, data = CRblockData, mean )
RHampplot<-ggplot(RHampMeans) +geom_boxplot(aes(Group, CRRHampblocks, color = Group)) + facet_grid(. ~ Timepoint)
RHampplot + ggtitle("CR Peak Amplitudes among RH Electrodes by Group and Timepoint")
RHampByBlock<-ggplot(CRRHLatblocks)+geom_boxplot(aes(Block, RHcrsPeakLat, color = Group)) + facet_grid(Timepoint~.)
RHampByBlock + ggtitle("CR Peak Amplitudes among RH Electrodes by Group By Timepoint By Block")
#Line Graph
RHampByBlock.1<-ggplot(RHampMeans, aes(Block, CRRHampblocks, group = Timepoint, color = Timepoint))+ geom_line() + geom_point()+facet_grid(Group~.)
RHampByBlock.1 + ggtitle("CR Peak Amplitudes among RH Electrodes by Group By Timepoint By Block")

#LH
LHampMeans<-aggregate(CRLHampblocks ~ Group + Timepoint + Block + Retreat, data = CRblockData, mean )
LHampplot<-ggplot(LHampMeans) +geom_boxplot(aes(Group, CRLHampblocks, color = Group)) + facet_grid(. ~ Timepoint)
LHampplot + ggtitle("CR Peak Amplitudes among LH Electrodes by Group and Timepoint")
LHampByBlock<-ggplot(CRLHLatblocks)+geom_boxplot(aes(Block, LHcrsPeakLat, color = Group)) + facet_grid(Timepoint~.)
LHampByBlock + ggtitle("CR Peak Amplitudes among LH Electrodes by Group By Timepoint By Block")
#Line Graph
LHampByBlock.1<-ggplot(LHampMeans, aes(Block, CRLHampblocks, group = Group, color = Group))+ geom_line() + geom_point()+facet_grid(Timepoint~.)
LHampByBlock.1 + ggtitle("CR Peak Amplitudes among LH Electrodes by Group By Timepoint By Block")


#All
AllampMeans<-aggregate(CRAllampblocks ~ Group + Timepoint + Block + Retreat, data = CRblockData, mean )
Allampplot<-ggplot(AllampMeans) +geom_boxplot(aes(Group, CRAllampblocks, color = Group)) + facet_grid(. ~ Timepoint)
Allampplot + ggtitle("CR Peak Amplitudes among All Electrodes by Group and Timepoint")
AllampByBlock<-ggplot(CRAllampblocks)+geom_boxplot(aes(Block, AllcrsPeakAmp, color = Group)) + facet_grid(Timepoint~.)
AllampByBlock + ggtitle("CR Peak Amplitudes among All Electrodes by Group By Timepoint By Block")
#Line Graph
AllampByBlock.1<-ggplot(AllampMeans, aes(Block, CRAllampblocks, group = Group, color = Group))+ geom_line() + geom_point()+facet_grid(Timepoint~.)
AllampByBlock.1 + ggtitle("CR Peak Amplitudes among All Electrodes by Group By Timepoint By Block")

#Latencies
#RH
RHLatMeans<-aggregate(CRRHLatblocks ~ Group + Timepoint + Block + Retreat, data = CRblockData, mean )
RHLatplot<-ggplot(RHLatMeans) +geom_boxplot(aes(Group, CRRHLatblocks, color = Group)) + facet_grid(. ~ Timepoint)
RHLatplot + ggtitle("CR Peak Latencies among RH Electrodes by Group and Timepoint")
RHLatByBlock<-ggplot(CRRHLatblocks)+geom_boxplot(aes(Block, RHcrsPeakLat, color = Group)) + facet_grid(Timepoint~.)
RHLatByBlock + ggtitle("CR Peak Latencies among RH Electrodes by Group By Timepoint By Block")
#Line Graph
RHLatByBlock.1<-ggplot(RHLatMeans, aes(Block, CRRHLatblocks, group = Group, color = Group))+ geom_line() + geom_point()+facet_grid(Timepoint~.)
RHLatByBlock.1 + ggtitle("CR Peak Latencies among RH Electrodes by Group By Timepoint By Block")


#LH
LHLatMeans<-aggregate(CRLHLatblocks ~ Group + Timepoint + Block + Retreat, data = CRblockData, mean )
LHLatplot<-ggplot(LHLatMeans) +geom_boxplot(aes(Group, CRLHLatblocks, color = Group)) + facet_grid(. ~ Timepoint)
LHLatplot + ggtitle("CR Peak Latencies among LH Electrodes by Group and Timepoint")
LHLatByBlock<-ggplot(CRLHLatblocks)+geom_boxplot(aes(Block, LHcrsPeakLat, color = Group)) + facet_grid(Timepoint~.)
LHLatByBlock + ggtitle("CR Peak Latencies among LH Electrodes by Group By Timepoint By Block")
#Line Graph
LHLatByBlock.1<-ggplot(LHLatMeans, aes(Block, CRLHLatblocks, group = Group, color = Group))+ geom_line() + geom_point()+facet_grid(Timepoint~.)
LHLatByBlock.1 + ggtitle("CR Peak Latencies among LH Electrodes by Group By Timepoint By Block")

#All
AllLatMeans<-aggregate(CRAllLatblocks ~ Group + Timepoint + Block + Retreat, data = CRblockData, mean )
AllLatplot<-ggplot(AllLatMeans) +geom_boxplot(aes(Group, CRAllLatblocks, color = Group)) + facet_grid(. ~ Timepoint)
AllLatplot + ggtitle("CR Peak Latencies among All Electrodes by Group and Timepoint")
AllLatByBlock<-ggplot(CRAllLatblocks)+geom_boxplot(aes(Block, AllcrsPeakLat, color = Group)) + facet_grid(Timepoint~.)
AllLatByBlock + ggtitle("CR Peak Latencies among All Electrodes by Group By Timepoint By Block")
#Line Graph
AllLatByBlock.1<-ggplot(AllLatMeans, aes(Block, CRAllLatblocks, group = Group, color = Group))+ geom_line() + geom_point()+facet_grid(Timepoint~.)
AllLatByBlock.1 + ggtitle("CR Peak Latencies among All Electrodes by Group By Timepoint By Block")

#Hits
#Overall Alpha
OVAlphaMeans<-aggregate(OV1sPreAlpha ~ Group + Timepoint + Block + Retreat, data = HitblockData, mean )
OVAlphaplot<-ggplot(OVAlphaMeans) +geom_boxplot(aes(Group, OV1sPreAlpha, color = Group)) + facet_grid(. ~ Timepoint)
OVAlphaplot + ggtitle("Hit Overall Alpha by Group and Timepoint")
HitOVAlphaByBlock<-ggplot(HitOVAlphablocks)+geom_boxplot(aes(Block, OV1sPreAlpha, color = Group)) + facet_grid(Timepoint~.)
HitOVAlphaByBlock + ggtitle("Hit OVerall Alpha by Group By Timepoint By Block")
#Line Graph
HitOVAlphaByBlock.1<-ggplot(OVAlphaMeans, aes(Block, OV1sPreAlpha, group = Group, color = Group))+ geom_line() + geom_point()+facet_grid(Timepoint~.)
HitOVAlphaByBlock.1 + ggtitle("Hit Overall Alpha by Group By Timepoint By Block")

#hi Alpha
hiAlphaMeans<-aggregate(HitHiAlphablocks ~ Group + Timepoint + Block + Retreat, data = HitblockData, mean )
hiAlphaplot<-ggplot(hiAlphaMeans) +geom_boxplot(aes(Group, HitHiAlphablocks, color = Group)) + facet_grid(. ~ Timepoint)
hiAlphaplot + ggtitle("Hit Hi Alpha by Group and Timepoint")
HithiAlphaByBlock<-ggplot(HitHiAlphablocks)+geom_boxplot(aes(Block, Hi1sPreAlpha, color = Group)) + facet_grid(Timepoint~.)
HithiAlphaByBlock + ggtitle("Hit Hi Alpha by Group By Timepoint By Block")
#Line Graph
HithiAlphaByBlock.1<-ggplot(hiAlphaMeans, aes(Block, HitHiAlphablocks, group = Group, color = Group))+ geom_line() + geom_point()+facet_grid(Timepoint~.)
HithiAlphaByBlock.1 + ggtitle("Hit Hi Alpha by Group By Timepoint By Block")

#low Alpha
lowAlphaMeans<-aggregate(HitLoAlphablocks ~ Group + Timepoint + Block + Retreat, data = HitblockData, mean )
lowAlphaplot<-ggplot(lowAlphaMeans) +geom_boxplot(aes(Group, HitLoAlphablocks, color = Group)) + facet_grid(. ~ Timepoint)
lowAlphaplot + ggtitle("Hit Low Alpha by Group and Timepoint")
HitloAlphaByBlock<-ggplot(HitLoAlphablocks)+geom_boxplot(aes(Block, Low1sPreAlpha, color = Group)) + facet_grid(Timepoint~.)
HitloAlphaByBlock + ggtitle("Hit Low Alpha by Group By Timepoint By Block")
#Line Graph
HitloAlphaByBlock.1<-ggplot(lowAlphaMeans, aes(Block, HitLoAlphablocks, group = Group, color = Group))+ geom_line() + geom_point()+facet_grid(Timepoint~.)
HitloAlphaByBlock.1 + ggtitle("Hit Hi Alpha by Group By Timepoint By Block")

#IAP
IAPMeans<-aggregate(HitIAPblocks ~ Group + Timepoint + Block + Retreat, data = HitblockData, mean )
IAPplot<-ggplot(IAPMeans) +geom_boxplot(aes(Group, HitIAPblocks, color = Group)) + facet_grid(. ~ Timepoint)
IAPplot + ggtitle("Hit IAP by Group and Timepoint")
HitIAPByBlock<-ggplot(HitIAPblocks)+geom_boxplot(aes(Block, IAP, color = Group)) + facet_grid(Timepoint~.)
HitIAPByBlock + ggtitle("Hit IAP by Group By Timepoint By Block")
# #Line Graph
# HitIAPByBlock.1<-ggplot(IAPMeans, aes(Block, HitIAPblocks, group = Group, color = Group))+ geom_line() + geom_point()+facet_grid(Timepoint~.)
# HitIAPByBlock.1 + ggtitle("Hit Hi Alpha by Group By Timepoint By Block")


#Amplitudes
#RH
RHampMeans<-aggregate(HitRHampblocks ~ Group + Timepoint + Block + Retreat, data = HitblockData, mean )
RHampplot<-ggplot(RHampMeans) +geom_boxplot(aes(Group, HitRHampblocks, color = Group)) + facet_grid(. ~ Timepoint)
RHampplot + ggtitle("Hit Peak Amplitudes among RH Electrodes by Group and Timepoint")
HitRHampByBlock<-ggplot(HitRHampblocks)+geom_boxplot(aes(Block, RHhitsPeakAmp, color = Group)) + facet_grid(Timepoint~.)
HitRHampByBlock + ggtitle("Hit Peak Amplitudes among RH Electrodes by Group By Timepoint By Block")
#Line Graph
HitRHampByBlock.1<-ggplot(RHampMeans, aes(Block, HitRHampblocks, group = Timepoint, color = Timepoint))+ geom_line() + geom_point()+facet_grid(Group~.)
HitRHampByBlock.1 + ggtitle("Hit Peak Amplitudes among RH Electrodes by Group By Timepoint By Block")

#LH
LHampMeans<-aggregate(HitLHampblocks ~ Group + Timepoint + Block + Retreat, data = HitblockData, mean )
LHampplot<-ggplot(LHampMeans) +geom_boxplot(aes(Group, HitLHampblocks, color = Group)) + facet_grid(. ~ Timepoint)
LHampplot + ggtitle("Hit Peak Amplitudes among LH Electrodes by Group and Timepoint")
HitLHampByBlock<-ggplot(HitLHampblocks)+geom_boxplot(aes(Block, LHhitsPeakAmp, color = Group)) + facet_grid(Timepoint~.)
HitLHampByBlock + ggtitle("Hit Peak Amplitudes among LH Electrodes by Group By Timepoint By Block")
#Line Graph
HitLHampByBlock.1<-ggplot(LHampMeans, aes(Block, HitLHampblocks, group = Timepoint, color = Timepoint))+ geom_line() + geom_point()+facet_grid(Group~.)
HitLHampByBlock.1 + ggtitle("Hit Peak Amplitudes among LH Electrodes by Group By Timepoint By Block")

#All
AllampMeans<-aggregate(HitAllampblocks ~ Group + Timepoint + Block + Retreat, data = HitblockData, mean )
Allampplot<-ggplot(AllampMeans) +geom_boxplot(aes(Group, HitAllampblocks, color = Group)) + facet_grid(. ~ Timepoint)
Allampplot + ggtitle("Hit Peak Amplitudes among All Electrodes by Group and Timepoint")
HitAllampByBlock<-ggplot(HitAllampblocks)+geom_boxplot(aes(Block, AllhitsPeakAmp, color = Group)) + facet_grid(Timepoint~.)
HitAllampByBlock + ggtitle("Hit Peak Amplitudes among All Electrodes by Group By Timepoint By Block")
#Line Graph
HitAllampByBlock.1<-ggplot(AllampMeans, aes(Block, HitAllampblocks, group = Timepoint, color = Timepoint))+ geom_line() + geom_point()+facet_grid(Group~.)
HitAllampByBlock.1 + ggtitle("Hit Peak Amplitudes among All Electrodes by Group By Timepoint By Block")


#Latencies
#RH
RHLatMeans<-aggregate(HitRHLatblocks ~ Group + Timepoint + Block + Retreat, data = HitblockData, mean )
RHLatplot<-ggplot(RHLatMeans) +geom_boxplot(aes(Group, HitRHLatblocks, color = Group)) + facet_grid(. ~ Timepoint)
RHLatplot + ggtitle("Hit Peak Latencies among RH Electrodes by Group and Timepoint")
HitRHLatByBlock<-ggplot(HitRHLatblocks)+geom_boxplot(aes(Block, RHhitsPeakLat, color = Group)) + facet_grid(Timepoint~.)
HitRHLatByBlock + ggtitle("Hit Peak Latencies among RH Electrodes by Group By Timepoint By Block")
#Line Graph
HitRHLatByBlock.1<-ggplot(RHLatMeans, aes(Block, HitRHLatblocks, group = Group, color = Group))+ geom_line() + geom_point()+facet_grid(Timepoint~.)
HitRHLatByBlock.1 + ggtitle("Hit Peak Latencies among RH Electrodes by Group By Timepoint By Block")


#LH
LHLatMeans<-aggregate(HitLHLatblocks ~ Group + Timepoint + Block + Retreat, data = HitblockData, mean )
LHLatplot<-ggplot(LHLatMeans) +geom_boxplot(aes(Group, HitLHLatblocks, color = Group)) + facet_grid(. ~ Timepoint)
LHLatplot + ggtitle("Hit Peak Latencies among LH Electrodes by Group and Timepoint")
HitLHLatByBlock<-ggplot(HitLHLatblocks)+geom_boxplot(aes(Block, LHhitsPeakLat, color = Group)) + facet_grid(Timepoint~.)
HitLHLatByBlock + ggtitle("Hit Peak Latencies among LH Electrodes by Group By Timepoint By Block")
#Line Graph
HitLHLatByBlock.1<-ggplot(LHLatMeans, aes(Block, HitLHLatblocks, group = Group, color = Group))+ geom_line() + geom_point()+facet_grid(Timepoint~.)
HitLHLatByBlock.1 + ggtitle("Hit Peak Latencies among LH Electrodes by Group By Timepoint By Block")


#All
AllLatMeans<-aggregate(HitAllLatblocks ~ Group + Timepoint + Block + Retreat, data = HitblockData, mean )
AllLatplot<-ggplot(AllLatMeans) +geom_boxplot(aes(Group, HitAllLatblocks, color = Group)) + facet_grid(. ~ Timepoint)
AllLatplot + ggtitle("Hit Peak Latencies among All Electrodes by Group and Timepoint")
HitAllLatByBlock<-ggplot(HitAllLatblocks)+geom_boxplot(aes(Block, AllhitsPeakLat, color = Group)) + facet_grid(Timepoint~.)
HitAllLatByBlock + ggtitle("Hit Peak Latencies among All Electrodes by Group By Timepoint By Block")
#Line Graph
HitAllLatByBlock.1<-ggplot(AllLatMeans, aes(Block, HitAllLatblocks, group = Group, color = Group))+ geom_line() + geom_point()+facet_grid(Timepoint~.)
HitAllLatByBlock.1 + ggtitle("Hit Peak Latencies among All Electrodes by Group By Timepoint By Block")


##RT
RTMeans<-aggregate(RT ~ Group + Timepoint + Block + Retreat, data = HitblockData, mean )
RTplot<-ggplot(RTMeans) +geom_boxplot(aes(Group, RT, color = Group)) + facet_grid(. ~ Timepoint)
RTplot + ggtitle("Hit Response Times by Group and Timepoint")
HitRTByBlock<-ggplot(HitRTblocks)+geom_boxplot(aes(Block, RT, color = Group)) + facet_grid(Timepoint~.)
HitRTByBlock + ggtitle("Hit Response Times by Group By Timepoint By Block")
#Line Graph
HitRTByBlock.1<-ggplot(RTMeans, aes(Block, RT, group = Group, color = Group))+ geom_line() + geom_point()+facet_grid(Timepoint~.)
HitRTByBlock.1 + ggtitle("Hit Response Times by Group By Timepoint By Block")

#Aprime
AprimeMeans<-aggregate(HitAprimeblocks ~ Group + Timepoint + Block + Retreat, data = HitblockData, mean)
Aprimeplot<-ggplot(AprimeMeans)+geom_boxplot(aes(Group,HitAprimeblocks,color=Group))+facet_grid(.~Timepoint)
Aprimeplot+ggtitle("Aprime by Group and Timepoint")

##Some regression analyses to see how well variables correspond with one another according to our hypothesized relationships
#Load the libs
library(lme4)
library(nlme)

#Is response time predicted by Prestimulus alpha regardless of Group and timepoint?
#Overall Alpha
RTbyOVAlpha<-lm(RT ~ OV1sPreAlpha, HitblockData)
summary(RTbyOVAlpha)

#With Group and Timepoint included
RTbyOVAlpha_gt<-lm(RT ~ Group + Timepoint + OV1sPreAlpha, HitblockData)
summary(RTbyOVAlpha_gt)

#With Group and Timepoint and Block included

RTbyOVAlpha_gtb<-lm(RT ~ Group + Timepoint + Block + OV1sPreAlpha, HitblockData)
summary(RTbyOVAlpha_gtb)


#Hi Alpha
RTbyHiAlpha<-lm(RT ~ HitHiAlphablocks, HitblockData)
summary(RTbyHiAlpha)

#With Group and Timepoint included
RTbyHiAlpha_gt<-lm(RT ~ Group + Timepoint + HitHiAlphablocks, HitblockData)
summary(RTbyHiAlpha_gt)

#With Group and Timepoint and Block included

RTbyHiAlpha_gtb<-lm(RT ~ Group + Timepoint + Block + HitHiAlphablocks, HitblockData)
summary(RTbyHiAlpha_gtb)
#does appear to have as much of a relationship with RT as the overall alpha power.

#Low Alpha
RTbyLoAlpha<-lm(RT ~ HitLoAlphablocks, HitblockData)
summary(RTbyLoAlpha)

#With Group and Timepoint included
RTbyLoAlpha_gt<-lm(RT ~ Group + Timepoint + HitLoAlphablocks, HitblockData)
summary(RTbyLoAlpha_gt)

#With Group and Timepoint and Block included

RTbyLoAlpha_gtb<-lm(RT ~ Group + Timepoint + Block + HitLoAlphablocks, HitblockData)
summary(RTbyLoAlpha_gtb)
##Appears to be more predictive of response time than overall alpha.


#Is response time predicted by VEP amplitudes regardless of Group and timepoint?
#RH amplitude
RTbyRHamp<-lm(RT ~ HitRHampblocks, HitblockData)
summary(RTbyRHamp)
#not predictive. 

#with Group and Timepoint included
RTbyRHamp_gt<-lm(RT ~ Group + Timepoint + HitRHampblocks, HitblockData)
summary(RTbyRHamp_gt)
#still not very predictive. Though, better with group and timepoint included.

#with Group and Timepoint and Block included
RTbyRHamp_gtb<-lm(RT ~ Group + Timepoint + Block + HitRHampblocks, HitblockData)
summary(RTbyRHamp_gtb)
#better, but still not good.

#LH amplitude
RTbyLHamp<-lm(RT ~ HitLHampblocks, HitblockData)
summary(RTbyLHamp)
#somewhat predictive on its own

#with Group and Timepoint included
RTbyLHamp_gt<-lm(RT ~ Group + Timepoint + HitLHampblocks, HitblockData)
summary(RTbyLHamp_gt)
#decent model

#with Group and Timepoint and Block included
RTbyLHamp_gtb<-lm(RT ~ Group + Timepoint + Block + HitLHampblocks, HitblockData)
summary(RTbyLHamp_gtb)
#much better - looks nearly as predictive as low alpha.

#of All electrodes amplitude
RTbyAllamp<-lm(RT ~ HitAllampblocks, HitblockData)
summary(RTbyAllamp)
#no relationship

#with Group and Timepoint included
RTbyAllamp_gt<-lm(RT ~ Group + Timepoint + HitAllampblocks, HitblockData)
summary(RTbyAllamp_gt)

#with Group and Timepoint and Block included
RTbyAllamp_gtb<-lm(RT ~ Group + Timepoint + Block + HitAllampblocks, HitblockData)
summary(RTbyAllamp_gtb)

#RH Latency
RTbyRHLat<-lm(RT ~ HitRHLatblocks, HitblockData)
summary(RTbyRHLat)

#with Group and Timepoint included
RTbyRHLat_gt<-lm(RT ~ Group + Timepoint + HitRHLatblocks, HitblockData)
summary(RTbyRHLat_gt)
#decent model when Group and Timepoint are included.

#with Group and Timepoint and Block included
RTbyRHLat_gtb<-lm(RT ~ Group + Timepoint + Block + HitRHLatblocks, HitblockData)
summary(RTbyRHLat_gtb)
#not a good model

#LH Latency
RTbyLHLat<-lm(RT ~ HitLHLatblocks, HitblockData)
summary(RTbyLHLat)
#no relationship

#with Group and Timepoint included
RTbyLHLat_gt<-lm(RT ~ Group + Timepoint + HitLHLatblocks, HitblockData)
summary(RTbyLHLat_gt)

#with Group and Timepoint and Block included
RTbyLHLat_gtb<-lm(RT ~ Group + Timepoint + Block + HitLHLatblocks, HitblockData)
summary(RTbyLHLat_gtb)
#not a good model

#of All electrodes Latency
RTbyAllLat<-lm(RT ~ HitAllLatblocks, HitblockData)
summary(RTbyAllLat)
#no good

#with Group and Timepoint included
RTbyAllLat_gt<-lm(RT ~ Group + Timepoint + HitAllLatblocks, HitblockData)
summary(RTbyAllLat_gt)
#better model -- but all explained by group and timepoint

#with Group and Timepoint and Block included
RTbyAllLat_gtb<-lm(RT ~ Group + Timepoint + Block + HitAllLatblocks, HitblockData)
summary(RTbyAllLat_gtb)
#not the best model.

#Among correct rejections, Is VEP amplitude predicted Prestimulus alpha?
CRLHVEPbyAlpha<-lm(CRLHampblocks ~ OV1sPreAlpha, CRblockData)
summary(CRLHVEPbyAlpha)

#Among Hits, Is VEP amplitude predicted by Prestimulus alpha?
HitLHVEPbyAlpha<-lm(HitLHampblocks ~ OV1sPreAlpha, HitblockData)
summary(HitLHVEPbyAlpha)
#LH amplitude is strongly predicted by overall prestimulus alpha. Among both CRs and Hits

#Among correct rejections, Is VEP amplitude predicted Prestimulus alpha?
CRLHVEPbyAlpha_gt<-lm(CRLHampblocks ~ Group + Timepoint + OV1sPreAlpha, CRblockData)
summary(CRLHVEPbyAlpha_gt)
#Interestingly, timepoint doesn't add very much here.

#Among Hits, Is VEP amplitude predicted by Prestimulus alpha?
HitLHVEPbyAlpha_gt<-lm(HitLHampblocks ~ Group + Timepoint + OV1sPreAlpha, HitblockData)
summary(HitLHVEPbyAlpha_gt)

#Among correct rejections, Is VEP amplitude predicted Prestimulus alpha?
CRLHVEPbyAlpha_gtb<-lm(CRLHampblocks ~ Group + Timepoint + Block + OV1sPreAlpha, CRblockData)
summary(CRLHVEPbyAlpha_gtb)
#Among Hits, Is VEP amplitude predicted by Prestimulus alpha?
HitLHVEPbyAlpha_gtb<-lm(HitLHampblocks ~ Group + Timepoint + Block + OV1sPreAlpha, HitblockData)
summary(HitLHVEPbyAlpha_gtb)
#This model is most reliable when group and timepoint are NOT included.

#Among correct rejections, Is VEP amplitude predicted Prestimulus alpha?
CRRHVEPbyAlpha<-lm(CRRHampblocks ~ OV1sPreAlpha, CRblockData)
summary(CRRHVEPbyAlpha)
#Among Hits, Is VEP amplitude predicted by Prestimulus alpha?
HitRHVEPbyAlpha<-lm(HitRHampblocks ~ OV1sPreAlpha, HitblockData)
summary(HitRHVEPbyAlpha)
#strong relationship between these variables.

#Among correct rejections, Is VEP amplitude predicted Prestimulus alpha?
CRRHVEPbyAlpha_gt<-lm(CRRHampblocks ~ Group + Timepoint + OV1sPreAlpha, CRblockData)
summary(CRRHVEPbyAlpha_gt)
#Timepoint not a strong impact on model performance.

#Among Hits, Is VEP amplitude predicted by Prestimulus alpha?
HitRHVEPbyAlpha_gt<-lm(HitRHampblocks ~ Group + Timepoint + OV1sPreAlpha, HitblockData)
summary(HitRHVEPbyAlpha_gt)
#no strong impact with group or timepoint.

#Among correct rejections, Is VEP amplitude predicted Prestimulus alpha?
CRRHVEPbyAlpha_gtb<-lm(CRRHampblocks ~ Group + Timepoint + Block + OV1sPreAlpha, CRblockData)
summary(CRRHVEPbyAlpha_gtb)
#Among Hits, Is VEP amplitude predicted by Prestimulus alpha?
HitRHVEPbyAlpha_gtb<-lm(HitRHampblocks ~ Group + Timepoint + Block + OV1sPreAlpha, HitblockData)
summary(HitRHVEPbyAlpha_gtb)

#Among correct rejections, Is VEP amplitude predicted Prestimulus alpha?
CRAllVEPbyAlpha<-lm(CRAllampblocks ~ OV1sPreAlpha, CRblockData)
summary(CRAllVEPbyAlpha)
#Among Hits, Is VEP amplitude predicted by Prestimulus alpha?
HitAllVEPbyAlpha<-lm(HitAllampblocks ~ OV1sPreAlpha, HitblockData)
summary(HitAllVEPbyAlpha)

#Among correct rejections, Is VEP amplitude predicted Prestimulus alpha?
CRAllVEPbyAlpha_gt<-lm(CRAllampblocks ~ Group + Timepoint + OV1sPreAlpha, CRblockData)
summary(CRAllVEPbyAlpha_gt)
#Among Hits, Is VEP amplitude predicted by Prestimulus alpha?
HitAllVEPbyAlpha_gt<-lm(HitAllampblocks ~ Group + Timepoint + OV1sPreAlpha, HitblockData)
summary(HitAllVEPbyAlpha_gt)

#Among correct rejections, Is VEP amplitude predicted Prestimulus alpha?
CRAllVEPbyAlpha_gtb<-lm(CRAllampblocks ~ Group + Timepoint + Block + OV1sPreAlpha, CRblockData)
summary(CRAllVEPbyAlpha_gtb)
#Among Hits, Is VEP amplitude predicted by Prestimulus alpha?
HitAllVEPbyAlpha_gtb<-lm(HitAllampblocks ~ Group + Timepoint + Block + OV1sPreAlpha, HitblockData)
summary(HitAllVEPbyAlpha_gtb)

#Among correct rejections, Is VEP Latency predicted Prestimulus alpha?
CRLHVEPbyAlpha<-lm(CRLHLatblocks ~ OV1sPreAlpha, CRblockData)
summary(CRLHVEPbyAlpha)
#Among Hits, Is VEP Latency predicted by Prestimulus alpha?
HitLHVEPbyAlpha<-lm(HitLHLatblocks ~ OV1sPreAlpha, HitblockData)
summary(HitLHVEPbyAlpha)

#Among correct rejections, Is VEP Latency predicted Prestimulus alpha?
CRLHVEPbyAlpha_gt<-lm(CRLHLatblocks ~ Group + Timepoint + OV1sPreAlpha, CRblockData)
summary(CRLHVEPbyAlpha_gt)
#Among Hits, Is VEP Latency predicted by Prestimulus alpha?
HitLHVEPbyAlpha_gt<-lm(HitLHLatblocks ~ Group + Timepoint + OV1sPreAlpha, HitblockData)
summary(HitLHVEPbyAlpha_gt)

#Among correct rejections, Is VEP Latency predicted Prestimulus alpha?
CRLHVEPbyAlpha_gtb<-lm(CRLHLatblocks ~ Group + Timepoint + Block + OV1sPreAlpha, CRblockData)
summary(CRLHVEPbyAlpha_gtb)
#Among Hits, Is VEP Latency predicted by Prestimulus alpha?
HitLHVEPbyAlpha_gtb<-lm(HitLHLatblocks ~ Group + Timepoint + Block + OV1sPreAlpha, HitblockData)
summary(HitLHVEPbyAlpha_gtb)
#Interesting differences in model performance between hits and CRs here. CRPrestim Alpha is strongly predictive
#of LHVEPlatency with group and timepoint and block included, but not GTB doesn't influence predictions for
#VEP latency with hits.

#Among correct rejections, Is VEP Latency predicted Prestimulus alpha?
CRRHVEPbyAlpha<-lm(CRRHLatblocks ~ OV1sPreAlpha, CRblockData)
summary(CRRHVEPbyAlpha)
#Among Hits, Is VEP Latency predicted by Prestimulus alpha?
HitRHVEPbyAlpha<-lm(HitRHLatblocks ~ OV1sPreAlpha, HitblockData)
summary(HitRHVEPbyAlpha)

#Among correct rejections, Is VEP Latency predicted Prestimulus alpha?
CRRHVEPbyAlpha_gt<-lm(CRRHLatblocks ~ Group + Timepoint + OV1sPreAlpha, CRblockData)
summary(CRRHVEPbyAlpha_gt)
#Among Hits, Is VEP Latency predicted by Prestimulus alpha?
HitRHVEPbyAlpha_gt<-lm(HitRHLatblocks ~ Group + Timepoint + OV1sPreAlpha, HitblockData)
summary(HitRHVEPbyAlpha_gt)

#Among correct rejections, Is VEP Latency predicted Prestimulus alpha?
CRRHVEPbyAlpha_gtb<-lm(CRRHLatblocks ~ Group + Timepoint + Block + OV1sPreAlpha, CRblockData)
summary(CRRHVEPbyAlpha_gtb)
#Among Hits, Is VEP Latency predicted by Prestimulus alpha?
HitRHVEPbyAlpha_gtb<-lm(HitRHLatblocks ~ Group + Timepoint + Block + OV1sPreAlpha, HitblockData)
summary(HitRHVEPbyAlpha_gtb)

#Among correct rejections, Is VEP Latency predicted Prestimulus alpha?
CRAllVEPbyAlpha<-lm(CRAllLatblocks ~ OV1sPreAlpha, CRblockData)
summary(CRAllVEPbyAlpha)
#Among Hits, Is VEP Latency predicted by Prestimulus alpha?
HitAllVEPbyAlpha<-lm(HitAllLatblocks ~ OV1sPreAlpha, HitblockData)
summary(HitAllVEPbyAlpha)

#Among correct rejections, Is VEP Latency predicted Prestimulus alpha?
CRAllVEPbyAlpha_gt<-lm(CRAllLatblocks ~ Group + Timepoint + OV1sPreAlpha, CRblockData)
summary(CRAllVEPbyAlpha_gt)
#Among Hits, Is VEP Latency predicted by Prestimulus alpha?
HitAllVEPbyAlpha_gt<-lm(HitAllLatblocks ~ Group + Timepoint + OV1sPreAlpha, HitblockData)
summary(HitAllVEPbyAlpha_gt)

#Among correct rejections, Is VEP Latency predicted Prestimulus alpha?
CRAllVEPbyAlpha_gtb<-lm(CRAllLatblocks ~ Group + Timepoint + Block + OV1sPreAlpha, CRblockData)
summary(CRAllVEPbyAlpha_gtb)
#Among Hits, Is VEP Latency predicted by Prestimulus alpha?
HitAllVEPbyAlpha_gtb<-lm(HitAllLatblocks ~ Group + Timepoint + Block + OV1sPreAlpha, HitblockData)
summary(HitAllVEPbyAlpha_gtb)
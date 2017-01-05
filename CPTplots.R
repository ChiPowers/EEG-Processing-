## Use basic R graphics to create a pair-wise scatter plot
eeframe <- AllCR.trim

## Use ggplot2 to create conditioned scatter plots
library(ggplot2)
plotCols <- c(
       # "IAP",
              "OV1sPreAlpha",
              "Hi1sPreAlpha",
              "Low1sPreAlpha",
              "AllhitsPeakLat",
              "AllhitsPeakAmp",
              "LHhitsPeakLat",
              "LHhitsPeakAmp",
              "RHhitsPeakLat",
              "RHhitsPeakAmp")
plotEE <- function(x){
  title <- paste("RT vs", x, "\n conditioned on Group and Timepoint")
  ggplot(eeframe, aes_string(x, "IAP")) +
    geom_point() +
    facet_grid(Group ~ Timepoint) +
    ggtitle(title) +
    stat_smooth(method = "lm")
}
lapply(plotCols, plotEE)

## Create histograms
plotCols4 <- c("IAP",
              "OV1sPreAlpha",
              "Hi1sPreAlpha",
              "Low1sPreAlpha",
              "AllhitsPeakLat",
              "AllhitsPeakAmp",
              "LHhitsPeakLat",
              "LHhitsPeakAmp",
              "RHhitsPeakLat",
              "RHhitsPeakAmp"
             )
library(gridExtra)
eeHist <- function(x){
  title <- paste("Histogram of", x, "conditioned on Group and Timepoint")
  ggplot(eeframe, aes_string(x)) +
  #  geom_histogram(aes(y = ..density.., fill = Group)) +
    geom_histogram(bindwidth = .01, aes(fill = Group)) +
    facet_grid(. ~ Timepoint) +
    ggtitle(title)
  # geom_density()
                    }
lapply(plotCols4, eeHist)


## Create box plots
eebox <- function(x) {
  title <- paste("Box plot of", x, "by RT")
  ggplot(eeframe, aes_string('RT', x)) +
    geom_boxplot() +
    ggtitle(title)
}
lapply(plotCols4, eebox)

#boxplots of OverallPrestimulus alpha during CR trials averaged across blocks
a<-ggplot(Averaged) + geom_boxplot(aes(Group, OV1sPreAlpha)) + facet_grid(. ~ Timepoint)
#of RHPeakAmp
b<-ggplot(ampAveraged) + geom_boxplot(aes(Group, RHcrsPeakAmp)) + facet_grid(. ~ Timepoint)


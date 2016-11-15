## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(tidy = FALSE)
figwidth.out <- 600

## ----wine data, dpi=200, fig.width=7, fig.height=4, out.width = figwidth.out----
library(speaq2)
data(Winedata)
Spectra.wine <- as.matrix(Winedata$spectra )
ppm.wine <- as.numeric(Winedata$ppm) 
wine.color <- Winedata$wine.color 
wine.origin <- Winedata$origin 
# all spectra
drawSpecPPM(Y.spec = Spectra.wine, 
            X.ppm = ppm.wine, 
            title = 'Wine data spectra', 
            groupFactor = wine.color, 
            legend.extra.x = 1, 
            legend.extra.y = 1.1)

## ----wine excerpt, dpi=200, fig.width=7, fig.height=4, out.width = figwidth.out----
# small excerpt by defining the region of interest
drawSpecPPM(Y.spec = Spectra.wine, 
            X.ppm = ppm.wine, 
            groupFactor = as.factor(wine.color), 
            title = 'Raw wine data excerpt', 
            legend.extra.x = 1.1, 
            legend.extra.y = 1.0, 
            ROI.ppm = 3.6, 
            ROI = NULL, 
            roiWidth.ppm = 0.15, 
            legendpos = "topright" )

## ----detect winepeaks,  results = "hide"---------------------------------
wine.peaks <- getWaveletPeaks(Y.spec=Spectra.wine, 
                             X.ppm=ppm.wine, 
                             baselineThresh = 10,
                             SNR.Th = -1, 
                             nCPU = 2, 
                             include_nearbyPeaks = TRUE) # nCPU set to 2 for the vignette build


wine.aligned <- PeakAligner(Y.peaks = wine.peaks,  
                            min.samp.grp = 5, 
                            grouping.window.width = 200)


## ----plots base, dpi=200, fig.width=7, fig.height=10, fig.keep = "last", out.width = figwidth.out, warnings = FALSE----
# adding labels to the dat a for plotting and the group ppm values
library(grid)
library(gridBase)
library(gridExtra)
library(ggplot2)
.pardefault <- par(no.readonly = T)
wine.peaks.plot <- AddPlottingStuff(Y.peaks = wine.peaks, 
                                    X.ppm = ppm.wine, 
                                    groupLabels = wine.color )

wine.aligned.plot <- AddPlottingStuff(Y.peaks = wine.aligned, 
                                      X.ppm = ppm.wine, 
                                      groupLabels = wine.color )
#ROI.ppm <- 3.6
#roiWidth.ppm <- 0.15
ROI.ppm <- 1.330
roiWidth.ppm <- 0.025

pp1 <- ggplot(wine.peaks.plot[wine.peaks.plot$peakPPM > (ROI.ppm - roiWidth.ppm ) &
                              wine.peaks.plot$peakPPM < (ROI.ppm + roiWidth.ppm ) ,], 
              aes(x = peakPPM, y = peakValue, colour = label) ) + 
       geom_point() + 
       theme_bw() + 
       xlim(c(ROI.ppm + roiWidth.ppm, ROI.ppm - roiWidth.ppm)) +
       scale_colour_manual(values = c("rose" = "#00BF7D","red" = "#F8766D","white" = "#00B0F6")) +  
       labs(x = "ppm", y = "peak value") + 
       ggtitle("Wavelet based peak detection") +
       theme(legend.title = element_blank(),
             legend.position = c(0.061,0.75),
             legend.background = element_rect(colour = "black", size = 0.3, linetype = 1),
             legend.key = element_blank(),
             legend.text = element_text(size = 10),
             text = element_text(size = 12),
             axis.text.y = element_text(angle = 90, hjust = 0.5),
             axis.ticks.length = unit(0.2,"cm"), 
             axis.title.x = element_text(margin = margin(10,0,0,0)),
             axis.title.y = element_text(margin = margin(0,20,0,0)),
             plot.title = element_text(lineheight = 0.8, face = "bold", margin = margin(0,0,20,0)),
             plot.margin = unit(c(0.5,1,0.5,0.5), "cm")) 
     


pp2 <- ggplot(wine.aligned.plot[wine.aligned.plot$peakPPM > (ROI.ppm - roiWidth.ppm ) &
                                wine.aligned.plot$peakPPM < (ROI.ppm + roiWidth.ppm ) ,],
              aes(x = groupPPM, y = peakValue, colour = label) ) +
       geom_point() + 
       theme_bw() + 
       xlim(c(ROI.ppm + roiWidth.ppm, ROI.ppm - roiWidth.ppm)) +
       scale_colour_manual( values = c("rose" = "#00BF7D","red" = "#F8766D","white" = "#00B0F6")) +  
       labs(x = "ppm", y = "peak value") + 
       ggtitle("Aligned peaks") +
       theme(legend.title = element_blank(),
             legend.position = c(0.061,0.748),
             legend.background = element_rect(colour = "black",size = 0.3, linetype = 1),
             legend.key = element_blank(), 
             legend.text = element_text(size = 10),
             text = element_text(size = 12),
             axis.text.y = element_text(angle = 90, hjust = 0.5),
             axis.ticks.length = unit(0.2,"cm"), 
             axis.title.x = element_text(margin = margin(10,0,0,0)),
             axis.title.y = element_text(margin = margin(0,20,0,0)),
             plot.title = element_text(lineheight = 0.8, face="bold", margin = margin(0,0,20,0)),
             plot.margin = unit(c(0.5,1,0.5,0.5), "cm")) 

plot.new()

grid.newpage()
pushViewport(viewport(layout = grid.layout(13, 1)))

#Draw ggplot1
pushViewport(viewport(layout.pos.row = (6:9)))
print(pp1, newpage = FALSE)
popViewport()

#Draw ggplot2
pushViewport(viewport(layout.pos.row = (10:13)))
print(pp2, newpage = FALSE)
popViewport()

#Draw bsae plot
pushViewport(viewport(layout.pos.row = (1:5)))
par(fig = gridFIG(), new = TRUE)
drawSpecPPM(Y.spec = Spectra.wine, 
            X.ppm = ppm.wine, 
            groupFactor = as.factor(wine.color), 
            title = "Raw wine data excerpt",
            ROI.ppm = ROI.ppm, 
            roiWidth.ppm = roiWidth.ppm,  
            nAxisPos = 6,
            manual.colours = c("#F8766D","#00BF7D", "#00B0F6"),
            legend.extra.x = 0.3, 
            legend.extra.y = 0.8)
popViewport()


## ----silhouette values,  results = "hide", dpi=200, fig.width=7, fig.height=4, out.width = figwidth.out----
SilhouetteValues <- SilhouetR(DataMatrix = wine.aligned$peakPPM, 
                              GroupIndices = wine.aligned$peakIndex)

Silh_plot <- ggplot(SilhouetteValues, aes(SilhouetteValues)) +
             geom_freqpoly(binwidth = 0.03) +
             theme_bw()
Silh_plot



## ----average silhouette, tidy = TRUE-------------------------------------
groups <- unique(SilhouetteValues$GroupIndices)
Ngroups <- length(groups)
sil_means <- matrix(NA, ncol = 3, nrow = Ngroups)

for(k in 1:Ngroups){
    sil_means[k,1] = groups[k]
    sil_means[k,2] = mean(SilhouetteValues$SilhouetteValues[SilhouetteValues$GroupIndices==groups[k]])
    sil_means[k,3] = mean(wine.aligned$peakSNR[wine.aligned$peakIndex==groups[k]])
}

sil_means <- sil_means[order(sil_means[,2]),]
colnames(sil_means) <- c("groupIndex", "avg_silhouette_val", "avg. SNR")
head(sil_means)


## ----wrong grouping plot,  dpi=200, fig.width=7, fig.height=10, fig.keep = "last", out.width = figwidth.out, warnings = FALSE----

faulty.groupIndex <- sil_means[1,1]
ROI.ppm <- ppm.wine[faulty.groupIndex]
roiWidth.ppm <- 0.1
pp1 <- ggplot(wine.peaks.plot[wine.peaks.plot$peakPPM > (ROI.ppm - roiWidth.ppm ) &
                              wine.peaks.plot$peakPPM < (ROI.ppm + roiWidth.ppm ) ,],
              aes(x = peakPPM, y = peakValue, colour = label) ) + 
       geom_point() + 
       xlim(c(ROI.ppm + roiWidth.ppm, ROI.ppm - roiWidth.ppm)) +
       scale_colour_manual( values = c("rose" = "#00BF7D","red" = "#F8766D","white" = "#00B0F6")) +  
       labs(x = "ppm", y = "peak value") + 
       ggtitle("Wavelet based peak detection") +
       theme_bw() + 
       theme(legend.title = element_blank(),
             legend.position = c(0.061,0.75),
             legend.background = element_rect(colour = "black", size = 0.3, linetype = 1),
             legend.key = element_blank(), 
             legend.text = element_text(size=10),
             text = element_text(size = 12),
             axis.text.y = element_text(angle=90, hjust=0.5),
             axis.ticks.length = unit(0.2,"cm"), 
             axis.title.x = element_text(margin=margin(10,0,0,0)),
             axis.title.y = element_text(margin=margin(0,20,0,0)),
             plot.title = element_text(lineheight = 0.8, face = "bold", margin = margin(0,0,20,0)),
             plot.margin = unit(c(0.5,1,0.5,0.5), "cm"))


pp2 <- ggplot(wine.aligned.plot[wine.aligned.plot$groupPPM > (ROI.ppm - roiWidth.ppm ) &
                                wine.aligned.plot$groupPPM < (ROI.ppm + roiWidth.ppm ) ,], 
              aes(x = groupPPM, y = peakValue, colour = label) ) +
       geom_point() + 
       theme_bw() + 
       xlim(c( ROI.ppm + roiWidth.ppm, ROI.ppm - roiWidth.ppm)) +
       scale_colour_manual( values = c("rose" = "#00BF7D","red" = "#F8766D","white" = "#00B0F6")) +  
       labs(x = "ppm", y = "peak value") + 
       ggtitle("Aligned peaks") +
       theme(legend.title = element_blank(),
             legend.position = c(0.061,0.748),
             legend.background = element_rect(colour = "black", size = 0.3, linetype = 1),
             legend.key = element_blank(), 
             legend.text = element_text(size = 10),
             text = element_text(size = 12),
             axis.text.y = element_text(angle = 90, hjust = 0.5),
             axis.ticks.length = unit(0.2,"cm"), 
             axis.title.x = element_text(margin = margin(10,0,0,0)),
             axis.title.y = element_text(margin = margin(0,20,0,0)),
             plot.title = element_text(lineheight = 0.8, face="bold", margin = margin(0,0,20,0)),
             plot.margin = unit(c(0.5,1,0.5,0.5), "cm"))

plot.new()

grid.newpage()
pushViewport(viewport(layout = grid.layout(13, 1)))

#Draw ggplot1
pushViewport(viewport(layout.pos.row = (6:9)))
print(pp1, newpage = FALSE)
popViewport()

#Draw ggplot2
pushViewport(viewport(layout.pos.row = (10:13)))
print(pp2, newpage = FALSE)
popViewport()

#Draw bsae plot
pushViewport(viewport(layout.pos.row = (1:5)))
par(fig = gridFIG(), new = TRUE)
drawSpecPPM(Y.spec = Spectra.wine, 
            X.ppm = ppm.wine, 
            groupFactor = as.factor(wine.color), 
            title = "Raw wine data excerpt",
            ROI.ppm = ROI.ppm, 
            roiWidth.ppm = roiWidth.ppm,  
            nAxisPos = 6,
            manual.colours = c("#F8766D","#00BF7D", "#00B0F6"),
            legend.extra.x = 0.3, 
            legend.extra.y = 0.8)
popViewport()


## ----regroup-------------------------------------------------------------
wrong.groups <- sort(sil_means[sil_means[,1]>=sil_means[1,1],1])[1:2]

wine.realigned <- regroupR(aligned.peaks = wine.aligned, 
                           list.to.regroup = wrong.groups, 
                           min.samp.grp = 5 ,
                           max.dupli.prop = 0.1)


## ----regroup fix plot,  dpi=200, fig.width=7, fig.height=10, fig.keep = "last", out.width = figwidth.out, warnings = FALSE----
wine.realigned.plot <- AddPlottingStuff(Y.peaks = wine.realigned, 
                                        X.ppm = ppm.wine, 
                                        groupLabels = wine.color )

faulty.groupIndex <- sil_means[1,1]
ROI.ppm <- ppm.wine[faulty.groupIndex]
roiWidth.ppm <- 0.1

pp1 <- ggplot(wine.peaks.plot[wine.peaks.plot$peakPPM > (ROI.ppm - roiWidth.ppm ) &
                              wine.peaks.plot$peakPPM < (ROI.ppm + roiWidth.ppm ) ,], 
              aes(x = peakPPM, y = peakValue, colour = label) ) + 
       geom_point() + 
       theme_bw() + 
       xlim(c(ROI.ppm + roiWidth.ppm, ROI.ppm - roiWidth.ppm)) +
       scale_colour_manual(values = c("rose" = "#00BF7D","red" = "#F8766D","white" = "#00B0F6")) +  
       labs(x = "ppm", y = "peak value") + 
       ggtitle("Wavelet based peak detection") +
       theme(legend.title = element_blank(),
             legend.position = c(0.061,0.75),
             legend.background = element_rect(colour = "black", size = 0.3, linetype = 1),
             legend.key = element_blank(), 
             legend.text = element_text(size = 10),
             text = element_text(size = 12),
             axis.text.y = element_text(angle = 90, hjust = 0.5),
             axis.ticks.length = unit(0.2,"cm"), 
             axis.title.x = element_text(margin = margin(10,0,0,0)),
             axis.title.y = element_text(margin = margin(0,20,0,0)),
             plot.title = element_text(lineheight = 0.8, face = "bold", margin = margin(0,0,20,0)),
             plot.margin = unit(c(0.5,1,0.5,0.5), "cm"))


pp2 <- ggplot(wine.realigned.plot[wine.realigned.plot$groupPPM > (ROI.ppm - roiWidth.ppm ) & 
                                  wine.realigned.plot$groupPPM < (ROI.ppm + roiWidth.ppm ) ,], 
              aes(x = groupPPM, y = peakValue, colour = label) ) +
       geom_point() + 
       theme_bw() + 
       xlim(c(ROI.ppm + roiWidth.ppm, ROI.ppm - roiWidth.ppm)) +
       scale_colour_manual(values = c("rose" = "#00BF7D","red" = "#F8766D","white" = "#00B0F6")) +  
       labs(x = "ppm", y = "peak value") + 
       ggtitle("Aligned peaks") +
       theme(legend.title = element_blank(),
             legend.position = c(0.061,0.748),
             legend.background = element_rect(colour = "black", size= 0.3, linetype = 1),
             legend.key = element_blank(), 
             legend.text = element_text(size = 10),
             text = element_text(size = 12),
             axis.text.y = element_text(angle = 90, hjust = 0.5),
             axis.ticks.length = unit(0.2,"cm"), 
             axis.title.x = element_text(margin = margin(10,0,0,0)),
             axis.title.y = element_text(margin = margin(0,20,0,0)),
             plot.title = element_text(lineheight = 0.8, face = "bold", margin = margin(0,0,20,0)),
             plot.margin = unit(c(0.5,1,0.5,0.5), "cm"))

plot.new()

grid.newpage()
pushViewport(viewport(layout = grid.layout(13, 1)))

#Draw ggplot1
pushViewport(viewport(layout.pos.row = (6:9)))
print(pp1, newpage = FALSE)
popViewport()

#Draw ggplot2
pushViewport(viewport(layout.pos.row = (10:13)))
print(pp2, newpage = FALSE)
popViewport()

#Draw bsae plot
pushViewport(viewport(layout.pos.row = (1:5)))
par(fig = gridFIG(), new = TRUE)
drawSpecPPM(Y.spec = Spectra.wine, 
            X.ppm = ppm.wine, 
            groupFactor = as.factor(wine.color), 
            title = "Raw wine data excerpt",
            ROI.ppm = ROI.ppm, 
            roiWidth.ppm = roiWidth.ppm,  
            nAxisPos = 6,
            manual.colours = c("#F8766D","#00BF7D", "#00B0F6"),
            legend.extra.x = 0.3, 
            legend.extra.y = 0.8)
popViewport()

## ----data matrix, results = "hide", message=FALSE------------------------

wine.filled <- PeakFilling(Y.aligned = wine.realigned, 
                          Y.spec = Spectra.wine,  
                          max.index.shift = 200,
                          nCPU = 2) # nCPU set to 1 for the vignette build

wine.Features <- BuildFeatureMatrix(wine.aligned)


## ----scaling-------------------------------------------------------------
wine.Features.scaled <- SCANT(data.matrix = wine.Features, 
                              type = c("pareto", "center"))  


## ----PCA, dpi=200, fig.width=6, fig.height=4, out.width = figwidth.out----
library(stats)

common.pca <- prcomp(wine.Features.scaled) 


loadings <- common.pca$rotation
scores <- common.pca$x
varExplained <- common.pca$sdev^2

barplot(varExplained/sum(varExplained), 
        main="Scree Plot",ylab="Proportion of variance explained", 
        xlab = "Principal comonent", 
        names.arg = as.character(seq(1,length(varExplained))))

## ----PCA2, dpi=200, fig.width=7, fig.height=5,out.width = figwidth.out, tidy = FALSE----
plot.marks <- as.numeric(wine.color)
plot.marks[plot.marks == 1] <- 8 
plot.marks[plot.marks == 2] <- 15
plot.marks[plot.marks == 3] <- 1

cp1 <- 1
cp2 <- 2 
plot(scores[,cp1]/max(scores[,cp1]), scores[,cp2]/max(scores[,cp2]),
     main=paste("score plot, PC",cp1," vs. PC",cp2,sep=""),
     xlab=paste("PC",cp1,round(varExplained[cp1]/sum(varExplained),digits=2),""),
     ylab=paste("PC",cp2,round(varExplained[cp2]/sum(varExplained),digits=2),""),
     pch = plot.marks)
text(scores[,cp1]/max(scores[,cp1]),scores[,cp2]/max(scores[,cp2]), wine.color, cex=0.5, pos=4, col="red")
lines(x = c(-100,100), y = c(0,0))
lines(x = c(0,0), y = c(-100,100))
legend("topleft", 
       legend = c("red  ","rosé  ","white      "), 
       pch = c(8,15,1), 
       y.intersp = 1.9)




## ----no rose-------------------------------------------------------------
red.white.scaled <- SCANT(wine.Features[wine.color!="rose",], 
                         type = c("pareto", "center"))  

red.white.colors <- as.factor(as.character(wine.color)[wine.color!="rose"])


## ----relevant, dpi=200, fig.width=7, fig.height=4.5, message=F, out.width = figwidth.out----

p.all_noadj <- relevant.features.p(datamatrix = red.white.scaled,
                                   responsevector = red.white.colors, 
                                   p.adj = "none")

p.all_bonf <- relevant.features.p(datamatrix = red.white.scaled,
                                  responsevector = red.white.colors, 
                                  p.adj = "bonferroni")


pp1 <- ggplot(p.all_noadj, aes(x=as.numeric(rownames(p.all_noadj)), y=p.values)) + 
       geom_point() +
       xlab("feature index") + 
       ylab("p value") + 
       ggtitle("p-values") +
       theme_bw() + 
       theme(plot.title = element_text(face = "bold"), text = element_text(size=13))

pp2 <- ggplot(p.all_bonf, aes(x=as.numeric(rownames(p.all_bonf)), y=p.values)) + 
       geom_point() +
       xlab("feature index") + 
       ylab("adjusted p value") + 
       ggtitle("Bonferroni corrected") +
       geom_hline(aes(yintercept=0.05, color="red"),linetype = 2) + guides(color=FALSE)+
       scale_y_continuous(breaks = c(0.00,0.05,0.25,0.50,0.75,1.00))+
       theme_bw() + 
       theme(plot.title = element_text(face = "bold"), 
             text = element_text(size=13),
             axis.text.y = element_text(colour=c("black","red","black","black","black","black")))

lay <- rbind(c(1,2))

grid.arrange(pp1, pp2, layout_matrix = lay)


## ----significant features------------------------------------------------
significant.features <- p.all_bonf[p.all_bonf$p.values<=0.05, ]

# order from most significant
significant.features <- significant.features[order(significant.features[,2]),]
head(significant.features)


## ----plot significant features, dpi=200, fig.width=7, fig.height=4, out.width = figwidth.out----

peak_of_interest <- 4# change this number to any of the peaks you want to see
drawSpecPPM(Y.spec = Spectra.wine[wine.color != "rose", ], 
            X.ppm = ppm.wine, 
            groupFactor = red.white.colors, 
            title = paste("significant feature, p-value =",
                          format(significant.features$p.values[peak_of_interest], 
                                 scientific = TRUE, 
                                 digits=2),
                          sep=" "), 
            legend.extra.x = 1.1, 
            legend.extra.y = 0.9, 
            ROI = significant.features$index[peak_of_interest], 
            roiWidth = 100, 
            legendpos = "topright" )


## ----speaq 1.0,  dpi=200, fig.width=7, fig.height=4, message=F, out.width = figwidth.out----
library(speaq)

peakList <- detectSpecPeaks(as.matrix(Spectra.wine),   
                            nDivRange = 128,                
                            scales = seq(1, 16, 2),
                            baselineThresh = 100,
                            SNR.Th = -1,
                            verbose=FALSE)

resFindRef <- findRef(peakList)
refInd <- resFindRef$refInd

aligned.spectra <- dohCluster(as.matrix(Spectra.wine), 
                              peakList = peakList,
                              refInd = refInd,
                              maxShift  = 200,
                              acceptLostPeak = TRUE, verbose=FALSE)
      
                
drawSpecPPM(Y.spec = aligned.spectra[wine.color != "rose", ], 
            X.ppm = ppm.wine, 
            groupFactor = red.white.colors, 
            title = paste("significant feature, p-value =",
                          format(significant.features$p.values[peak_of_interest], 
                                 scientific = TRUE, 
                                 digits=2),
                          sep=" "), 
            legend.extra.x = 1.1, 
            legend.extra.y = 0.9,
            ROI = significant.features$index[peak_of_interest], 
            roiWidth = 100, 
            legendpos = "topright" )               


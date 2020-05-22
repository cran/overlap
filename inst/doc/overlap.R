### R code from vignette source 'overlap.Rnw'

###################################################
### code chunk number 1: options
###################################################
options(continue="  ")


###################################################
### code chunk number 2: loadData
###################################################
library(overlap)
data(kerinci)
head(kerinci)
table(kerinci$Zone)
summary(kerinci$Sps)
range(kerinci$Time)


###################################################
### code chunk number 3: convertToRadians
###################################################
timeRad <- kerinci$Time * 2 * pi


###################################################
### code chunk number 4: singleDensityCurve
###################################################
tig2 <- timeRad[kerinci$Zone == 2 & kerinci$Sps == 'tiger']
densityPlot(tig2, rug=TRUE)


###################################################
### code chunk number 5: smoothing
###################################################
par(mfrow=2:1)
densityPlot(tig2, rug=TRUE, adjust=2)
text(2, 0.07, "adjust = 2")
densityPlot(tig2, rug=TRUE, adjust=0.2)
text(2, 0.1, "adjust = 0.2")


###################################################
### code chunk number 6: tigerMacaque2
###################################################
tig2 <- timeRad[kerinci$Zone == 2 & kerinci$Sps == 'tiger']
mac2 <- timeRad[kerinci$Zone == 2 & kerinci$Sps == 'macaque']
min(length(tig2), length(mac2))
tigmac2est <- overlapEst(tig2, mac2, type="Dhat4")
tigmac2est
overlapPlot(tig2, mac2, main="Zone 2")
legend('topright', c("Tigers", "Macaques"), lty=c(1,2), col=c(1,4), bty='n')


###################################################
### code chunk number 7: bootstrap1
###################################################
tigmac2 <- bootstrap(tig2, mac2, 1000, type="Dhat4")  # takes a few seconds
( BSmean <- mean(tigmac2) )


###################################################
### code chunk number 8: bootstrapCI
###################################################
bootCI(tigmac2est, tigmac2)


###################################################
### code chunk number 9: bootstrapCIlogit
###################################################
bootCIlogit(tigmac2est, tigmac2)



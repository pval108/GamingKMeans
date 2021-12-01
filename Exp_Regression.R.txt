# Loading required libraries
library(tidyverse)          # Pipe operator (%>%) and other commands
library(caret)              # Random split of data/cross validation
library(olsrr)              # Heteroscedasticity Testing (ols_test_score)
library(car)                # Muticolinearity detection (vif)
library(broom)              # Diagnostic Metric Table (augment)
library(ggplot2)
options(scipen = 999)

gameData<-read.csv("SteamCharts_SimpleID_N.csv")
summary(gameData)

gameData60 <- gameData[-c(1:2)]

wss <- (nrow(gameData60)-1)*sum(apply(gameData60,2,var))
for (i in 1:10) wss[i] <- sum(kmeans(gameData60,
                                     centers=i)$withinss)
plot(1:10, wss, type="b", main = paste('The Elbow Method'), xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# Calculating Clusters 

# Fitting K-Means to the Cluster Databases
set.seed(29)
kmeans.c = kmeans(x = gameData60, centers = 7)
y_kmeans.c = kmeans.c$cluster

# Visualizing the clusters

library(cluster)

clusplot(gameData60,
         y_kmeans.c,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Steam Games Clusters'))

# Attaching Cluster data to table with coefficients
gameData60.c <- gameData60.c[,-c(66)]
gameData60.c <- cbind(gameData60,y_kmeans.c)

# Describing Clusters by aggregating Statistics to locate center within each cluster for each position

cluster.def = aggregate(gameData60.c,
                        by = list(y_kmeans.c),
                        FUN = mean)

#writing raw file for excel processing 
write.csv(cluster.def,"/Users/andresarias/Dropbox/MSIS/PDS/R/AID_Datathon/cluster.def.csv", row.names = TRUE)

# bringing back processed file for plotting - standard file
clusplot <-read.csv("cluster.plot.csv")
plot(smooth.spline(clusplot$Period, clusplot$C1), main=paste("Game Diffusion Clusters"), ylim = c(0,0.035), xlab="period", ylab="normalized users", col="royal blue", type = "l", las=1)
lines(smooth.spline(clusplot$Period, clusplot$C2), col="tan", lwd=1)
lines(smooth.spline(clusplot$Period, clusplot$C3), col="magenta", lwd=1)
lines(smooth.spline(clusplot$Period, clusplot$C4), col="turquoise", lwd=1)
lines(smooth.spline(clusplot$Period, clusplot$C5), col="coral", lwd=1)
lines(smooth.spline(clusplot$Period, clusplot$C6), col="yellow2", lwd=1)
lines(smooth.spline(clusplot$Period, clusplot$C7), col="purple", lwd=1)
lines(smooth.spline(clusplot$Period, clusplot$ALL), col="brown1", lwd=5)

#bringing back accumulated file
clusplot2 <-read.csv("cluster.plot.acc.csv")
plot(smooth.spline(clusplot2$Period, clusplot2$C1), main=paste("Game Diffusion Clusters - Accumulated"), xlab="period", ylab="normalized users", col="royal blue", type = "l", las=1)
abline(h=0.025,col="#19A084", lwd=3)
text(57,0.05, "Innovators", col = "#19A084")
abline(h=0.16,col="#9BBB5C", lwd=3)
text(57,0.185, "Early Adopters", col = "#9BBB5C")
abline(h=0.5,col="#F29B25", lwd=3)
text(57,0.525, "Early Majority", col = "#F29B25")
abline(h=0.84,col="#BD392E", lwd=3)
text(57,0.865, "Late Majority", col = "#BD392E")
abline(h=1,col="#445469", lwd=3)
text(57,1.025, "Laggards", col = "#445469")
legend(1, .95, legend=c("C1", "C2", "C3", "C4", "C5", "C6", "C7"), col=c("royal blue","tan","magenta", "turquoise", "coral","yellow2", "purple"), lty=1, cex = 0.8, box.lty = 0, horiz=TRUE )
lines(smooth.spline(clusplot2$Period, clusplot2$C2), col="tan", lwd=1)
lines(smooth.spline(clusplot2$Period, clusplot2$C3), col="magenta", lwd=1)
lines(smooth.spline(clusplot2$Period, clusplot2$C4), col="turquoise", lwd=1)
lines(smooth.spline(clusplot2$Period, clusplot2$C5), col="coral", lwd=1)
lines(smooth.spline(clusplot2$Period, clusplot2$C6), col="yellow2", lwd=1)
lines(smooth.spline(clusplot2$Period, clusplot2$C7), col="purple", lwd=1)

#loading Quartiles
clusplot3 <- read.csv("cluster.plot.quart.csv")
plot(smooth.spline(clusplot3$Quarter, clusplot3$C1), main=paste("Game Diffusion Clusters - Quarters"), 
                xlab="quarter", ylab="normalized users", ylim = c(0,0.60), xaxp = c(1,4,3),
                col="royal blue", type = "b", las=1)
lines(smooth.spline(clusplot3$Quarter, clusplot3$C2), col="tan", lwd=1, type="b",pch=16)
lines(smooth.spline(clusplot3$Quarter, clusplot3$C3), col="magenta", lwd=1,type="b",pch=17)
lines(smooth.spline(clusplot3$Quarter, clusplot3$C4), col="turquoise", lwd=1, type="b",pch=18)
lines(smooth.spline(clusplot3$Quarter, clusplot3$C5), col="coral", lwd=1,type="b",pch=19)
lines(smooth.spline(clusplot3$Quarter, clusplot3$C6), col="yellow2", lwd=1, type="b",pch=20)
lines(smooth.spline(clusplot3$Quarter, clusplot3$C7), col="purple", lwd=1, type="b",pch=21)
lines(smooth.spline(clusplot3$Quarter, clusplot3$ALL), col="red", lwd=2, type="b",pch=22)
legend("topright", legend=c("C1", "C2", "C3", "C4", "C5", "C6", "C7"), 
       col=c("royal blue","tan","magenta", "turquoise", "coral","yellow2", "purple"), 
       lty=1, cex = 0.8, box.lty = 0, horiz=FALSE )

abline(v=1.5,col="black", lwd=3, lty=2)
abline(v=2.5,col="black", lwd=3, lty=2)
abline(v=3.5,col="black", lwd=3, lty=2)

# Trying to match games to predictions


game60 = subset(gameData60,gamenameID==2)
plot(game60$Period, game60$avg, main=paste("gameID =",2), type = "l", las=1)
lines(smooth.spline(game60$Period, game60$avg ), col="gray", lwd=1)
lines(smooth.spline(modeltest$Period+10, predict(testmodel)), col="blue", lwd=3)
lines(smooth.spline(x, y), col="red", lwd=2)

#coeff dataframe
  
gameCoefficients <- data.frame(c(0),c(0),c(0),c(0),c(0))                                 
names(gameCoefficients) <- c("(Intercept)", 
                             "poly(modeldata$Period, degree = 3, raw = T)1", 
                             "poly(modeldata$Period, degree = 3, raw = T)2", 
                             "poly(modeldata$Period, degree = 3, raw = T)3", 
                             "5")

## Dry-run
## Running one model
testCoefficients <- data.frame(c(0),c(0),c(0),c(0),c(0))                                 
names(testCoefficients) <- c("(Intercept)", 
                             "poly(modeldata$Period, degree = 3, raw = T)1", 
                             "poly(modeldata$Period, degree = 3, raw = T)2", 
                             "poly(modeldata$Period, degree = 3, raw = T)3", 
                             "5")

modeltest = subset(gameData,gamenameID==215)
testmodel <- lm(modeltest$avg ~ poly(modeltest$Period, degree = 3, raw = T))
matrix_test <- summary(testmodel)$coefficients
summary(testmodel)
testcoeff <- matrix_test[,1]
testcoeff[5] <- 215
testCoefficients <- rbind(testCoefficients,testcoeff)

## Plotting Dry-run

plot(modeltest$Period, modeltest$avg, main=paste("gameID =",215), las=1)
lines(smooth.spline(modeltest$Period, modeltest$avg), col="gray", lwd=1)
lines(smooth.spline(modeltest$Period, predict(testmodel)), col="blue", lwd=3)
lines(smooth.spline(x, y), col="red", lwd=2)

x = seq(1, 100, len = 100)
a = 1243.548
b = 66.19076
c = -1.238564
d = 0.007262876
y = (d * x^3) + (c * x^2) + (b * x) + a
y[1:100]




for (i in 1:1259) {
  modeldata = subset(gameData,gamenameID==i)
  modelpol <- lm(modeldata$avg ~ poly(modeldata$Period, degree = 3, raw=T))
  matrix_coef <- summary(modelpol)$coefficients
  summary(modelpol)
  gameCoeff <- matrix_coef[,1]
  gameCoeff[5] <- i
  gameCoefficients <- rbind(gameCoefficients, gameCoeff)
}

write.csv(gameCoefficients,"/Users/andresarias/Dropbox/MSIS/PDS/R/AID_Datathon/Game_Coeff.csv", row.names = TRUE)

# removing id in preparation for clustering and removing record 1 with cero values 
game.growthclusters1 = gameCoefficients[-c(1),-c(5)]
summary(gameCoefficients)


# scaling
game.growthclusters1 <- scale(game.growthclusters1) 
summary(game.growthclusters1)
# replacing NA w 0
game.growthclusters1[is.na(game.growthclusters1)] <- 0

# K-Means Clustering

# Calculating Numbers of Clusters for Centers (C)
wss <- (nrow(game.growthclusters1)-1)*sum(apply(game.growthclusters1,2,var))
for (i in 1:10) wss[i] <- sum(kmeans(game.growthclusters1,
                                     centers=i)$withinss)
plot(1:10, wss, type="b", main = paste('The Elbow Method for Game'), xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# Calculating Clusters 

# Fitting K-Means to the Cluster Databases
set.seed(29)
kmeans.c = kmeans(x = game.growthclusters1, centers = 4)
y_kmeans.c = kmeans.c$cluster

# Visualizing the clusters

library(cluster)

clusplot(game.growthclusters1,
         y_kmeans.c,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of Steam Games'))

# Attaching Cluster data to table with coefficients

game.growthclusters1 <- cbind(game.growthclusters1,y_kmeans.c)

# Describing Clusters by aggregating Statistics to locate center within each cluster for each position

cluster.def = aggregate(game.growthclusters1,
                          by = list(y_kmeans.c),
                          FUN = mean)

#plotting Cluster 1

x = seq(1, 100, len = 100)
a = 29.85406992
b = -32.22668889
c = 33.02296637
y = (c * x^2) + (b * x) + a
y[1:100]

plot(x, y, type = "l")

#plotting Cluster 2

x = seq(1, 100, len = 100)
a = 3.44932877
b = -2.35543150
c = 2.58542359
d = -4.068153723
y = (d * x^3) + (c * x^2) + (b * x) + a
y[1:100]

lines(smooth.spline(x, y), col="red", lwd=2)


#plotting Cluster 3

x = seq(1, 100, len = 100)
a = -0.05113229
b = 0.04242797
c = -0.04087098
d = 0.004746844
y = (d * x^3) + (c * x^2) + (b * x) + a
y[1:100]

lines(smooth.spline(x, y), col="blue", lwd=2)

#plotting Cluster 4

x = seq(1, 100, len = 100)
a = 2.91506941
b = 0.47546430
c = -5.28479582
d = 30.689322099
y = (d * x^3) + (c * x^2) + (b * x) + a
y[1:100]

lines(smooth.spline(x, y), col="green", lwd=2)

#plotting examples "manually"
gameIDplot <- 1215
gameplot <- subset(gameData, gamenameID==gameIDplot)
plot(gameplot$Period, gameplot$avg, main=paste("gameID =",gameIDplot), las=1)
singlemodel <- lm(gameplot$avg ~ poly(gameplot$Period, degree = 3, raw=T))
lines(smooth.spline(gameplot$Period, predict(singlemodel)), col="blue", lwd=3)
summary(singlemodel)

#linear regression
modelLinear <- lm(gameplot$avg ~ gameplot$Period)
summary(modelLinear)
abline(modelLinear, lwd=3, col="red")

anova(singlemodel,modelLinear)

#polynomial test 2nd
modelPol2 <- lm(gameplot$avg ~ poly(gameplot$Period, degree = 2, raw = T))
summary(modelPol2)
lines(smooth.spline(gameplot$Period, predict(modelPol2)), col="green", lwd=3)

anova(singlemodel,modelPol2)




library(tibble)
library(dplyr)
library(factoextra)
library(NbClust)
#################################################################################################3
#PRE-PROCESSING
getwd()
setwd('C:/Users/PRM/Downloads/AIS Comp/AIS Comp/supporting datasets')

df1 <- read.csv('steam.csv')
df2 <- read.csv('steamcharts.csv')
#df3 <- read.csv('Game_Coeff.csv')

length(unique(df1$name))
length(unique(df2$gamename))

l1 <- c(unique(df1$name))
l2 <- c(unique(df2$gamename))

v1 <- data.frame(l2[!(l2 %in% l1)])

#df <- merge(x=df2,y=df3,all.x=FALSE, all.y=FALSE,by.x= 'gamename',by.y = 'gamename')
df <- merge(x=df1,y=df2,all.x=FALSE, all.y=FALSE,by.x= 'name',by.y = 'gamename')
df<- na.omit(df)
df<-df[df$name!='Cyberpunk 2077',]

length(unique(df$name))

df$categories2<- ifelse(grepl("Multi-player",df$categories),'Multi-player','Single-player')

#write.csv(df,'master.csv')
#write.csv(v1,'games not matched.csv')

#################################################################################################
#KMEANS CLUSTERING

#SINGLE PLAYER
single <- df[df$categories2=='Single-player',]
single.gr <-unique(single %>% group_by(name) %>% summarise(avg = mean(avg),
                                                           gain = mean(gain),
                                                           peak = mean(peak),
                                                           achievements=achievements,
                                                           positive_ratings=positive_ratings,
                                                           negative_ratings=negative_ratings,
                                                           average_playtime=average_playtime))
single.gr <- column_to_rownames(single.gr, var = 'name')
single.t<- single.gr[,1:7]
#single.gr <-single %>% select(avg,gain,peak,positive_ratings,negative_ratings,average_playtime,price) 
single.t <- scale(single.t )
fviz_nbclust(single.t, kmeans, method = "wss")
single.km<-kmeans(single.t, centers = 6,nstart = 25)
fviz_cluster(single.km, data = single.t,geom = 'point') + ggtitle(label='Single-Player Games') 
single.ta <- cbind(single.km$cluster,single.gr)

#MULTI PLAYER
multi <- df[df$categories2=='Multi-player',]
multi.gr <-unique(multi %>% group_by(name) %>% summarise(avg = mean(avg),
                                                         gain = mean(gain),
                                                         peak = mean(peak),
                                                         achievements=achievements,
                                                         positive_ratings=positive_ratings,
                                                         negative_ratings=negative_ratings,
                                                         average_playtime=average_playtime))

multi.gr <- column_to_rownames(multi.gr, var = 'name')
multi.t<- multi.gr[,1:7]
multi.t <- scale(multi.t )
fviz_nbclust(multi.t, kmeans, method = "wss")
multi.km<-kmeans(multi.t, centers = 6,nstart = 25)
fviz_cluster(multi.km, data = multi.t,geom='point',ellipse = TRUE)+ ggtitle(label='Multi-Player Games') 
multi.ta <- cbind(multi.km$cluster,multi.gr)

#ALL GAMES
all <- df
all.gr <-unique(all %>% group_by(name) %>% summarise(avg = mean(avg),
                                                     gain = mean(gain),
                                                     peak = mean(peak),
                                                     achievements=achievements,
                                                     positive_ratings=positive_ratings,
                                                     negative_ratings=negative_ratings,
                                                     average_playtime=average_playtime))

all.gr <- column_to_rownames(all.gr, var = 'name')
all.t<- all.gr[,1:7]
all.t <- scale(all.t )
fviz_nbclust(all.t, kmeans, method = "wss")
all.km<-kmeans(all.t, centers = 3,nstart = 25)
fviz_cluster(all.km, data = all.t, geom = 'point')+ ggtitle(label='All Games') 
all.ta <- cbind(all.km$cluster,all.gr)



##########################################################################################################
#DBSCAN CLUSTERING

library("fpc")
library("factoextra")
# Compute DBSCAN using fpc package

dbscan::kNNdistplot(all.t, k =  3)
abline(h = 2.1, lty = 2)

set.seed(123)
db <- fpc::dbscan(all.t, eps = 2.1, MinPts = 4)




fviz_cluster(db, data = all.t, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point")



res.hc <- all.t %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering

# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k =  2, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


write.csv(df,'C:/Users/PRM/Downloads/AIS Comp/AIS Comp/output/master.csv',row.names = TRUE)
write.csv(single.ta,'C:/Users/PRM/Downloads/AIS Comp/AIS Comp/output/single-cluster.csv',row.names = TRUE)
write.csv(multi.ta,'C:/Users/PRM/Downloads/AIS Comp/AIS Comp/output/multi-cluster.csv',row.names = TRUE)
write.csv(all.ta,'C:/Users/PRM/Downloads/AIS Comp/AIS Comp/output/all-cluster.csv',row.names = TRUE)


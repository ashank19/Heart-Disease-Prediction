library(corrplot)
library(gridExtra)
library(factoextra)
library(caret)
library(dplyr)
library(ggplot2)
library(cluster)
install.packages("NbClust")
install.packages('fpc')
install.packages("fastDummies")
library(NbClust)
install.packages("devtools")
library("devtools")

install_github("vqv/ggbiplot")
library("ggbiplot")

heart_data= read.csv("heart.csv")
heart= heart_data

library(fpc)
#heart$HeartDisease=NULL
#head(heart)
#str(heart)
summary(heart_data)

colnames(heart_data)

ggplot(heart_data, 
       aes(x =HeartDisease , 
           fill = Sex)) + 
  geom_bar(position = "dodge")+ggtitle('Count of patient with heart disease by gender')

ggplot(heart_data, 
       aes(x =HeartDisease, 
           fill = ST_Slope)) +
  geom_bar(position = "dodge")+ggtitle('Count of patient with heart disease by ST_Slope')

install.packages("shinydashboard")

library(shinydashboard)
library(factoextra)

library(corrgram)

newdata=heart[,-12]

corrgram(heart_data[,-12], order = F,upper.panel=panel.cor,text.panel=panel.txt, main = "Correlation Plot")


vars= c('Age','RestingBP','Cholesterol','FastingBS','MaxHR')
Outliers <- c()
for(i in vars){
  max <- quantile(heart[,i], 0.75) + (IQR(heart[,i]) * 1.5 )
  min <- quantile(heart[,i], 0.25) - (IQR(heart[,i]) * 1.5 )
  idx <- which(heart[,i] < min | heart[,i] > max)
  print(paste(i, length(idx), sep=' ')) # printing variable and number of potential outliers 
  Outliers <- c(Outliers, idx) 
}

ordinal_encoding <- function(val, order = unique(val)) {
  val <- as.numeric(factor(val, levels = order, exclude = NULL))
  val
}
heart[["Sex"]]<-ordinal_encoding(heart[["Sex"]], order = c("M","F"))
heart[["ChestPainType"]]<-ordinal_encoding(heart[["ChestPainType"]], order = c("ATA","NAP","ASY","TA"))
heart[["RestingECG"]]<-ordinal_encoding(heart[["RestingECG"]], order = c("Normal","ST","LVH"))
heart[["ExerciseAngina"]]<-ordinal_encoding(heart[["ExerciseAngina"]], order = c("N","Y"))
heart[["ST_Slope"]]<-ordinal_encoding(heart[["ST_Slope"]], order = c("Up","Flat","Down"))


par(mfrow=c(2,2))
colnames <- colnames(heart[,c(4:6, 8, 10:11)])
for (i in colnames) {
  plot(heart[,i], main = paste("Plot of ", i), ylab = i)
}

new_data=heart[,1:11]

new_data=scale(new_data)

df2_pca=prcomp(new_data,center=TRUE,scale.=TRUE)

ggbiplot(df2_pca)

df_transform = as.data.frame(-df2_pca$x[,1:11])

plot_ly(data=df_transform,x=~PC1,y=~PC2,z=~PC3)

kmeans_df = kmeans(df_transform, centers = 2, nstart = 50)
fviz_cluster(kmeans_df, data = df_transform,)

new_data <- as.data.frame(new_data)

kmeans_basic_df <- data.frame(Cluster = kmeans_df$cluster, df_transform)

plot_ly(df_transform, x = ~ PC1, y = ~ PC2, z = ~ PC3, color = as.factor(kmeans_basic_df$Cluster),type='scatter3d',mode='markers')


#Gap statistic method
fviz_nbclust(new_data, FUNcluster = kmeans, method = "gap_stat")+theme_classic()

a <- fviz_nbclust(new_data, FUNcluster = kmeans, method = "silhouette") + theme_classic() 
b <- fviz_nbclust(new_data, FUNcluster = hcut, method = "silhouette") + theme_classic() 

grid.arrange(a,b)

cl_kmeans <- eclust(new_data, k=2, FUNcluster="kmeans", hc_metric="pearson", graph=FALSE)
a <- fviz_silhouette(cl_kmeans)

#b <- fviz_cluster(cl_kmeans, data = heart, elipse.type = "convex") + theme_minimal()
grid.arrange(a)

#Kmeans clustering result using pearson metric
table(cl_kmeans$cluster, heart_data$HeartDisease)

cl_kmeans1 <- eclust(new_data, k=2, FUNcluster="kmeans", hc_metric="euclidean", graph=FALSE)
g <- fviz_silhouette(cl_kmeans1)

h <- fviz_cluster(cl_kmeans1, data = new_data, elipse.type = "convex") + theme_minimal()

grid.arrange(g)

#Kmeans clustering result using euclidean metric
table(cl_kmeans1$cluster, heart$HeartDisease)

wcss=vector()

for (i in 1:10) {wcss[i]=sum(kmeans(x=new_data,centers=i)$withinss) }

plot(x=1:10, y=wcss, type='b',main='Elbow graph for clusters',xlab='Number of clusters',
     ylab='wcss')

library(dendextend)

#Hierarchical clustering

d <- dist(new_data, method = "euclidean")
hc1 <- hclust(d, method = "single" )
plot(hc1, cex = 0.6, hang = -1)
clusterCut2 <- cutree(hc1, 2)
table(clusterCut2, heart$HeartDisease)

d <- dist(new_data, method = "euclidean")
hc2 <- hclust(d, method = "average" )
plot(hc2, cex = 0.6, hang = -1)

clusterCut2 <- cutree(hc2, 2)
table(clusterCut2, heart$HeartDisease)

hc5 <- eclust(new_data, k=2, FUNcluster="diana")

pltree(hc5, cex = 0.6, hang = -1, main = "Dendrogram of DIANA")
rect.hclust(hc5, k=2, border='red')

clusterCut5 <- cutree(hc5, 2)
table(clusterCut5, heart$HeartDisease)



NbClust(new_data,distance = "euclidean",
        min.nc = 2, max.nc = 10, method = "kmeans", index = "all")

fviz_cluster((eclust(heart[,1:11], "kmeans", k = 2, nstart = 25, graph = FALSE)), geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())

cluster.stats(dist(heart[,1:11]),(eclust(heart[,1:11], "kmeans", k = 2, nstart = 25, graph = FALSE))$cluster)

(cluster.stats(dist(heart[,1:11]),(eclust(heart[,1:11], "kmeans", k = 2, nstart = 25, graph = FALSE))$cluster))$dunn

p=heart[,1:11]
kmeans_basic_df <- data.frame(Cluster = kmeans_df$cluster, p)

box=ggplot(data=kmeans_basic_df,mapping=aes(x=as.factor(Cluster), y=Age))+geom_boxplot()
box

kmeans_basic_df$Cluster=as.factor(kmeans_basic_df$Cluster)

plot_ly(kmeans_basic_df,x= ~Sex ,color=~Cluster) %>% 
  add_histogram()

heart$HeartDisease=as.factor(heart$HeartDisease)

plot_ly(heart,x= ~Sex ,color=~HeartDisease) %>% 
  add_histogram()


library(shiny)

runUrl('127.0.0.1:5508')

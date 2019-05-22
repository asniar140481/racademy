library(tidyverse)
library(cluster)
install.packages("factoextra")
library(factoextra)
temp <- agriculture
head(temp)
sum(is.na(temp))
dfnorm <- scale(temp) #untuk normalisasi atau keakurasian data
#jarak tiap data dgn centroid methode : euclidean atau manghattan distance
get_distance <- get_dist(dfnorm, method = "euclidean") 
fviz_dist(get_distance, gradient = list(low= "#00AFBB", mid = "white", high = "#FC4E07"))

#Melakukan clustering

#Mula2, menyimpan nilai kmeans, dfnorm adalah normalisasi sebelumnya,centers adalah jumlah cluster, nstart = iterasi atau jumlah pengulangan sampai nilai cluster stabil)

k2 <- kmeans (dfnorm, centers = 2, nstart = 25)
str(k2) #mengeluarkan struktur data dari k2                         
fviz_cluster(k2, data = dfnorm) # menampilkan cluster, bisa juga (k2, dfnorm)

#Cara lain Menampilkan cluster, sebanyak 2 cluster

dfnorm %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster, state = row.names(agriculture)) %>%
  ggplot(aes(x, y, color = factor(cluster), label = state)) +
  geom_text()  #geom_text untuk menampilkan label datanya

#Cara lain Menampilkan cluster, sebanyak 3 cluster, 4 cluster, dan 5 cluster

k3 <- kmeans (dfnorm, centers = 3, nstart = 25)
k4 <- kmeans (dfnorm, centers = 4, nstart = 25)
k5 <- kmeans (dfnorm, centers = 5, nstart = 25)

p2 <- fviz_cluster (k2, geom = "point", data = dfnorm) + ggtitle("k=2")
p3 <- fviz_cluster (k3, geom = "point", data = dfnorm) + ggtitle("k=3")
p4 <- fviz_cluster (k4, geom = "point", data = dfnorm) + ggtitle("k=4")
p5 <- fviz_cluster (k5, geom = "point", data = dfnorm) + ggtitle("k=5")

library(gridExtra)
grid.arrange(p2,p3,p4,p5, nrow = 2) #nrow, di figur jadi ada 2 baris

#Menentukan optimal cluster : elbow dan silluet

#Visualisasi dengan metode Elbow
fviz_nbclust(dfnorm, kmeans, method = "wss")

#Visualisasi dengan metode Sillhoute
fviz_nbclust(dfnorm, kmeans, method = "silhouette")

#Brarti optimal cluster adalah k=2

set.seed(123)#123 bebas, bisa juga 12345,dll
final <- kmeans (dfnorm, 5, nstart = 25) #mencetak cluster k=5, 25 adalah iterasi atau pengulangan nilai clustering, jumlah berapa kali training.
print(final) #mencetak nilai final
fviz_cluster(final, data = dfnorm) #visualisasi cluster k=5


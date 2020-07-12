print("Este archivo fue creado en Rstudio")
print("Ahora lo vicularemos con GitHub")
print("Incorpraremos la primera Rutina")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("cluster")

library(dplyr) 
library(ggplot2)
library(cluster)
library(readxl)
getwd()
dataset1 <- read_excel("ListadoSocios.xlsx")

head(dataset1)
names(dataset1)


#dataset <- dataset %>% 
#  rename(income = Annual.Income..k.. ,
#         spend = Spending.Score..1.100.)
#names(dataset)

colnames(dataset1) <- tolower(colnames(dataset1))
names(dataset)
dataset<-select(dataset1,un,sect,sexo)
head(dataset)
# grafico

ggplot(dataset, aes(x = sect, y =un, colour = sexo)) +
  geom_point()
?ggplot
km <- kmeans(dataset[, 1:2], 10)


# K-means clustering
set.seed(8)
w <- c()
for (i in 1:10) {
  km <- kmeans(dataset[, 1:2], i)
  w[i] <- km$tot.withinss
}


codo <- data.frame("n_cluster" = c(1:10), w)

ggplot(codo, aes(x = n_cluster, y = w)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) + 
  labs(title = "Suma cuadrados intra-cluster") +
  theme_bw()

# Ajustamos K-Means a nuestro dataset
# Ajustamos K-Means a nuestro dataset
set.seed(8)
km <- kmeans(x = dataset[, 1:2], centers = 5, iter.max = 300, nstart = 10)
cluster <- km$cluster

# Visualizaci?n de los clusters
clusplot(dataset[,1:2],
         cluster,
         lines = 1,
         shade = F,
         color = T,
         labels = 1,
         plotchar = T,
         span = TRUE,
         main = paste('Clusters de Socios'),
         xlab = 'Fecha Ingreso',
         ylab = 'Edad')
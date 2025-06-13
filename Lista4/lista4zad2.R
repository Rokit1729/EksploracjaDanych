library(mlbench)
library(stats)
library(factoextra)
library(cluster)
library(clue)

data(Glass)

dim(Glass)
#214 przypadków, 10 cech, w tym 1 kalsyfikacyjna
levels(Glass$Type)
#6 klas, 1-7 z pominięciem 4, zawarte w Type
sum(is.na(Glass))
#Brak wartości brakujących
min(Glass[-10])
max(Glass[-10])
#Liczby w zakresie 0 - 75.41
#Czy 0 to NA? Raczej nie, chodzi o zerową/znikomą ilość jakiegoś pierwiastka

#Wszystkie zmienne dobrze przypisane

#Czy 214 rekordów zalicza się "do większych"? Ja bym zostawił już te 14 dodatkowych

boxplot(Glass[-10], las=2, col="tomato") #KURWA SKALOWANIE MACHENNNNNNN

Glass[-10] <- scale(Glass[-10])
glass <- Glass[-10]

boxplot(glass, las=2, col="tomato") #Teraz zajebiście

pca_result <- prcomp(glass, scale=TRUE)
pca_summary <- summary(pca_result)$importance

#Do wykresów
xlab1 <- paste0("PC1 (", round(pca_summary[2, 1]*100, 2), "%)")
ylab1 <- paste0("PC2 (", round(pca_summary[2, 2]*100, 2), "%)")

plot(pca_result$x[, 1], pca_result$x[, 2], xlab = xlab1, ylab = ylab1, col = Glass$Type)
#Jak widać, chujowo, tylko fiolety jako tako, resztę trudno będzie rozpoznać


k <- length(levels(Glass$Type))

kmeans.k3 <- kmeans(glass, centers = k, iter.max = 10)
wyniki <- kmeans.k3$cluster

plot(pca_result$x[, 1], pca_result$x[, 2], xlab = xlab1, ylab = ylab1,
     col = Glass$Type, pch = wyniki)
#JEBAĆ K-MEANS KURWAAAA

plot(glass$RI, glass$Na, xlab = "RI", ylab = "Na",
     col = wyniki, pch = as.numeric(Glass$Type))
points(kmeans.k3$centers[,c("RI", "Na")], pch=16, cex=1.5, col=1:6)
#Tutaj, żeby centra wstawić oczywiście, bo PCA tak trochę na tym nie działa

##################################################################################################3
#PAM

conf_mat <- daisy(glass)

#fviz_dist(conf_mat, order = TRUE)
#Macierz odmienności, po uporządkowaniu

glass.pam7 <- pam(x=conf_mat, diss=TRUE, k=k)

wyniki <- glass.pam7$clustering

plot(pca_result$x[, 1], pca_result$x[, 2], xlab = xlab1, ylab = ylab1,
     col = Glass$Type, pch = wyniki)

medoids <- glass[glass.pam7$id.med, ]

plot(glass$RI, glass$Na, xlab = "RI", ylab = "Na",
     col = wyniki, pch = as.numeric(Glass$Type))
points(medoids[,c("RI", "Na")], pch=16, cex=1.5, col=1:6)


matrix <- table(wyniki, Glass$Type)

#Najlepsze dopasowanie
mapping <- solve_LSAP(matrix , maximum = TRUE)
wyniki <- mapping[wyniki]

conf_mat <- table(wyniki, Glass$Type)
rownames(matrix) <- as.character(c(1:3, 5:7))
colnames(matrix) <- as.character(c(1:3, 5:7))

kable(matrix, caption = "Macierz błędów; metoda k-średnich")
##################################################################################################3
#AGNES

conf_mat <- daisy(glass)

glass.agnes.single <- agnes(x = conf_mat,
                            diss=TRUE,
                            method="single")

fviz_dend(glass.agnes.single, cex=0.4, k = k)

glass.agnes.avg.k6 <- cutree(glass.agnes.single, k = 6)
a <- table(glass.agnes.avg.k6, Glass$Type)
a

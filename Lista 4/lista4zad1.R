library(adabag)
library(ipred)
library(rpart)
library(rpart.plot)
library(mlbench)
library(randomForest)
library(HDclassif)
library(e1071)

set.seed(21370)

data(wine)
colnames(wine) <- c(
  "Gatunek", "Alkohol", "Kwas_mlekowy", "Popiół", "Zasadowość_popiołu", 
  "Magnez", "Fenole_ogółem", "Flawonoidy", 
  "Fenole_nieflawonoidowe", "Proantocyjanidyny",
  "Koloru", "Barwa", "OD280_OD315", "Prolina"
)
wine$Gatunek <- as.factor(wine$Gatunek)
wine$Magnez <- as.numeric(wine$Magnez)
wine$Prolina <- as.numeric(wine$Prolina)

#################################################################################################

#DRZEWA DECYZYJNE

#Parametry cp oraz minsplit (można zmienić, ale takie wybrane jako optymalne na liście 3)
cp <- 0.005
minsplit <- 3

wine.tree <- rpart(Gatunek ~ .,
                   data = wine,
                   method = "class",
                   control = rpart.control(cp = cp, minsplit = minsplit))
rpart.plot(wine.tree)

#################################################################################################
#Bagging
B.vector <- c(1, 5, 10, 20, 30, 40, 50, 100)

bagging.error.rates <- sapply(B.vector, function(b)  errorest(Gatunek ~ ., data=wine, model=bagging, nbagg=b, est.para=control.errorest(nboot = 20),
                                                              estimator="632plus")$error)

bagging.error.rates1 <- sapply(B.vector, function(b)  errorest(Gatunek ~ ., data=wine, model=bagging, nbagg=b, est.para=control.errorest(nboot = 20), 
                                                               stimator="boot")$error)

bagging.error.rates2 <- sapply(B.vector, function(b)  errorest(Gatunek ~ ., data=wine, model=bagging, nbagg=b, est.para=control.errorest(nboot = 20),
                                                               estimator="cv")$error)

plot(B.vector, bagging.error.rates2, xlab="B", main="Bagging: error rate vs. B (cross-validation)", type="b")
grid()

#################################################################################################
#RandomForest
B.vector <- c(1, 5, 10, 20, 30, 40, 50, 100)

random.error.rates <- sapply(B.vector, function(b)  errorest(Gatunek ~ ., data=wine, model=randomForest, model.args = list(ntree=b), est.para=control.errorest(nboot = 20),
                                                              estimator="632plus")$error)

random.error.rates1 <- sapply(B.vector, function(b)  errorest(Gatunek ~ ., data=wine, model=randomForest, model.args = list(ntree=b), est.para=control.errorest(nboot = 20),
                                                               estimator="boot")$error)

random.error.rates2 <- sapply(B.vector, function(b)  errorest(Gatunek ~ ., data=wine, model=randomForest, model.args = list(ntree=b), est.para=control.errorest(nboot = 20),
                                                               estimator="cv")$error)

plot(B.vector, random.error.rates, xlab="B", main="Random Forest: error rate vs. B", type="b")
grid()

random.error.rates <- sapply(B.vector, function(b)  errorest(Gatunek ~ ., data=wine, model=randomForest, model.args = list(ntree=b), est.para=control.errorest(nboot = 20),
                                                             estimator="632plus")$error)
#################################################################################################
#Boosting
mypredict.rpart <- function(object, newdata)  as.factor(predict(object, newdata=newdata)$class)

boosting.error.rates <- sapply(B.vector, function(b)  errorest(Gatunek ~ ., data=wine, model=boosting, model.args = list(mfinal=b), est.para=control.errorest(nboot = 20), predict=mypredict.rpart,
                                                               estimator="632plus")$error)

boosting.error.rates1 <- sapply(B.vector, function(b)  errorest(Gatunek ~ ., data=wine, model=boosting, model.args = list(mfinal=b), est.para=control.errorest(nboot = 20), predict=mypredict.rpart,
                                                                estimator="boot")$error)

boosting.error.rates2 <- sapply(B.vector, function(b)  errorest(Gatunek ~ ., data=wine, model=boosting, model.args = list(mfinal=b), est.para=control.errorest(nboot = 20), predict=mypredict.rpart,
                                                                estimator="cv")$error)

plot(B.vector, boosting.error.rates2, xlab="B", main="Boosting: error rate vs. B", type="b")
grid()

#################################################################################################
#Porównanie błędów

mypredict.rpart <- function(object, newdata)  predict(object, newdata=newdata, type="class")
mypredict.boost <- function(object, newdata)  as.factor(predict(object, newdata=newdata)$class)

K <- 10
est <- "632plus"
lista <- list()
B.vector <- c(1, 5, 10, 20, 30, 40, 50, 100)

error.tree <- (errorest(Gatunek~., data = wine, model = rpart,
               predict = mypredict.rpart,
               estimator = est,
               est.para = control.errorest(nboot = K)))$error

error.bagging <- 0
error.randomForest <- 0
error.boosting <- 0

for(b in B.vector){
  
  error.bagging <- (errorest(Gatunek~., data = wine, model = bagging,
                                            estimator = est,
                                            est.para = control.errorest(nboot = K),
                                            nbagg = b))$error
    
  error.randomForest <- (errorest(Gatunek~., data = wine, model = randomForest,
                                                      estimator = est,
                                                      est.para = control.errorest(nboot = K),
                                                      model.args = list(ntree = b)))$error
    
  error.boosting <- (errorest(Gatunek~., data = wine, model = boosting,
                                              predict = mypredict.boost,
                                              estimator = est,
                                              est.para = control.errorest(nboot = K),
                                              model.args = list(mfinal = b)))$error
  
  poprawa <- c((error.tree - error.bagging)/error.tree*100,
               (error.tree - error.randomForest)/error.tree*100,
              (error.tree - error.boosting)/error.tree*100)
  
  lista[[as.character(b)]] <- poprawa

}

matrix <- do.call(cbind, lista)
matrix <- rbind(matrix, colSums(matrix)/3)
rownames(matrix) <- c("Bagging", "Random Forest", "Boosting", "Średnia")
View(matrix)


#################################################################################################
#SVM (Support Vector Machines)

C <- 10^(-3:3) #kara
gamma <- round(seq(from = .01, to = 10, length.out = 10), 2)
coef <- 0 #sprawdź, co to robi

#NIE działa dla kernel = "linear"
kernel.type <- function(C1 = C, gamma1 = gamma,  K = 10, size1 = 2/3, kernel){
  
  df <- as.data.frame(matrix(NA, nrow = length(C), ncol = length(gamma)))
  
  rownames(df) <- as.character(C)
  colnames(df) <- as.character(gamma)
  
  for(c in C){
    
    for(g in gamma){
      
      accuracy <- 0
      
      for(i in 1:K){
        
        id.learn <- sample(seq_len(nrow(wine)), size = floor(nrow(wine) * size1))
        train.set <- wine[id.learn, ]
        test.set <- wine[-id.learn, ]
        
        svm <- svm(Gatunek ~ ., data=train.set, kernel = kernel, gamma = g, cost = c)
        
        prediction <- predict(svm, newdata=test.set)
        prediction <- table(prediction, test.set$Gatunek)
        
        accuracy <- accuracy + sum(diag(prediction))/sum(prediction)
      }
      
      df[as.character(c), as.character(g)] <- accuracy/K
    
    }
  }
  
  return(df)
}

a1 <- kernel.type(kernel="radial")
View(a1)

a2 <- kernel.type(kernel = "sigmoid")
View(a2)

a3 <- kernel.type(kernel="polynomial")
View(a3)
max(a3)
#Najlepszy wynik dla gamma=1.12 (3 kolumna) i C=0.001
#W raporcie zrobimy tak, aby zczytywało nazwę kolumny jako gamma, oraz nazwę wiersza jako karę
#Badamy dla stopni 1:5

degree <- 1:7
df <- as.data.frame(matrix(NA, nrow=1, ncol=length(degree)))
rownames(df) <- "Dokładność"
colnames(df) <- as.character(degree)

K <- 10
for(d in degree){
  
  accuracy <- 0
  
  for(i in seq_len(K)){
    
    id.learn <- sample(seq_len(nrow(wine)), size = floor(nrow(wine) * 2/3))
    train.set <- wine[id.learn, ]
    test.set <- wine[-id.learn, ]
    
    svm <- svm(Gatunek ~ ., data=train.set, kernel = "polynomial",
               degree = d, gamma = 1.12, cost = 0.001)
    
    prediction <- predict(svm, newdata=test.set)
    prediction <- table(prediction, test.set$Gatunek)
    
    accuracy <- accuracy + sum(diag(prediction))/sum(prediction)
  
  }
  
  df[1, as.character(d)] <- accuracy/K

}
View(df)
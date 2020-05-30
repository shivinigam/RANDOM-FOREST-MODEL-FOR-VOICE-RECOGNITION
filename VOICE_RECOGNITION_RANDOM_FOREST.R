
####Loading the data####
install.packages("randomForest")
library(randomForest)
install.packages("MASS")
library(MASS)
voice_recognition <- read.csv("voice.csv")
View(voice_recognition)

####creating a random forest####
table(voice_recognition$Gender)
set.seed(171)
voice.rf <- randomForest(Gender ~., data = voice_recognition,
                       mtry = 2, importance = TRUE,
                       do.trace = 100)
print(voice.rf)

####Model Comparison####
install.packages("ipred")
library(ipred)
set.seed(131)
error.RF <- numeric(10)
for (i in 1:10) {error.RF[i] <-errorest(Gender ~., data = voice_recognition,
                                        model = randomForest,
                                        mtry = 2)$error}
summary(error.RF)
install.packages("e1071")
library(e1071)
set.seed(563)
error.SVM <- numeric(10)
for (i in 1:10) {error.SVM[i] <-errorest(Gender ~., data = voice_recognition,
                                         model = svm, cost = 10,
                                         gamma = 1.5)$error}
summary(error.SVM)

par(mfrow = c(2, 2))
for (i in 1:4) {plot(sort(voice.rf$importance[,i], dec = TRUE),
                     type = "h", main = paste("Measure", i))
  
}
View(voice_recognition)


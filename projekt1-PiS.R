
#PREDYKCJA BANKRUCTWA

library('dplyr')
library('smotefamily')
library('corrplot')
library('MASS')
library('caret')
library('neuralnet')

options(scipen = 10)
setwd("C:/Users/kszaf/OneDrive/Pulpit/I rok - mag/prognozowanie_symulacje/projekt-prognoza")

dane = read.csv("5year.arff", header=FALSE, comment.char = "@")
summary(dane)

dane[dane == "?"] <- NA

sum(is.na(dane)) #4666

# ograniczenie zbioru do istotnych zmiennych
dane2 = dane[c(5,9,13,15,22,25,27,31,36,40,42,48,52,58,65)]
sum(is.na(dane2))
dane2 = na.omit(dane2)
dane2 = as.data.frame(lapply(dane2, as.numeric))
dane2$V65 = as.factor(dane2$V65)
summary(dane2)

colnames(dane2)[15] <- "bankrupt"

M1 = cor(dane2[-c(15)])
corrplot(M1, type = "lower", method = "number")

#STANDARYZACJA

dane3 <- dane2 %>% mutate_at(c(1:14), ~(scale(.) %>% as.vector))
summary(dane3)

#SMOTE
dane2_smote = SMOTE(dane3[,-c(15)],dane3[,c(15)])
dane2_smote = dane2_smote$data 
colnames(dane2_smote)[15] <- "bankrupt"
dane2_smote$bankrupt = as.factor(dane2_smote$bankrupt)
summary(dane2_smote)

#train and test (70%-30%)

dt = sort(sample(nrow(dane2_smote), nrow(dane2_smote)*.7))
dane2_smote_train <- dane2_smote[dt,]
dane2_smote_test <- dane2_smote[-dt,]
summary(dane2_smote_test)

#LDA

model1 = lda(bankrupt ~ ., data = dane2_smote_train)
model1

#macierze b³êdów
p1 <- predict(model1, dane2_smote_test)$class
tab1 <- table(Predicted = p1, Actual = dane2_smote_test$bankrupt)
tab1 #test 
sum(diag(tab1))/sum(tab1) #76%
confusionMatrix(as.factor(dane2_smote_test$bankrupt), as.factor(p1))


#LOGIT

model2 = glm(bankrupt ~ ., data = dane2_smote_train, family = binomial(link = "logit"))
summary(model2)

probabilities1 <- model2 %>% predict(dane2_smote_test, type = "response")
predicted.classes1 <- ifelse(probabilities1 > 0.5, "1", "0")
# macierz b³êdów
mean(predicted.classes1 == dane2_smote_test$bankrupt)
confusionMatrix(as.factor(dane2_smote_test$bankrupt), as.factor(predicted.classes1)) #78%

#Neural Network

model3 = neuralnet(bankrupt ~ ., data = dane2_smote_train, act.fct = "logistic", linear.output = FALSE)
plot(model3)

#macierz b³êdów
Predict = compute(model3, dane2_smote_test)
probabilities2 <- Predict$net.result
predicted.classes2 <- ifelse(probabilities2 > 0.5, "1", "0")

mean(predicted.classes2[3104:6206] == dane2_smote_test$bankrupt)
confusionMatrix(as.factor(dane2_smote_test$bankrupt), as.factor(predicted.classes2[3104:6206]))

model3$weights 


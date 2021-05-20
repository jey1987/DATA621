library(caret)
library(dplyr)
library(gdata)
library(tidyverse)
library(Amelia)
library(mice)
library(kableExtra)
library(data.table)
library(e1071)
library(corrplot)
library(MASS)
library(pscl)
wine_train_input <- read.csv('wine-training-data.csv')
wine_eval_input <- read.csv('wine-evaluation-data.csv')
colSums(is.na(train2))

wine_train_input$INDEX <- wine_train_input$ï..INDEX

drop <- c("ï..INDEX")
wine_train_input = wine_train_input[,!(names(wine_train_input) %in% drop)]

missmap(wine_train_input, main="Missing Values")

wine_train_imputed <- mice(wine_train_input, m = 1, method = "pmm", print = F) %>% complete()
wine_eval_imputed <- mice(wine_eval_input, m = 1, method = "pmm", print = F) %>% complete()

colSums(is.na(wine_eval_imputed))

wine_train_imputed[is.na(wine_train_imputed)]=0
wine_eval_imputed[is.na(wine_eval_imputed)]=0

missmap(wine_train_imputed, main="Missing Values")

summary(wine_train_imputed) %>% kable()
head(wine_train_imputed)
wine_train_imputed %>% 
  cor(., use = "complete.obs") %>%
  corrplot(., method = "color", type = "upper", tl.col = "black", tl.cex=.8, diag = FALSE)

sapply(wine_train_imputed, skewness, function(x) skewness(x))

X <- wine_train_imputed
nonbinary <- c(1:16)
par(mfrow = c(4,4))
for (i in nonbinary) {
  hist(X[,i], xlab = names(X[i]), main = names(X[i]))
}

par(mfrow = c(4,4))
for (i in nonbinary) {
  d <- density(X[,i])
  plot(d, main = names(X[i]))
  polygon(d, col="red")
}

X <- wine_eval_imputed
nonbinary <- c(1:16)
par(mfrow = c(4,4))
for (i in nonbinary) {
  hist(X[,i], xlab = names(X[i]), main = names(X[i]))
}

par(mfrow = c(4,4))
for (i in nonbinary) {
  d <- density(X[,i])
  plot(d, main = names(X[i]))
  polygon(d, col="red")
}



set.seed(1003)
training_partition <- createDataPartition(wine_train_imputed$TARGET, p=0.8, list = FALSE, times=1)
train2 <- wine_train_imputed[training_partition, ]
test2 <- wine_train_imputed[-training_partition, ]

wine_train_imputed$INDEX <- wine_train_imputed$IN
model1 <- glm(formula = TARGET ~ ., family = poisson, data = train2)
summary(model1)

model2 <- zeroinfl(TARGET ~ ., data = train2)
summary(model2)

model3 <- glm.nb(TARGET ~ ., data = train2)
summary(model3)

vuong(model1,model2)

model1_pred <- predict(model1, test2, type = "response")
model1_class <- ifelse(model1_pred >= 0.5, 1, 0)
probability_class <- factor(model1_class, levels = c(1, 0))
actual_class <- factor(test2$TARGET, levels = c(1, 0))
confusionMatrix(probability_class, actual_class)

model2_pred <- predict(model2, test2, type = "response")
model2_class <- ifelse(model2_pred >= 0.5, 1, 0)#

probability_class <- factor(model2_class, levels = c(1, 0))
actual_class <- factor(test2$TARGET, levels = c(1, 0))
confusionMatrix(probability_class, actual_class)

model4_pred <- predict(model4, test2, type = "response")
model4_class <- ifelse(model4_pred >= 0.5, 1, 0)
probability_class <- factor(model4_class, levels = c(1, 0))
actual_class <- factor(test2$TARGET, levels = c(1, 0))
confusionMatrix(probability_class, actual_class)

model5_pred <- predict(model5, test2, type = "response")
model5_class <- ifelse(model5_pred >= 0.5, 1, 0)
probability_class <- factor(model5_class, levels = c(1, 0))
actual_class <- factor(test2$TARGET, levels = c(1, 0))
confusionMatrix(probability_class, actual_class)

model6_pred <- predict(model6, test2, type = "response")
model6_class <- ifelse(model6_pred >= 0.5, 1, 0)
probability_class <- factor(model6_class, levels = c(1, 0))
actual_class <- factor(test2$TARGET, levels = c(1, 0))
confusionMatrix(probability_class, actual_class)


model7_pred <- predict(model7, test2, type = "response")
model7_class <- ifelse(model7_pred >= 0.5, 1, 0)
probability_class <- factor(model7_class, levels = c(1, 0))
actual_class <- factor(test2$TARGET, levels = c(1, 0))
confusionMatrix(probability_class, actual_class)

model3_pred <- predict(model3, test2, type = "response")
model3_class <- ifelse(model3_pred >= 0.5, 1, 0)
probability_class <- factor(model3_class, levels = c(1, 0))
actual_class <- factor(test2$TARGET, levels = c(1, 0))
confusionMatrix(probability_class, actual_class)



model4 <- lm(TARGET ~ ., train2)
summary(model4)

model5 <- lm(TARGET ~ STARS, train2)
summary(model5)

model6 <- step(model5,direction='backward')
summary(model6)

wine_train_imputed$totalAcid <- wine_train_imputed$FixedAcidity + wine_train_imputed$VolatileAcidity + wine_train_imputed$CitricAcid
wine_eval_imputed$totalAcid <- wine_eval_imputed$FixedAcidity + wine_eval_imputed$VolatileAcidity + wine_eval_imputed$CitricAcid
wine_eval_imputed$INDEX <- wine_eval_imputed$IN
set.seed(1003)
training_partition <- createDataPartition(wine_train_imputed$TARGET, p=0.8, list = FALSE, times=1)
train2 <- wine_train_imputed[training_partition, ]
test2 <- wine_train_imputed[-training_partition, ]


model7 <- lm(TARGET ~ totalAcid, train2)
summary(model7)


eval_pred <- predict(model2, wine_eval_imputed, type = "response")
eval_class <- ifelse(eval_pred >= 0.5, 1, 0)
eval_target <- ifelse(eval_class == 1, predict(model3, wine_eval_imputed, type = "response"), 0)

wine_eval_imputed$TARGET_FLAG <- eval_class
wine_eval_imputed$TARGET <- eval_target

wine_eval_imputed %>% head() %>% kable() %>% kable_material()

ret <- write.csv(x=wine_eval_imputed, file="predicted_eval_wine.csv")
print(ret)

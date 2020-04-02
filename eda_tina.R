#tina's code

#--- DOCUMENT SETUP ---
#load libraries
library(tidyverse)
library(caret)
library(GGally)

#--- DATA SETUP ---
#import data
bank = read.csv("data/bank-additional-full.csv",sep=";")

#see the data lay of the land. There's a lot more No's than Yes's
str(bank)
head(bank)

#checking for missing data-- this has no missing data
summary(bank)
colSums(is.na(bank))

#--- CROSS VALIDATION SETUP ---

#creates a train test split with upsampling train data for balance
split_train_test <- function(bankdata) {
  # dataset with "no"
  bankdata_no = bankdata[which(bankdata$y=="no"),]
  # dataset with "yes"
  bankdata_yes = bankdata[which(bankdata$y=="yes"),]
  
  splitPerc = .7 #Training / Test split Percentage
  
  # Training / Test split of "no" dataset
  noIndices = sample(1:dim(bankdata_no)[1],round(splitPerc * dim(bankdata_no)[1]))
  train_no = bankdata_no[noIndices,]
  test_no = bankdata_no[-noIndices,]
  
  # Training / Test split of "yes" dataset
  yesIndices = sample(1:dim(bankdata_yes)[1],round(splitPerc * dim(bankdata_yes)[1]))
  train_yes = bankdata_yes[yesIndices,]
  test_yes = bankdata_yes[-yesIndices,]
  
  # Combine training "no" and "yes"
  bank_train = rbind(train_no, train_yes)
  
  # Combine test "no" and "yes"
  bank_test = rbind(test_no, test_yes)
  
  # upsampling training dataset
  bank_train_upsample <- upSample(x = bank_train[, -ncol(bank_train)], y = bank_train$y)
  colnames(bank_train_upsample)[21] <- "y"
  summary(bank_train_upsample$y)
  
  return(list(bank_train_upsample, bank_test))
}

#gives the average accuracy, sensitivity, and specificity over reps number of train-test splits
#for logistic regression model
get_test_metrics <- function(data, formula, thresh, reps){
  accuracies <- c()
  sensitivities <- c()
  specificities <- c()
  for(i in 1:reps){
    split_data <- split_train_test(data)
    train <- split_data[[1]]
    test <- split_data[[2]]
    
    model <- glm(formula, data=train,family = binomial(link="logit"))
    pred_probs <- predict(model, newdata = test, type="response")
    print(summary(pred_probs))
    pred_yns <- factor(ifelse(pred_probs>thresh, "yes", "no"))
    print(summary(pred_yns))
    
    cm <- confusionMatrix(table(pred_yns, test$y))
    accuracies <- c(accuracies, cm$overall[1])
    print(accuracies)
    sensitivities <- c(sensitivities, cm$byClass[1])
    print(sensitivities)
    specificities <- c(specificities, cm$byClass[2])
    print(specificities)
  }
  acc = mean(accuracies)
  sen = mean(sensitivities)
  spec = mean(specificities)
  result = list(acc, sen, spec)
  names(result) = c("Accuracy", "Sensitivity", "Specificity")
  return(result)
}



#--- MODELING | simple logistic regression ---

#starting with just everything
model.main<-glm(y ~ ., data=bank,family = binomial(link="logit"))
head(model.main$fitted.values)
summary(model.main)

#see accuracy metrics for this model, at threshold of .5 for mapping yes/no
m_everything <- get_test_metrics(bank, y ~ ., .5)
m_everything

#trying with manual variable selection based on what visually looked relevant
m_manual <- get_test_metrics(bank, y ~ job + default + contact + month + 
                               poutcome + duration + emp.var.rate + cons.price.idx +
                               euribor3m + nr.employed, .2, 2)
m_manual


#forward selection to see what it chooses
model.null<-glm(y ~ 1, data=bank,family = binomial(link="logit"))
model.forward <- step(model.null,
                   scope = list(upper=model.main),
                   direction="forward",
                   test="Chisq",
                   data=bank)

coef(model.forward)
m_forward <- get_test_metrics(bank, y~duration + nr.employed + month + poutcome + emp.var.rate + 
                                cons.price.idx + job + contact + euribor3m + default + day_of_week + 
                                pdays + campaign + cons.conf.idx, .5)
m_forward

#--- EVALUATING METRICS ---

#finding the best threshold for yes/no
thresholds <- seq(0.02, .6, by=.02)
accs <- c()
sens <- c()
specs <- c()
for(i in thresholds){
  print("i", i)
  print(i)
  m <- get_test_metrics(bank, y~duration + nr.employed + month + poutcome + emp.var.rate + 
                                  cons.price.idx + job + contact + euribor3m + default + day_of_week + 
                                  pdays + campaign + cons.conf.idx, i, 2)
  accs <- c(accs, m$Accuracy)
  sens <- c(sens, m$Sensitivity)
  specs <- c(specs, m$Specificity)
}

accs
sens
specs

metrics <- data.frame(thresholds,accs,sens,specs)
metrics_long <- metrics %>%
  gather(key=metric, percent, accuracies:specificities)

#plot the accuracy, sensitivity, and specificity over the thresholds; between .1-.2 seems decent to me
metrics_long %>%
  ggplot(aes(x=thresholds, y=percent, color = metric)) +
  geom_point() +
  ggtitle("Accuracy, Sensitivity, and Specificity at Thresholds")



#--- MODELING | adding complexity ---
model.main$terms
summary(model.step)

model.main<-glm(y ~ duration * nr.employed + month + poutcome + emp.var.rate + 
                  cons.price.idx + job + contact + euribor3m + default + day_of_week + 
                  pdays + campaign + cons.conf.idx, data=bank,family = binomial(link="logit"))
#things I like manually: y~job + default + contact + month + poutcome +
# duration + emp.var.rate + cons.price.idx + euribor3m + nr.employed

bank %>%
  select(c(y, job, default, contact, month, poutcome)) %>%
  ggpairs(aes())
#job*duration looks useful
bank %>%
  ggplot(aes(x=job, y=log(duration), color = y)) +
  geom_boxplot()
#default*duration looks useful
bank %>%
  ggplot(aes(x=default, y=duration, color = y)) +
  geom_boxplot()
#contact*duration looks not useful
bank %>%
  ggplot(aes(x=contact, y=duration, color = y)) +
  geom_boxplot()
#month*duration looks useful
bank %>%
  ggplot(aes(x=month, y=duration, color = y)) +
  geom_boxplot()
#poutcome*duration looks not useful
bank %>%
  ggplot(aes(x=poutcome, y=duration, color = y)) +
  geom_boxplot()
#nr.employed
#job*duration looks useful
bank %>%
  ggplot(aes(x=job, y=duration, color = y)) +
  geom_boxplot()
#default*duration looks useful
bank %>%
  ggplot(aes(x=default, y=duration, color = y)) +
  geom_boxplot()
#contact*duration looks not useful
bank %>%
  ggplot(aes(x=contact, y=duration, color = y)) +
  geom_boxplot()
#month*duration looks useful
bank %>%
  ggplot(aes(x=month, y=duration, color = y)) +
  geom_boxplot()
#poutcome*duration looks not useful
bank %>%
  ggplot(aes(x=poutcome, y=duration, color = y)) +
  geom_boxplot()
summary(model.main)

#OLD DUMP
#put yes and no into separate dataframes
yeses = bank[which(bank$y=="yes"),]
nos = bank[which(bank$y=="no"),]

library(caret)
#make 8 folds and use the entire yeses as train and test for all folds?
folds <- createFolds(nos$y, k = 8)
str(folds)
train1 = rbind(nos[folds$Fold1,],yeses)
test1 = rbind(nos[-folds$Fold1,], yeses)
train2 = rbind(nos[folds$Fold2,],yeses)
test2 = rbind(nos[-folds$Fold2,], yeses)

#make 10 folds and use random half of yeses as train and test for each fold?

folds_no <- createFolds(nos$y, k = 10)
str(folds_no)
folds_yes <- createFolds(yeses$y, k=2)
str(folds_yes)

train1 = rbind(nos[folds_no$Fold1,], yeses[folds_yes$Fold1,])
test1 = rbind(nos[-folds_no$Fold1,], yeses[-folds_yes$Fold1,])

folds_yes <- createFolds(yeses$y, k=2)
train2 = rbind(nos[folds_no$Fold2,], yeses[folds_yes$Fold1,])
test2 = rbind(nos[-folds_no$Fold2,], yeses[-folds_yes$Fold2,])

folds_yes <- createFolds(yeses$y, k=2)
train3 = rbind(nos[folds_no$Fold3,], yeses[folds_yes$Fold1,])
test3 = rbind(nos[-folds_no$Fold3,], yeses[-folds_yes$Fold2,])

folds_yes <- createFolds(yeses$y, k=2)
train4 = rbind(nos[folds_no$Fold2,], yeses[folds_yes$Fold1,])
test4 = rbind(nos[-folds_no$Fold2,], yeses[-folds_yes$Fold2,])


#lda exploration
library(GGally)
bank %>%
  ggpairs(aes(color = y))

bank_lda <- lda(Response ~ X1 + X2, data = bank)
prd <- as.numeric(predict(mylda, newdata = nd)$class)

plot(full[, 1:2], col = full$Response, main="Shift in X2")
points(mylda$means, pch = "+", cex = 2, col = c("black", "red"))
contour(x = nd.x, y = nd.y, z = matrix(prd, nrow = np, ncol = np), 
        levels = c(1, 2), add = TRUE, drawlabels = FALSE)
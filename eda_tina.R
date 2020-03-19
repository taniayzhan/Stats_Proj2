#tina's code

#load libraries
library(tidyverse)

#import data
bank = read.csv("data/bank-additional-full.csv",sep=";")

#see the data lay of the land
str(bank)
head(bank)

#checking for missing data-- this has no missing data
summary(bank)
nas <- bank[rowSums(is.na(bank)) > 0,]
dim(nas)

#train test split
yeses = bank[which(bank$y=="yes"),]
nos = bank[which(bank$y=="no"),]

require(caret)
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



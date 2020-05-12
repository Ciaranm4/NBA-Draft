setwd("/Users/cjmg2/Desktop/4th Year/Software Project/Rfiles")

library(readr)
library(dplyr)
library(tidyr)
library(feather)
library(stringr)
library(rquery)
library(corrplot)
library(mice) 
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(FactoMineR)
library(tictoc)
source("transform.R")

## linear glmnet regression model
library(glmnet)

sparseX <- sparse.model.matrix(~ + (1 + factor(Tm)) * (1 + Yrs + G + 
                                                          FG. + X3P. + FT. + Min + Pts +
                                                          Trb + Ast + WS + WS.48 + VORP + BPM + TotalPts + TotalTrb + TotalAst), combined)
tic()
m1 <- cv.glmnet(sparseX[train.set,],
                first.picks[train.set],
                alpha = 0.5,
                family = 'binomial')
combined$sparse.fr.hat <- predict(m1, newx = sparseX, type = 'response')[,1]
toc()



#See how well we do with sample
library(ROCR)
library(pROC)
preds <- prediction(combined$sparse.fr.hat[test.set], first.picks[test.set])
perf <- performance(preds, 'tpr', 'fpr')
plot(perf)
auc.perf = performance(preds, measure = "auc")
auc.perf@y.values




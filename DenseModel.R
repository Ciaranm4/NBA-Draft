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
source("glmnettest.R")


## Dense Model
library(xgboost)
fitX <- model.matrix(~ 1 + factor(Tm)  + sparse.fr.hat + Yrs + G + 
                       FG. + FT. + X3P. + Min + Pts +
                       Trb + Ast + WS + WS.48 + VORP + TotalPts + TotalTrb + TotalAst, combined)
tic()
b1.tuning <- expand.grid(depth = c(2,3,4),
                         rounds = c(50, 100, 150, 200, 250)) %>%
  group_by(depth, rounds) %>%
  do({
    m <- xgboost(data = fitX[train.set,],
                 label = first.picks[train.set],
                 max.depth = .$depth,
                 nround =.$rounds,
                 print.every.n = 50,
                 objective = 'binary:logistic')
    yhat <- predict(m, newdata = fitX)
    data_frame(test.set = test.set, yhat = yhat, label = first.picks)
  })
toc()

aucs <- b1.tuning %>%
  ungroup %>%
  filter(test.set) %>%
  group_by(depth, rounds) %>%
  do({
    auc <- performance(prediction(.$yhat, .$label), "auc")@y.values[[1]]
    data_frame(auc = auc)
  }) %>%
  ungroup %>%
  arrange(-auc)
best <- aucs %>% head(1)
best


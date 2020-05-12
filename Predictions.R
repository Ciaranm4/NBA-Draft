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
source("DenseModel.R")

library(kableExtra)
## Predicting the 2013 draft
pre2013 <- with(combined, year <2013)
b1.train <- xgboost(data = fitX[pre2013,],
                    label = first.picks[pre2013],
                    max.depth = best$depth,
                    nround = best$rounds,
                    verbose = FALSE,
                    objective = "binary:logistic")
combined$fr.hat2013 <- predict(b1.train, newdata = fitX)
preds2013 <- combined %>%
  filter(year == 2013) %>%
  arrange(-fr.hat2013) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2013) %>%
  head(30)
kable(preds2013, digits = 2)
write.csv(file = "Predicted13.csv", preds2013)


##2019 draft
pre2019 <- with(combined, year <2019)
b2.train <- xgboost(data = fitX[pre2019,],
                    label = first.picks[pre2019],
                    max.depth = best$depth,
                    nround = best$rounds,
                    verbose = FALSE,
                    objective = "binary:logistic")
combined$fr.hat2019 <- predict(b2.train, newdata = fitX)
preds2019 <- combined %>%
  filter(year == 2019) %>%
  arrange(-fr.hat2019) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2019) %>%
  head(30)
kable(preds2019, digits = 2)
write.csv(file = "Predicted19.csv", preds2019)


## Predicting the 2000 draft
pre2000 <- with(combined, year <2000)
b4.train <- xgboost(data = fitX[pre2000,],
                    label = first.picks[pre2000],
                    max.depth = best$depth,
                    nround = best$rounds,
                    verbose = FALSE,
                    objective = "binary:logistic")
combined$fr.hat2000 <- predict(b4.train, newdata = fitX)
preds2000 <- combined %>%
  filter(year == 2000) %>%
  arrange(-fr.hat2000) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2000) %>%
  head(28)
kable(preds2000, digits = 2)
write.csv(file = "Predicted00.csv", preds2000)

## Predicting the 2013 draft
pre2001 <- with(combined, year <2001)
b5.train <- xgboost(data = fitX[pre2001,],
                    label = first.picks[pre2001],
                    max.depth = best$depth,
                    nround = best$rounds,
                    verbose = FALSE,
                    objective = "binary:logistic")
combined$fr.hat2001 <- predict(b5.train, newdata = fitX)
preds2001 <- combined %>%
  filter(year == 2001) %>%
  arrange(-fr.hat2001) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2001) %>%
  head(27)
kable(preds2001, digits = 2)
write.csv(file = "Predicted01.csv", preds2001)

## Predicting the 2013 draft
pre2002 <- with(combined, year <2002)
b6.train <- xgboost(data = fitX[pre2002,],
                    label = first.picks[pre2002],
                    max.depth = best$depth,
                    nround = best$rounds,
                    verbose = FALSE,
                    objective = "binary:logistic")
combined$fr.hat2002 <- predict(b6.train, newdata = fitX)
preds2002 <- combined %>%
  filter(year == 2002) %>%
  arrange(-fr.hat2002) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2002) %>%
  head(27)
kable(preds2002, digits = 2)
write.csv(file = "Predicted02.csv", preds2002)

## Predicting the 2013 draft
pre2003 <- with(combined, year <2003)
b7.train <- xgboost(data = fitX[pre2003,],
                    label = first.picks[pre2003],
                    max.depth = best$depth,
                    nround = best$rounds,
                    verbose = FALSE,
                    objective = "binary:logistic")
combined$fr.hat2003 <- predict(b7.train, newdata = fitX)
preds2003 <- combined %>%
  filter(year == 2003) %>%
  arrange(-fr.hat2003) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2003) %>%
  head(30)
kable(preds2003, digits = 2)
write.csv(file = "Predicted13.csv", preds2003)


## Predicting the 2013 draft
pre2004 <- with(combined, year <2004)
b8.train <- xgboost(data = fitX[pre2004,],
                    label = first.picks[pre2004],
                    max.depth = best$depth,
                    nround = best$rounds,
                    verbose = FALSE,
                    objective = "binary:logistic")
combined$fr.hat2004 <- predict(b8.train, newdata = fitX)
preds2004 <- combined %>%
  filter(year == 2004) %>%
  arrange(-fr.hat2004) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2004) %>%
  head(30)
kable(preds2004, digits = 2)
write.csv(file = "Predicted04.csv", preds2004)


## Predicting the 2013 draft
pre2005 <- with(combined, year <2005)
b9.train <- xgboost(data = fitX[pre2005,],
                    label = first.picks[pre2005],
                    max.depth = best$depth,
                    nround = best$rounds,
                    verbose = FALSE,
                    objective = "binary:logistic")
combined$fr.hat2005 <- predict(b9.train, newdata = fitX)
preds2005 <- combined %>%
  filter(year == 2005) %>%
  arrange(-fr.hat2005) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2005) %>%
  head(30)
kable(preds2005, digits = 2)
write.csv(file = "Predicted05.csv", preds2005)


## Predicting the 2013 draft
pre2006 <- with(combined, year <2006)
b10.train <- xgboost(data = fitX[pre2006,],
                     label = first.picks[pre2006],
                     max.depth = best$depth,
                     nround = best$rounds,
                     verbose = FALSE,
                     objective = "binary:logistic")
combined$fr.hat2006 <- predict(b10.train, newdata = fitX)
preds2006 <- combined %>%
  filter(year == 2006) %>%
  arrange(-fr.hat2006) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2006) %>%
  head(30)
kable(preds2006, digits = 2)
write.csv(file = "Predicted06.csv", preds2006)


## Predicting the 2013 draft
pre2007 <- with(combined, year <2007)
b11.train <- xgboost(data = fitX[pre2007,],
                     label = first.picks[pre2007],
                     max.depth = best$depth,
                     nround = best$rounds,
                     verbose = FALSE,
                     objective = "binary:logistic")
combined$fr.hat2007 <- predict(b11.train, newdata = fitX)
preds2007 <- combined %>%
  filter(year == 2007) %>%
  arrange(-fr.hat2007) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2007) %>%
  head(30)
kable(preds2007, digits = 2)
write.csv(file = "Predicted07.csv", preds2007)


## Predicting the 2008 draft
pre2008 <- with(combined, year <2008)
b12.train <- xgboost(data = fitX[pre2008,],
                     label = first.picks[pre2008],
                     max.depth = best$depth,
                     nround = best$rounds,
                     verbose = FALSE,
                     objective = "binary:logistic")
combined$fr.hat2008 <- predict(b12.train, newdata = fitX)
preds2008 <- combined %>%
  filter(year == 2008) %>%
  arrange(-fr.hat2008) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2008) %>%
  head(30)
kable(preds2008, digits = 2)
write.csv(file = "Predicted08.csv", preds2008)


## Predicting the 2009 draft
pre2009 <- with(combined, year <2009)
b13.train <- xgboost(data = fitX[pre2009,],
                     label = first.picks[pre2009],
                     max.depth = best$depth,
                     nround = best$rounds,
                     verbose = FALSE,
                     objective = "binary:logistic")
combined$fr.hat2009 <- predict(b13.train, newdata = fitX)
preds2009 <- combined %>%
  filter(year == 2009) %>%
  arrange(-fr.hat2009) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2009) %>%
  head(30)
kable(preds2009, digits = 2)
write.csv(file = "Predicted09.csv", preds2009)


## Predicting the 20110 draft
pre2010 <- with(combined, year <2010)
b14.train <- xgboost(data = fitX[pre2010,],
                     label = first.picks[pre2010],
                     max.depth = best$depth,
                     nround = best$rounds,
                     verbose = FALSE,
                     objective = "binary:logistic")
combined$fr.hat2010 <- predict(b14.train, newdata = fitX)
preds2010 <- combined %>%
  filter(year == 2010) %>%
  arrange(-fr.hat2010) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2010) %>%
  head(30)
kable(preds2010, digits = 2)
write.csv(file = "Predicted10.csv", preds2010)

## Predicting the 2011 draft
pre2011 <- with(combined, year <2011)
b15.train <- xgboost(data = fitX[pre2011,],
                     label = first.picks[pre2011],
                     max.depth = best$depth,
                     nround = best$rounds,
                     verbose = FALSE,
                     objective = "binary:logistic")
combined$fr.hat2011 <- predict(b15.train, newdata = fitX)
preds2011 <- combined %>%
  filter(year == 2011) %>%
  arrange(-fr.hat2011) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2011) %>%
  head(30)
kable(preds2011, digits = 2)
write.csv(file = "Predicted11.csv", preds2011)

## Predicting the 2012 draft
pre2012 <- with(combined, year <2012)
b16.train <- xgboost(data = fitX[pre2012,],
                     label = first.picks[pre2012],
                     max.depth = best$depth,
                     nround = best$rounds,
                     verbose = FALSE,
                     objective = "binary:logistic")
combined$fr.hat2012 <- predict(b16.train, newdata = fitX)
preds2012 <- combined %>%
  filter(year == 2012) %>%
  arrange(-fr.hat2012) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2012) %>%
  head(30)
kable(preds2012, digits = 2)
write.csv(file = "Predicted12.csv", preds2012)

## Predicting the 2014 draft
pre2014 <- with(combined, year <2014)
b17.train <- xgboost(data = fitX[pre2014,],
                     label = first.picks[pre2014],
                     max.depth = best$depth,
                     nround = best$rounds,
                     verbose = FALSE,
                     objective = "binary:logistic")
combined$fr.hat2014 <- predict(b17.train, newdata = fitX)
preds2014 <- combined %>%
  filter(year == 2014) %>%
  arrange(-fr.hat2014) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2014) %>%
  head(30)
kable(preds2014, digits = 2)
write.csv(file = "Predicted14.csv", preds2014)

## Predicting the 2015 draft
pre2015 <- with(combined, year <2015)
b18.train <- xgboost(data = fitX[pre2015,],
                     label = first.picks[pre2015],
                     max.depth = best$depth,
                     nround = best$rounds,
                     verbose = FALSE,
                     objective = "binary:logistic")
combined$fr.hat2015 <- predict(b18.train, newdata = fitX)
preds2015 <- combined %>%
  filter(year == 2015) %>%
  arrange(-fr.hat2015) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2015) %>%
  head(30)
kable(preds2015, digits = 2)
write.csv(file = "Predicted15.csv", preds2015)

## Predicting the 2016 draft
pre2016 <- with(combined, year <2016)
b19.train <- xgboost(data = fitX[pre2016,],
                     label = first.picks[pre2016],
                     max.depth = best$depth,
                     nround = best$rounds,
                     verbose = FALSE,
                     objective = "binary:logistic")
combined$fr.hat2016 <- predict(b19.train, newdata = fitX)
preds2016 <- combined %>%
  filter(year == 2016) %>%
  arrange(-fr.hat2016) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2016) %>%
  head(30)
kable(preds2016, digits = 2)
write.csv(file = "Predicted16.csv", preds2016)


## Predicting the 2018 draft
pre2018 <- with(combined, year <2018)
b20.train <- xgboost(data = fitX[pre2018,],
                     label = first.picks[pre2018],
                     max.depth = best$depth,
                     nround = best$rounds,
                     verbose = FALSE,
                     objective = "binary:logistic")
combined$fr.hat2018 <- predict(b20.train, newdata = fitX)
preds2018 <- combined %>%
  filter(year == 2018) %>%
  arrange(-fr.hat2018) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2018) %>%
  head(30)
kable(preds2018, digits = 2)
write.csv(file = "Predicted18.csv", preds2018)

## Predicting the 2017 draft
pre2017 <- with(combined, year <2017)
b21.train <- xgboost(data = fitX[pre2017,],
                     label = first.picks[pre2017],
                     max.depth = best$depth,
                     nround = best$rounds,
                     verbose = FALSE,
                     objective = "binary:logistic")
combined$fr.hat2017 <- predict(b21.train, newdata = fitX)
preds2017 <- combined %>%
  filter(year == 2017) %>%
  arrange(-fr.hat2017) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, Pk, Player, Tm, fr.hat2017) %>%
  head(30)
kable(preds2017, digits = 2)
write.csv(file = "Predicted17.csv", preds2017)

#model <- rbind(preds2000, preds2001, preds2002, preds2003, preds2004, preds2005, preds2006, preds2007, preds2008, preds2009, preds2010, preds2011, preds2012, preds2013, preds2014, preds2015, preds2016, preds2017, preds2018, preds2019)
Dashboardsub <- subset(combined, select = c(2:4, 7:24, 27:47))
write.csv(file = "dashboarddata.csv", Dashboardsub)


draft2000 <- subset(preds2000, select = c(1:4))
draft2001 <- subset(preds2001, select = c(1:4))
draft2002 <- subset(preds2002, select = c(1:4))
draft2003 <- subset(preds2003, select = c(1:4))
draft2004 <- subset(preds2004, select = c(1:4))
draft2005 <- subset(preds2005, select = c(1:4))
draft2006 <- subset(preds2006, select = c(1:4))
draft2007 <- subset(preds2007, select = c(1:4))
draft2008 <- subset(preds2008, select = c(1:4))
draft2009 <- subset(preds2009, select = c(1:4))
draft2010 <- subset(preds2010, select = c(1:4))
draft2011 <- subset(preds2011, select = c(1:4))
draft2012 <- subset(preds2012, select = c(1:4))
draft2013 <- subset(preds2013, select = c(1:4))
draft2014 <- subset(preds2014, select = c(1:4))
draft2015 <- subset(preds2015, select = c(1:4))
draft2016 <- subset(preds2016, select = c(1:4))
draft2017 <- subset(preds2017, select = c(1:4))
draft2018 <- subset(preds2018, select = c(1:4))
draft2019 <- subset(preds2019, select = c(1:4))

model <- rbind(draft2000, draft2001, draft2002,draft2003,draft2004,draft2005,draft2006,draft2007,draft2008,draft2009,draft2010,draft2011,draft2012,draft2013,draft2014,draft2015,draft2016,draft2017,draft2018,draft2019)
write.csv(file = "PredictionModels.csv", model)
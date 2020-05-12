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
source("Scrape.R")

salary = read.csv("salary.csv", stringsAsFactors=T, na.strings=c(NA,"NA"," NA", "na"))
Draft = read.csv("Draft1.csv", stringsAsFactors=T, na.strings=c(NA,"NA"," NA", "na"))
pythonDraft = read.csv("RealDraft.csv", stringsAsFactors=T, na.strings=c(NA,"NA"," NA", "na"))
names(pythonDraft)[names(pythonDraft)=="ï..i.d"]<-"i.d"

levels(Draft$Tm)[levels(Draft$Tm)=="SEA"]<-"OKC"


draftsub <- subset(Draft, select = c(1:6,8:24))
pythonsub <- subset(pythonDraft, select = c(1,4,12:13))

combined <- merge(draftsub, pythonsub, by=c("i.d"))
combined[is.na(combined)] <- 0
names(combined)[names(combined)=="PTS"]<-"TotalPts"
names(combined)[names(combined)=="MP"]<-"TotalMin"
names(combined)[names(combined)=="TRB"]<-"TotalTrb"
names(combined)[names(combined)=="AST"]<-"TotalAst"
names(combined)[names(combined)=="PTS.1"]<-"Pts"
names(combined)[names(combined)=="MP.1"]<-"Min"
names(combined)[names(combined)=="TRB.1"]<-"Trb"
names(combined)[names(combined)=="AST.1"]<-"Ast"
names(combined)[names(combined)=="PK"]<-"Pk"

write.csv(file = "Combined.csv", combined)
#combo <- read.csv("combined.csv", stringsAsFactors=F, na.strings=c(NA,"NA"," NA", "na"))

N <- nrow(combined)
train.set <- (rbinom(N, 1, prob = 0.75) == 1 & combined$year < 2019)
test.set <- (!train.set & combined$year < 2019)
holdout.set <- !(test.set | train.set)

pick <- combined$PK
winshare <- combined$WS.48
first.picks <- as.numeric(combined$Pk <= 30)
first.rounders <- as.numeric(combined$Pk <= 30)













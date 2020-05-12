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
source("cleaning.R")


#Correlation Matrix
cor <- cor(combined[c("Pk", "G", "Pts", "Ast", "Trb", "WS.48", "BPM")])
corrplot(cor, method="pie")
corrplot(cor, type = "lower")
cor


## Word Cloud
docs <- Corpus(VectorSource(combined$ORGANIZATION))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

#wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=467, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))


##Histograms 
ggplot(combined, aes(x=FG.)) + geom_histogram(color="blue", fill="white")

##Scatter Plots
ggplot(combined, aes(x=Pk, y=VORP)) + geom_point() + geom_smooth(methom=lm)

ggplot(combined, aes(x=FG., y=Pts)) + geom_point() + geom_smooth(methom=lm, se=FALSE)

ggplot(combined, aes(x=FG., y=X3P.)) + geom_point() + geom_smooth()

ggplot(combined, aes(x=FG., y=Pts)) + geom_point(color="red") + geom_smooth(methom=lm,se=FALSE)

ggplot(combined, aes(x=Pts, y=Ast, shade=VORP, color=Pk)) + geom_point() + geom_smooth(methom=lm)

combo <- subset(combined, select = c(4,8,13:23))

##PCA
pcares <- PCA(combined[c(4,8, 13:23)])
summary(pcares)

##Supplementray variables
pcares <- PCA(combo, quanti.sup = 1:2, quali.sup =3)
summary(pcares)
round(pcares$eig,3)
plot(pcares, choix="var")

dimdesc(pcares)

plot(pcares, cex=0.8, invisible = "quali", title="Individuals PCA graph")
plot(pcares, cex=0.8, habbillage=3)


Scree.Plot <- function(R,main="Scree Plot",sub=NULL){
  roots <- eigen(R)$values
  x <- 1:dim(R)[1]
  plot(x,roots,type="b",col='blue',ylab="Eigenvalue",
       xlab="Component Number",main=main,sub=sub) 
  abline(h=1,lty=2,col="red")
  
}
R <- cor(combo[,3:13])
Scree.Plot(R,main ="Scree Plot (NBA Draft Data)")

install.packages("SnowballC")
install.packages("dplyr")
install.packages(tm)
install.packages("stringr")
install.packages("rpart")
install.packages("tidytext")
install.packages("data.table")
install.packages("pROC")
install.packages("rpart")
install.packages('randomForest')
install.packages("plyr")
install.packages("gplots")
install.packages("plotROC")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("gridExtra")
install.packages("caret")
install.packages("lattice")


#*************************************************************************************************************************

library(tm)
library(dplyr)
library(stringr)
library(rpart)
library(tidytext)
library(data.table)
library(pROC)
library(rpart)
library(randomForest)
library(plyr)
library(gplots)
library(plotROC)
library(ggplot2)
library(ggthemr)
library(ggthemes)
library(gridExtra)
library(caret) # for confusion matrix
library(lattice)


#****************** Reading DATA ************************************************


setwd("C:/Users/Ravi/Desktop/Social")

# Defining bag of dictioary
Dictionary <- read.csv("Stopword_dictionary.csv", header = FALSE,stringsAsFactors=FALSE)


docs_negative <- Corpus(DirSource(directory = "negative"))
#docs_negative <- Corpus(DirSource(directory = "trial1"))
#docs_neutral <- Corpus(DirSource(directory = "trial2"))
docs_neutral <- Corpus(DirSource(directory = "neutral"))
docs_positive <- Corpus(DirSource(directory = "positive"))


#******************** Cleaning DATA **************************#
#To remove punctuations

docs_negative<-tm_map(docs_negative,removePunctuation)
docs_neutral<-tm_map(docs_neutral,removePunctuation)
docs_positive<-tm_map(docs_positive,removePunctuation)


#To remove whitespaces 
docs_negative<-tm_map(docs_negative,stripWhitespace)
docs_neutral<-tm_map(docs_neutral,stripWhitespace)
docs_positive<-tm_map(docs_positive,stripWhitespace)

#To change to lower case 
docs_negative<-tm_map(docs_negative,content_transformer(tolower))
docs_neutral<-tm_map(docs_neutral,content_transformer(tolower))
docs_positive<-tm_map(docs_positive,content_transformer(tolower))


#To remove numbers from docs_negative
docs_negative<-tm_map(docs_negative,removeNumbers)
docs_neutral<-tm_map(docs_neutral,removeNumbers)
docs_positive<-tm_map(docs_positive,removeNumbers)


# To Remove stems
docs_negative <- tm_map(docs_negative,stemDocument)
docs_neutral <- tm_map(docs_neutral,stemDocument)
docs_positive <- tm_map(docs_positive,stemDocument)


# Remove Stop  words
docs_negative <- tm_map(docs_negative,removeWords,Dictionary)
docs_neutral <- tm_map(docs_neutral,removeWords,Dictionary)
docs_positive <- tm_map(docs_positive,removeWords,Dictionary)



#***********************************************************************************************************************************

# Coverting into Data Matrix
matrix_negative <- DocumentTermMatrix(docs_negative)
matrix_neutral <- DocumentTermMatrix(docs_neutral)
matrix_positive <- DocumentTermMatrix(docs_positive)


# Converting into Data Frame

negative_df <- data.frame(as.matrix(matrix_negative), stringsAsFactors=FALSE)
neutral_df <- data.frame(as.matrix(matrix_neutral), stringsAsFactors=FALSE)
positive_df <- data.frame(as.matrix(matrix_positive), stringsAsFactors=FALSE)

#*******************************************************************************************************************************

# Adding Polarity to the documents

negative_sentiments <- negative_df %>% mutate(polarity = -1)
neutral_sentiments <- neutral_df %>% mutate(polarity = 0)
positive_sentiments <- positive_df %>% mutate(polarity = 1)


#************************ Merging all kinds of sentimental data into one dataframe ***************************************************************


all_sentiments = list(negative_sentiments,neutral_sentiments,positive_sentiments)
all_sentiments <- ldply(all_sentiments, data.frame)
all_sentiments[1,]



#******************************** Dividing into train,validation and test data set************************************************************

train_rows <- sample(1:nrow(all_sentiments), 0.50*nrow(all_sentiments)) 
train_data <- all_sentiments[train_rows, ]

valid_test <- all_sentiments[-train_rows, ]

valid_raws <- sample(1:nrow(valid_test), 0.50*nrow(valid_test))
 
valid_data <- valid_test[valid_raws,]
test_data <- valid_test[-valid_raws,]
test_data


#****************************** Modeling ***********************************************

# Custom function to calculate AUC: 
auc = function(trueval, predval){
  df = as.data.frame(cbind(trueval,predval))
  names(df) = c("trueval","predval")
  auc = roc(trueval~predval,data=df)$auc
  return(auc)
}

logitMod <- glm(polarity ~ ., data=train_data, family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, test_data, type="response"))
predicted

plotROC(test_data$polarity, predicted)

confusionMatrix(test_data$polarity, predicted, threshold = 0.55)



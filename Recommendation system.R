#------------ Required Packages and Library -------------#

# install.packages("arules")
# install.packages("recommenderlab")
# install.packages("Rmpfr") # select n
# install.packages("tidyr")
# install.packages("reshape2")
# install.packages("Metrics")
# install.packages("Matrix")
# install.packages("ggplot2")
# install.packages("proxy")


#------------- Loading Libraries ------------------------#

library("arules")
library("recommenderlab")
library("tidyr")
library("dplyr")
library("reshape2")
library("Metrics")
library("Matrix")
library("ggplot2")
library("proxy")

#----------- Working Directory Set and Get --------------#

setwd('C:/Users/Ravi/Desktop/Rec Sys Assignment Ravi')
getwd()

#----------- Reading Data and Manipulations -------------#

# Dataset 1 : information and details about Artists
artists_details = read.csv('artists.dat', sep="\t") 
artists_details$id %>% unique() %>% length() # Total number of Artists = 14036
head(artists_details)
dim(artists_details)


# Data 2 : User tag artist in particular genre tag Id
user_tagged_artists <- read.csv('user_taggedartists.dat', sep="\t")
head(user_tagged_artists)
dim(user_tagged_artists)
head(user_tagged_artists)
Top_genre <- user_tagged_artists %>% group_by(tagID) %>% summarise( Count_of_listeners = length(unique(userID))) %>% arrange(desc(Count_of_listeners))
Top_genre[1:10,]
hist(Top_genre$Count_of_listeners,main=" GENRE BY LISTERNERS FREQUENCY", xlab="Tags IDs", col="blue",las=1)
# Number of listner in each genre is very skewed
Top_genre$tagID%>% unique() %>% length() # 9749 unique Tag Ids

# Decrease Skewness by taking 300 top tag ids
Top_genre <- Top_genre[1:300,]
hist(Top_genre$Count_of_listeners,main=" GENRE BY LISTERNERS FREQUENCY", xlab="Tags IDs (300)", col="blue",las=1)
# Skewness got highly reduced

# Subsetting user_tagged_artist according to the Top_genre 300
user_tagged_artists <- subset(user_tagged_artists,tagID %in% Top_genre$tagID)
dim(user_tagged_artists)

# Boxplot for removing outliers
boxplot(user_tagged_artists$year, main="YEAR OF TAG CREATION",xlab="Year",horizontal = TRUE)

# Deleting outliers ( deleted 3 Observations)
user_tagged_artists <- user_tagged_artists[user_tagged_artists$year >= 2000,]
head(user_tagged_artists)
dim(user_tagged_artists)
user_tagged_artists$artistID %>% unique() %>% length()
user_tagged_artists$userID %>% unique() %>% length()



# Dataset 3 : Weitage given to artists by users

user_artists_weight = read.csv('user_artists.dat', sep="\t")
dim(user_artists_weight)
head(user_artists_weight)
user_artists_weight %>% summarise(max = max(weight),Min = min(weight),Mean = mean(weight), Median = median(weight))
glimpse(user_artists_weight)
user_artists_weight$userID %>% unique() %>% length() # Total 1892 Users
user_artists_weight$artistID %>% unique() %>% length() # Total 17632 Artists
hist(user_artists_weight$weight, main="Histogram for Artist's weight", xlab="Weight", col="blue",las=1)
# weight is very skewed so we catagorised it in band to make it less skewed
# decreasing some skewness by making range band  for Weight variables value
user_artists_weight$weight <- ifelse( user_artists_weight$weight %in% 0:50, 50,
                       ifelse( user_artists_weight$weight %in% 51:100, 100,
                       ifelse( user_artists_weight$weight %in% 101:150,150,                        
                       ifelse( user_artists_weight$weight %in% 151:200,200, 
                       ifelse( user_artists_weight$weight %in% 201:250,250,
                       ifelse( user_artists_weight$weight %in% 251:300,300,
                       ifelse( user_artists_weight$weight %in% 301:350,350, 
                       ifelse( user_artists_weight$weight %in% 351:400,400,
                       ifelse( user_artists_weight$weight %in% 401:450,450,
                       ifelse( user_artists_weight$weight %in% 451:500,500,
                       ifelse( user_artists_weight$weight %in% 501:600,600,
                       ifelse( user_artists_weight$weight %in% 601:700,700,
                       ifelse( user_artists_weight$weight %in% 701:800,800,
                       ifelse( user_artists_weight$weight %in% 801:900,900,
                       ifelse( user_artists_weight$weight %in% 901:1000,1000,
                       ifelse( user_artists_weight$weight %in% 1001:1200,1200,
                       ifelse( user_artists_weight$weight %in% 1201:1400,1400,
                       ifelse( user_artists_weight$weight %in% 1401:1600,1600,
                       ifelse( user_artists_weight$weight %in% 1601:1800,1800,
                       ifelse( user_artists_weight$weight %in% 1801:2000,2000,2500
                                                                           ))))))))))))))))))))
head(user_artists_weight) 
table(user_artists_weight$weight)
user_artists_weight %>% summarise(max = max(weight),Min = min(weight),Mean = mean(weight), Median = median(weight))
hist(user_artists_weight$weight,main="Histogram for Artist's weight", xlab="Weight", col="blue",las=1) 
# Skweness is highly reduces in weight

# calculate number of users listening to each artist
Artist_by_listners <- user_artists_weight %>% group_by(artistID) %>% summarise(TotalListners = length(unique(userID))) %>% arrange(desc(TotalListners))
head(Artist_by_listners)
dim(Artist_by_listners)
hist(Artist_by_listners$TotalListners,main="Artist's listenining frequency ", xlab="Artists", col="blue",las=1)
# Artist by number of listner is also very skewed
# Decreasing skweness of artist by number of  listner by deleting list heard artists

Top_Artist <- Artist_by_listners[1:2000,] # choosing Top 2000 Artists out of 17632
hist(Top_Artist$TotalListners,main="Artist's listenining frequency ", xlab="Artists (2000)", col="blue",las=1)
# Skwenesss in listening frequency get slightly reduced  

# Subset user_artist dataset according to the TopArtist dataset 
user_artists_weight$userID %>% unique() %>% length()
user_artists_weight <- subset(user_artists_weight,artistID %in% Top_Artist$artistID)
user_artists_weight$userID %>% unique() %>% length()
dim(user_artists_weight)

# Data 4 : Song genre withs its tag Id
tags <- read.csv('tags.dat', sep="\t")
tags[1:10,]
tags$tagID %>% unique() %>% length() # Total 11946 different tag Ids


#-------------------- CREATING WIDE BASETABLES MATRIXES --------------------#
#---------------------------------------------------------------------------#

# Prearing base table for collaborative filtering recommendation
# spreding user_tagged_artists
CF_Basetable <- spread(user_artists_weight,artistID,weight)
CF_Basetable[1:10,1:10]
dim(CF_Basetable) # rows=1871 & colms=1001

# Deleting Artists witch are not tagged by atleast 50 users
no_ratings_cutoff <- 50

if (length(which(colSums(is.na(CF_Basetable)*1) <= length(CF_Basetable[,1]) - no_ratings_cutoff)) > 0){
  CF_Basetable <- CF_Basetable[,-which(colSums(is.na(CF_Basetable)) <= length(CF_Basetable[,1]) - no_ratings_cutoff)]
}

dim(CF_Basetable)

# Subsetting user_tagged artists with respect to CF_Basetable
CF_basetable_artistID = colnames(CF_Basetable)
length(CF_basetable_artistID)
CF_basetable_userID = rownames(CF_Basetable)
length(CF_basetable_userID)

user_tagged_artists$userID %>% unique() %>% length()
user_tagged_artists$artistID %>% unique() %>% length()
user_tagged_artists = subset(user_tagged_artists, artistID %in% CF_basetable_artistID)
user_tagged_artists = subset(user_tagged_artists, userID %in% CF_basetable_userID)
user_tagged_artists$artistID %>% unique() %>% length() # 673 uniques Artists left
user_tagged_artists$userID %>% unique() %>% length()

# Subsetting CF_Basetable so that same Artists left in CF_Basetable and user_tagged_artists
CF_Basetable <- CF_Basetable[,which(colnames(CF_Basetable) %in% user_tagged_artists$artistID)]
CF_Basetable[1:10,1:10]
dim(CF_Basetable) # Final base table for collaborative filtering has rows:1871 & Colms:673

# Preaparing base table for content filtering recommendation

#merging user_tagged_artist and tags data set 
tagged_merged <- merge(x= user_tagged_artists,y = tags, by= "tagID",all.x =TRUE)
head(tagged_merged)

# selecting required coloumns from merged tagged dataset
artist_tag <- tagged_merged[,c("artistID","tagValue")] 
head(artist_tag)

# grouping artist_tag by artistsId and tagvalue 
# creating one variable name CountOfTags
artist_tag <- artist_tag %>% group_by(artistID,tagValue) %>% summarise(CountOfTags = length(tagValue))
head(artist_tag)

# Spreading artists_tag dataset and creating basetabe for content based filtering
CB_Basetable <- spread(artist_tag,tagValue,CountOfTags)
CB_Basetable = as(CB_Basetable, "matrix")
CB_Basetable[is.na(CB_Basetable)] <- 0
CB_Basetable[1:10,c(1,67,89,34,81,34,56,80)]
dim(CB_Basetable)

# creating matrix for factorization table in content based filteration
CF_Basetable_Matrix <- as(CF_Basetable,"matrix")
dim(CF_Basetable_Matrix)
rownames(CF_Basetable_Matrix) <- rownames(CF_Basetable)
CF_Basetable_Matrix[1:10,1:10]

# Artists in CF_Basetable_Matrix and CB_Basetable must be common
dim(CF_Basetable_Matrix)
dim(CB_Basetable)

#---------------------------- 2. CRETATING FUNCTIONS -----------------------------#
#---------------------------------------------------------------------------------#


#-----Creating user based collaborative function ---------#

User_Based_Collaborative_Function <- function(train_data, test_data, N, NN, onlyNew=TRUE){
  
  st <- proc.time()
  
  # creating similarity matrix
  similarity_matrix <- matrix(NA, nrow = nrow(test_data), ncol = nrow(train_data), 
                              dimnames = list(rownames(test_data), rownames(train_data)))
   
  colmeans = colMeans(train_data)
  k=0 
  for (i in rownames(test_data)){
    for (j in rownames(train_data)){
      
      mean_i = mean(test_data[i,],na.rm=TRUE)
      mean_j = mean(train_data[j,],na.rm=TRUE)
      
      sim <- sum((test_data[i, ] - mean_i)*(train_data[j,] - mean_j), na.rm=TRUE) / sqrt((sum(((test_data[i, ] - mean_i)^2), na.rm=TRUE))* (sum(((train_data[j,] - mean_j)^2), na.rm=TRUE)))
      similarity_matrix[i,j] <- sim
    }
    k = k + 1
    cat("\n"," Similarity matrix for observation num :",k)
  }
  print("similarity calculation done")
  
  # Finding Nearest Neighbors 
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:nrow(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
    similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], NA)
  }
  
  print("Nearest Neighbor selection done")
  
  ### Prediction ###
  
  prediction <- matrix(NA, nrow=nrow(test_data), ncol(test_data), 
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(NA, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
  
  TopN <- matrix(NA, nrow=nrow(test_data), ncol=N, dimnames=list(rownames(test_data)))
  
  ### Numerator ###
  
  for (u in rownames(test_data)){
    similarity_vector <- na.omit(similarity_matrix_NN[u, ])
    
    NN_norm <- train_data[rownames(train_data) %in% names(similarity_vector),]
    
    CM <- colMeans(train_data, na.rm=TRUE)
    for (l in 1:ncol(NN_norm)){
      NN_norm[,l] <- NN_norm[,l] - CM[l]
    }
    NN_norm[is.na(NN_norm)] <- 0
    
    # Numerator
  
      Num = similarity_vector %*% as.matrix(NN_norm)
    
    #Prediction
    
      prediction[u, ] =  sum(test_data[u, ], na.rm=TRUE)/length(test_data[u, ])  + (Num/sum(similarity_vector, na.rm=TRUE))
    
    
    if (onlyNew == TRUE){
      unseen <- names(test_data[u, is.na(test_data[u,])])
      prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
    
    TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
    
  }
  
  print("Prediction done")
  
  Time <- (proc.time() - st)
  
  cat("\n","TIME TAKEN TO RUN USER BASED COLLABORATIVE FUNCTION :",Time[][3], " Seconds")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
  
  
}


#---------------- Creating item based collaborative functiom -----------------#

Item_Based_Collaborative_Function <- function(train_data, test_data, N, NN, onlyNew=TRUE){
  
  st <- proc.time()
   
    
  # creating Similarity matrix
  
  similarity_matrix = matrix(NA, ncol=ncol(train_data), nrow=ncol(train_data), dimnames = list(colnames(train_data), colnames(train_data)))
  rowmeans = rowMeans(train_data)
  
  # Establish a counter to track how far through the loop the code has reached; report at every 20% point
  k = 0
  for (i in colnames(train_data)){
    for (j in colnames(train_data)){
      if (j > i){
        mean_i <- mean(train_data[,i],na.rm=TRUE)
        mean_j <- mean(train_data[,j],na.rm=TRUE)
        
        sim <- sum((train_data[,i] - mean_i) * (train_data[,j] - mean_j), na.rm=TRUE) / sqrt( (sum(((train_data[,i] - mean_i)^2), na.rm=TRUE)) * (sum(((train_data[,j] - mean_j)^2), na.rm=TRUE)))
        
        similarity_matrix[i, j] <- sim
        similarity_matrix[j, i] <- sim
      }
      
    }
    k = k + 1
    cat("\n", " Creating similarity matrix for observation number :",k)
  }
  
  print("Similarity calculation done")
  
  # Nearest Neighbor
  
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:ncol(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[,k])[NN]
    similarity_matrix_NN[,k] <- ifelse(similarity_matrix_NN[,k] >= crit_val, similarity_matrix_NN[,k], NA)
  }
  similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0
  
  train_data[is.na(train_data)] <- 0
  
  test_data2 <- test_data
  test_data2[is.na(test_data2)] <- 0
  
  print("Nearest neighbor selection done")
  
  ### Prediction ###
  
  prediction <- matrix(NA, nrow=nrow(test_data), ncol=ncol(test_data), 
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(NA, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
  TopN <- matrix(NA, nrow=nrow(test_data), N, dimnames=list(rownames(test_data)))
  
  for (u in rownames(test_data)){
    # Numerator
  
      Num <-  test_data2[u, ] %*% similarity_matrix_NN
    
    # Denominator
    
      Denom <- colSums(similarity_matrix_NN, na.rm=TRUE)
    
    # Prediction
    
      prediction[u, ] <- Num/Denom
    
    if (onlyNew == TRUE){
      unseen <- names(test_data[u, is.na(test_data[u,])])
      prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
    
    TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
    
  }
  
  print("Prediction done")
  
  Time <- (proc.time() - st)
  
  cat("\n"," TIME TAKEN TO RUN ITEM BASED COLLABORATIVE FUNCTION :",Time[][3], " Seconds")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
  
  
  
}



#----------- Creating Contenct based Function ---------------------------# 
   

Content_Based_Function <- function(product_data, test_data, N, NN, onlyNew=TRUE){
  
  st <- proc.time()
  
  # Similarity matrix creation
  similarity_matrix <- as.matrix(simil(product_data, method="cosine"))
  
  print("Similarity calculation done")
  
  # Set Nearest neighbors 
  
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:nrow(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
    similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], 0)
  }
  
  similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0
  test_data2 <- test_data
  test_data2[is.na(test_data2)] <- 0
  
  print("Nearest neighbor selection done")
  
  # Prediction 
  prediction <- matrix(NA, nrow=nrow(test_data), ncol=ncol(test_data), 
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(NA, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
  TopN <- matrix(NA, nrow=nrow(test_data), N, dimnames=list(rownames(test_data)))
  
  for (u in rownames(test_data)){
    # Numerator
    
    Num <-  test_data2[u, ] %*% similarity_matrix_NN
    
    # Denominator
    
    Denom <- colSums(similarity_matrix_NN, na.rm=TRUE)
    
    # Prediction
    
    prediction[u, ] <- Num/Denom
    
    if (onlyNew == TRUE){
      unseen <- names(test_data[u, is.na(test_data[u,])])
      prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
    
    TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
    
  }
  
  print("Prediction done")
  
  Time <- (proc.time() - st)
  
  cat("\n"," TIME TAKEN TO RUN CONTENT BASED FILTERING FUNCTION :",Time[][3], " Seconds")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
  
  
 
}

#-------- Creating Cluster based filterartion function -------------#


Cluster_Based_Collaborative_Function <- function(data, N, centers, iter, onlyNew=TRUE){
  
  st <- proc.time()
  
  data2 <- data
  
  # fill with average product rating
  colmeans <- colMeans(data2, na.rm=TRUE)
  
  for (j in colnames(data2)){
    data2[, j] <- ifelse(is.na(data2[ ,j]), colmeans[j], data2[, j])
  }
  
  km <- kmeans(data2, centers=centers, iter.max=iter)
  
  head(km$cluster)
  head(km$centers)
  
  
  # Statistics of the groups
  tab <- table(km$cluster)
  
  # Assign users to groups
  RES <- cbind(data, as.data.frame(km$cluster))
  
  aggregation <- aggregate(RES, list(RES$"km$cluster"), mean, na.rm=T)
  aggregation <- aggregation[,-1]
  
  # Make a prediction
  users <- as.data.frame(RES$"km$cluster")
  users <- cbind(users, rownames(RES))
  colnames(users) <- c("km$cluster", 'rn')
  
  
  prediction = merge(users, aggregation, by="km$cluster")
  rownames(prediction) <- prediction$rn
  
  prediction  <- prediction[order(rownames(prediction)), -1:-2]
  
  prediction2 <- matrix(NA, nrow=nrow(prediction), ncol(prediction), 
                        dimnames=list(rownames(prediction), colnames(prediction)))
  colnames(prediction2) <- colnames(prediction)
  rownames(prediction2) <- rownames(prediction)
  
  for (u in rownames(prediction)){
    #if (onlyNew == TRUE){
    unseen <- names(data[u, is.na(data[u,])])
    
    prediction2[u, ] <- as.numeric(t(ifelse(colnames(prediction) %in% unseen, prediction[u, ], as.numeric(NA))))
  }
  
  
  # TopN
  TopN <- t(apply(prediction, 1, function(x) names(head(sort(x, decreasing=TRUE), 5))))
  
  print("Prediction done")
  
  Time <- (proc.time() - st)
  
  cat("\n"," TIME TAKEN TO RUN CLUSTER BASED COLLABORATIVE FUNCTION :",Time[][3]," Seconds")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
 
} 


#--------------------------------------------------------------------#
#----------------- CREATING Evaluation Functions---------------------#
#--------------------------------------------------------------------#


#----- MAE Evaluation Function --------#

MAE_Function <- function(prediction, real){
  
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    MAE = ( sum( abs(prediction - real) , na.rm = TRUE ) / (nrow(prediction) * ncol(prediction)) )
    return(MAE)
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}


#----------------------Creating F1 Function ----------------------# 


F1_Function <- function(prediction, real, threshold=NA, TopN=NA){
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    
    # Threshold #
    
    if (!is.na(threshold)){
      TP = sum(ifelse(prediction >= threshold & real >= threshold, 1, 0), na.rm=T)
      FP = sum(ifelse(prediction >= threshold & real < threshold, 1, 0), na.rm=T)
      FN = sum(ifelse(prediction < threshold & real >= threshold, 1, 0), na.rm=T)
      Recall = TP/(TP+FN)
      Precision = TP/(TP+FP)
      F1 <-  ifelse(Precision + Recall == 0, 0, 2 * Precision * Recall / (Precision + Recall))
      Class_Thres = list(Recall, Precision, F1)
      names(Class_Thres) = c("Recall", "Precision", "F1")
    }
    if (!is.na(TopN)){
      TP = vector(NA, length = nrow(prediction))
      FP = vector(NA, length = nrow(prediction))
      FN = vector(NA, length = nrow(prediction))
      
      for (i in nrow(prediction)){
        threshold_pred = -sort(-prediction[i, ])[TopN]
        threshold_real = -sort(-real[i, ])[TopN]
        TP[i] = sum(ifelse(prediction[i, ] >= threshold_pred & real[i, ] >= threshold_real, 1, 0), na.rm=T)
        FP[i] = sum(ifelse(prediction[i, ] >= threshold_pred & real[i, ] < threshold_real, 1, 0), na.rm=T)
        FN[i] = sum(ifelse(prediction[i, ] < threshold_pred & real[i, ] >= threshold_real, 1, 0), na.rm=T)
      }
      TP = sum(TP[i])
      FP = sum(FP[i])
      FN = sum(FN[i])
      Recall = TP/(TP+FN)
      Precision = TP/(TP+FP)
      F1 <-  ifelse(Precision + Recall == 0, 0, 2 * Precision * Recall / (Precision + Recall))
      Class_TopN = list(Recall, Precision, F1)
      names(Class_TopN) = c("Recall", "Precision", "F1")
    }
    
    
    if (!is.na(threshold) & !is.na(TopN)){
      Class = list(Class_Thres, Class_TopN)
      names(Class) = c("Threshold", "TopN")
    }else if (!is.na(threshold) & is.na(TopN)) {
      Class = Class_Thres
    }else if (is.na(threshold) & !is.na(TopN)) {
      Class = Class_TopN
    }else{
      Class = "You have to specify the 'Threshold' or 'TopN' parameter!"
    }
    return(Class)  
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}

#---------------------------- RMSE ------------------------------------#

RSME <- function(prediction, real){
  
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    RSME = sqrt( sum( (prediction - real)^2 , na.rm = TRUE ) / (nrow(prediction) * ncol(prediction)) )
    return(RSME)
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}


#----------------------------------------------------------------------#
#---------------------------- MODELLINGS ------------------------------#
#----------------------------------------------------------------------#


#------------------- Creating Train and Test Dataset ------------------# 

set.seed(100)
n = nrow(CF_Basetable)
trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
train_data <- as(CF_Basetable[trainIndex,],"matrix")
test_data <- as(CF_Basetable[-trainIndex,],"matrix")
train_data[1:10,1:10]


#-------------------------- Model Execution ----------------------------#

# User based model execution
User_based_Results <- User_Based_Collaborative_Function(train_data, test_data, N = 3, NN = 10, onlyNew=TRUE)

# Item based model execution
Item_based_Results <- Item_Based_Collaborative_Function(train_data, test_data, N = 3, NN = 10, onlyNew=TRUE)

# Content based model execution
Content_based_results <- Content_Based_Function(CB_Basetable,test_data, 3, 10, onlyNew=T)

# Cluster based model execution
Clusterer_based_Results <- Cluster_Based_Collaborative_Function(CF_Basetable_Matrix, N = 3 , centers = 5, iter = 100, onlyNew=TRUE)

#-----------------------------------------------------------------------------#
#------------------------------- HYBRID MODELS -------------------------------#
#-----------------------------------------------------------------------------#

#------- 1. Hybrid Model ( User based & Item based collaboration filters)-----#

# Hybrid is prepared by computing mean of results of parent models
User_based_Prediction_List <- as.list(User_based_Results$prediction)
Item_based_Prediction_list <- as.list(Item_based_Results$prediction)

# taking row mean of predictions of parent data 
UI_Hybrid_mean = rowMeans(cbind(as.numeric(User_based_Prediction_List), as.numeric(Item_based_Prediction_list)), na.rm=TRUE)

# Transform UI_Hybrid_mean matrix in respect of test data
UI_Hybrid_prediction <- matrix(UI_Hybrid_mean, nrow=nrow(test_data), ncol=ncol(test_data))
rownames(UI_Hybrid_prediction) <- rownames(test_data)
colnames(UI_Hybrid_prediction) <- colnames(test_data)

#----------- 2. Hybrid Models ( Content Based & Item Based --------------------#

### Transform results to lists (to be able to use the rowMeans function)
Content_based_Prediction_list <- as.list(Content_based_results$prediction)
Item_based_Prediction_list <- as.list(Item_based_Results$prediction)

# taking row mean of predictions of parent data 
CI_Hybrid_mean = rowMeans(cbind(as.numeric(Content_based_Prediction_list), as.numeric(Item_based_Prediction_list)), na.rm=TRUE)

# Transform UI_Hybrid_mean matrix in respect of test data
CI_Hybrid_prediction <- matrix(CI_Hybrid_mean, nrow=nrow(test_data), ncol=ncol(test_data))
rownames(CI_Hybrid_prediction) <- rownames(test_data)
colnames(CI_Hybrid_prediction) <- colnames(test_data)

#------------------------------------------------------------------------------#
#-------------------------- ACCURACY & TESTS ----------------------------------#
#------------------------------------------------------------------------------#

#--------------- MAE: Accuracy determination for Prediction -------------------#


#User based  
User_based_MAE = MAE_Function(User_based_Results$prediction,test_data)
User_based_MAE

#Item based
Item_based_MAE = MAE_Function(Item_based_Results$prediction,test_data) 
Item_based_MAE

#Content based
Content_based_MAE = MAE_Function(Content_based_results$prediction,test_data)
Content_based_MAE

#Cluster Based
Cluster_based_MAE = MAE_Function(Clusterer_based_Results$prediction,CF_Basetable)
Cluster_based_MAE

# UI_Hybrid
UI_Hybrid_MAE = MAE_Function(UI_Hybrid_prediction,test_data)
UI_Hybrid_MAE

# CI_Hybrid
CI_Hybrid_MAE = MAE_Function(CI_Hybrid_prediction,test_data)
CI_Hybrid_MAE

#---------------- F1 : Accuracy for classification -----------------------#

#User based 
User_based_F1 = F1_Function(User_based_Results$prediction, test_data, threshold=3)
User_based_F1

#Item based
Item_based_F1 = F1_Function(Item_based_Results$prediction, test_data, threshold=3)
Item_based_F1

#Content based
Content_based_F1 = F1_Function(Content_based_results$prediction,test_data, threshold = 3)
Content_based_F1

#Cluster Based
Cluster_based_F1 = F1_Function(Clusterer_based_Results$prediction,CF_Basetable,threshold = 3)
Cluster_based_F1

# UI_Hybrid
UI_Hybrid_F1 = F1_Function(UI_Hybrid_prediction,test_data,threshold = 3)
UI_Hybrid_F1

# CI_Hybrid
CI_Hybrid_F1 = F1_Function(CI_Hybrid_prediction,test_data,threshold = 3)
CI_Hybrid_F1

#---------------------------- RMSE -------------------------------------#


# User Based
User_based_RSME <- RSME(User_based_Results$prediction,test_data)
User_based_RSME

# Item Based
Item_based_RSME <- RSME(Item_based_Results$prediction,test_data)
Item_based_RSME

# Content based
Content_based_RSME <- RSME(Content_based_results$prediction,test_data)
Content_based_RSME

# Cluster based
Cluster_based_RSME <- RSME(Clusterer_based_Results$prediction,CF_Basetable)
Cluster_based_RSME

# UI Hybrid
Hybrid_RSME <- RSME(UI_Hybrid_prediction,test_data)
Hybrid_RSME

# CI_Hybrid
CI_Hybrid_RSME = RSME(CI_Hybrid_prediction,test_data)
CI_Hybrid_RSME


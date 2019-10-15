#Libraries
library(RCurl)
library(tm)
library(tidyverse)
library(readxl)
library(cluster) 
library(factoextra)
library(proxy)

file <- getURL("https://raw.githubusercontent.com/BobAdamsEE/SouthParkData/master/All-seasons.csv")
SP <- read.csv(text = file,stringsAsFactors = FALSE)

#Repeating algroithm with different cuss list and agregating by character
Cuss_new=c(unique(tolower(lexicon::profanity_alvarez)))

#Converting to corpus and manipulating
SP_group = SP %>% group_by(Character) %>% summarise(Lines = paste0(Line, collapse = ""))
SP_corpus_t = Corpus(VectorSource(SP_group$Lines))
SP_corpus_1_t = tm_map(SP_corpus_t, content_transformer(tolower))
SP_corpus_2_t = tm_map(SP_corpus_1_t, removeNumbers)
SP_corpus_3_t = tm_map(SP_corpus_2_t, removePunctuation)
SP_corpus_4_t = tm_map(SP_corpus_3_t, removeWords, c("the", "and", stopwords("english")))
SP_corpus_5_t= tm_map(SP_corpus_4_t, stripWhitespace)
SP_corpus_6_t = tm_map(SP_corpus_5_t, stemDocument)

#Creating a term document matrix with weightings
SP_dtm_t = DocumentTermMatrix(SP_corpus_6_t,control = list(weighting = weightTfIdf))

#Only including cusswords
index_2_t = as.vector(colnames(SP_dtm_t) %in% Cuss_new)
SP_dtm_cuss_2_t = SP_dtm_t[,index_2_t] %>% as.matrix()

#Similarity matrix across documents
Distances_t = proxy::dist(SP_dtm_cuss_2_t, method = "cosine")

#Experimenting with differing clustering algorithms
#Setting random seed and number of clusters
set.seed(2019)
clusters_t = 6

#Fitting clusters
kmeansResult_t = kmeans(Distances_t, centers = clusters_t)

#Appending cluster assignment to original dataframe
SP_dtm_cuss_3_t = SP_dtm_cuss_2_t %>% as.data.frame() %>%
  mutate(Cluster_t = kmeansResult_t$cluster) 

#Finding mean scores for each cluster
Cluster_info_t = SP_dtm_cuss_3_t %>% group_by(Cluster_t) %>% summarise_all(mean)

#Finding the top three scores for each cluster
#Will use these scores to create cussing themes
Cuss_themes_t = matrix(nrow = clusters_t ,ncol = 6)

#Populating Cuss_themes
for(row in 1:nrow(Cluster_info_t)){
  
  #Finding the column order number
  Order_index_t = Cluster_info_t[row,1:ncol(Cluster_info_t)] %>% unlist() %>% as.numeric() %>% order()
  #Ordering that row by the index
  Cluster_info_2_t = Cluster_info_t[row,c(Order_index_t)]
  #Extracting the three largest value in each row
  value_t = Cluster_info_t[row,2:ncol(Cluster_info_t)] %>% unlist() %>% as.numeric %>% 
    sort() %>% tail(3)
  #Removing from the analysis if it is zero
  value_2_t = value_t[value_t>0]
  word_index_t = which((Cluster_info_2_t %>% unlist() %>% as.numeric()%>% sort()) %in% value_2_t)
  #Extracting the word name
  word_t = colnames(Cluster_info_2_t)[word_index_t][1:3]
  rep_value_t = 3 - length(word_t)
  #Combining it all together
  temp_t = c(rbind(word_t,rep(0,rep_value_t),value_2_t))
  Cuss_themes_t[row,] = temp_t
}

#This will show you the most popular cuss words for each cluster.
#Note the blank row.. these are characters that dont cuss
View(Cuss_themes_t)

#Final cluster assignments for the top 20 characters
final_clust = SP_group %>% mutate(Cluster = kmeansResult_t$cluster) %>% filter(Character %in% Character_popular_most) %>% select(Character,Cluster)

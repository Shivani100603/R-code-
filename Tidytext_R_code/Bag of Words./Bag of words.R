

install.packages("tidyverse")
install.packages("tidytext")
install.packages("wordcloud2")
install.packages("wordcloud")
install.packages("textdata")



library(tidyverse)
library(tidytext)
library(textdata)
library(wordcloud)
library(wordcloud2)





Bk1 <- read.delim("C:/Users/shiva/OneDrive - University of Plymouth/Big Data Analytics/Books or data needed for report/NLP Book 1/Book_1.txt", header=FALSE, comment.char="#")


Bk2 <- read.csv("C:/Users/shiva/OneDrive - University of Plymouth/Big Data Analytics/Books or data needed for report/Book_2.txt", header=FALSE, sep=";")


Bk3 <- read.table("C:/Users/shiva/OneDrive - University of Plymouth/Big Data Analytics/Books or data needed for report/Book_3.txt", header = FALSE, sep=";", quote="\"")

Bk4 <-  read.csv2("C:/Users/shiva/OneDrive - University of Plymouth/Big Data Analytics/Books or data needed for report/Book_4.txt", header=FALSE, comment.char="#")

Bk5 <- read.table("C:/Users/shiva/OneDrive - University of Plymouth/Big Data Analytics/Books or data needed for report/Book_5.txt", header = FALSE, sep=";", quote="\"")



Bk6 <- read.table("C:/Users/shiva/OneDrive - University of Plymouth/Big Data Analytics/Books or data needed for report/Book_6.txt", header = FALSE, sep=";", quote="\"")

Bk7 <- read.table("C:/Users/shiva/OneDrive - University of Plymouth/Big Data Analytics/Books or data needed for report/Book_7.txt", header = FALSE,  sep=";", quote="'", comment.char="")






Bk8 <-  read.table("C:/Users/shiva/OneDrive - University of Plymouth/Big Data Analytics/Books or data needed for report/Book_8.txt", header = FALSE, sep=";", quote="\"")





Bk1<- Bk1 %>% 
  select(V1) %>%
  unnest_tokens(word,V1) 




head(Bk1)



Bk1  %>%   # what does %>% do here or so?
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort = TRUE) %>%  # This could be a boolean agreement
  reshape2 :: acast(word ~ sentiment, value.var = "n" , fill = 0) %>%    # This line of code looks confusing to me.
  comparison.cloud(colors = c("red", "green"), max.words = 100)

Bk2<- Bk2 %>% 
  select(V1) %>%
  unnest_tokens(word,V1)




Bk2  %>%   # what does %>% do here or so?
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort = TRUE) %>%  # This could be a boolean agreement
  reshape2 :: acast(word ~ sentiment, value.var = "n" , fill = 0) %>%    # This line of code looks confusing to me.
  comparison.cloud(colors = c("red", "green"), max.words = 100)




Bk3<- Bk3 %>% 
  select(V1) %>%
  unnest_tokens(word,V1)



Bk3  %>%   
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort = TRUE) %>%  
  reshape2 :: acast(word ~ sentiment, value.var = "n" , fill = 0) %>%   
  comparison.cloud(colors = c("red", "green"), max.words = 100)


Bk4<- Bk4 %>% 
  select(V1) %>%
  unnest_tokens(word,V1)

Bk4  %>%   # what does %>% do here or so?
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort = TRUE) %>%  
  reshape2 :: acast(word ~ sentiment, value.var = "n" , fill = 0) %>%   
  comparison.cloud(colors = c("red", "green"), max.words = 100)



Bk5<- Bk5 %>% 
  select(V1) %>%
  unnest_tokens(word,V1)

Bk5  %>%   # what does %>% do here or so?
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort = TRUE) %>%  
  reshape2 :: acast(word ~ sentiment, value.var = "n" , fill = 0) %>%   
  comparison.cloud(colors = c("red", "green"), max.words = 100)




Bk6<- Bk6 %>% 
  select(V1) %>%
  unnest_tokens(word,V1)

Bk6  %>%   # what does %>% do here or so?
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort = TRUE) %>%  
  reshape2 :: acast(word ~ sentiment, value.var = "n" , fill = 0) %>%    
  comparison.cloud(colors = c("red", "green"), max.words = 100)


Bk7<- Bk7 %>% 
  select(V1) %>%
  unnest_tokens(word,V1)


Bk7  %>%   # what does %>% do here or so?
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort = TRUE) %>%  
  reshape2 :: acast(word ~ sentiment, value.var = "n" , fill = 0) %>%    
  comparison.cloud(colors = c("red", "green"), max.words = 100)



Bk8<- Bk8 %>% 
  select(V1) %>%
  unnest_tokens(word,V1)

Bk8  %>%   # what does %>% do here or so?
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort = TRUE) %>%  
  reshape2 :: acast(word ~ sentiment, value.var = "n" , fill = 0) %>%    
  comparison.cloud(colors = c("red", "green"), max.words = 100)








Bk3 %>% 
  cross_join(get_sentiments("nrc"))%>% # Whats wrong with this?
  count(word,sentiment,sort=TRUE)%>% 
  reshape2::acast(word ~sentiment, value.var="n",fill=0)%>% 
  comparison.cloud(colors=c("blue","red","gold","green","purple","hotpink","seagreen", "darkblue","darkmagenta"), max.words=100)

library(dplyr)




Bk1 %>%
  cross_join(get_sentiments("loughran"))%>%
  count(word,sentiment,sort=TRUE)%>% 
  reshape2::acast(word ~sentiment, value.var="n",fill=0)%>% 
  comparison.cloud(colors=c("blue","red","gold","green","purple","hotpink","seagreen", "darkblue","darkmagenta"), max.words=100)





Bk1_words<- Bk1 %>% 
  select(V1) %>% 
  unnest_tokens(word,V1) 

Bk2_words<- Bk2 %>% 
  select(V1) %>% 
  unnest_tokens(word,V1) 

Bk3_words<- Bk3 %>% 
  select(V1) %>% 
  unnest_tokens(word,V1) 


Bk4_words<- Bk4 %>% 
  select(V1) %>% 
  unnest_tokens(word,V1) 

Bk5_words<- Bk5 %>% 
  select(V1) %>% 
  unnest_tokens(word,V1) 

Bk6_words<- Bk6 %>% 
  select(V1) %>% 
  unnest_tokens(word,V1) 

Bk7_words<- Bk7 %>% 
  select(V1) %>% 
  unnest_tokens(word,V1) 

Bk8_words<- Bk8 %>% 
  select(V1) %>% 
  unnest_tokens(word,V1) 





# Removing the stop words

Bk1_words_clean <- Bk1_words %>% anti_join(stop_words)
Bk2_words_clean <- Bk2_words %>% anti_join(stop_words)

Bk3_words_clean <- Bk3_words %>% anti_join(stop_words)

Bk4_words_clean <- Bk4_words %>% anti_join(stop_words)

Bk5_words_clean <- Bk5_words %>% anti_join(stop_words)

Bk6_words_clean <- Bk6_words %>% anti_join(stop_words)

Bk7_words_clean <- Bk7_words %>% anti_join(stop_words)

Bk8_words_clean <- Bk8_words %>% anti_join(stop_words)



Bk8_words_clean %>% group_by(word) %>%  count(sort = TRUE) %>% head(15) %>%
  ggplot(aes(x = word, y = n)) +  
  geom_col(fill = "cyan", color = "blue") +  
  coord_flip() +
  labs(x = "Unique words", y = "Frequency" , 
       title = "Count of unique words found") + 
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
            title = element_text(size = 14))



Bk2_words_clean %>% group_by(word) %>%  count(sort = TRUE) %>% head(25) %>%
  ggplot(aes(x = word, y = n)) +  
  geom_col(fill = "cyan", color = "blue") +  
  coord_flip() +
  labs(x = "Unique words", y = "Frequency" , 
       title = "Count of unique words found") + 
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        title = element_text(size = 14))




Bk4_words_clean %>% group_by(word) %>%  count(sort = TRUE) %>% head(15) %>%
  ggplot(aes(x = word, y = n)) +  
  geom_col(fill = "cyan", color = "blue") +  
  coord_flip() +
  labs(x = "Unique words", y = "Frequency" , 
       title = "Count of unique words found") + 
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        title = element_text(size = 14))


Bk6_words_clean %>% group_by(word) %>%  count(sort = TRUE) %>% head(10) %>%
  ggplot(aes(x = word, y = n)) +  
  geom_col(fill = "cyan", color = "blue") +  
  coord_flip() +
  labs(x = "Unique words", y = "Frequency" , 
       title = "Count of unique words found") + 
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        title = element_text(size = 14))



Bk4_words_clean %>% group_by(word) %>%  count(sort = TRUE) %>% head(15) %>%
  ggplot(aes(x = word, y = n)) +  
  geom_col(fill = "cyan", color = "blue") +  
  coord_flip() +
  labs(x = "Unique words", y = "Frequency" , 
       title = "Count of unique words found") + 
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        title = element_text(size = 14))

# Look to see how words_clean is defined here or so. Hmmm. 


Bk6_words_clean %>% group_by(word) %>%  count(sort = TRUE) %>% head(15) %>%
  ggplot(aes(x = word, y = n)) +  
  geom_col(fill = "cyan", color = "blue") +  
  coord_flip() +
  labs(x = "Unique words", y = "Frequency" , 
       title = "Count of unique words found") + 
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        title = element_text(size = 14))



Bk7_words_clean %>% group_by(word) %>%  count(sort = TRUE) %>% head(15) %>%
  ggplot(aes(x = word, y = n)) +  
  geom_col(fill = "cyan", color = "blue") +  
  coord_flip() +
  labs(x = "Unique words", y = "Frequency" , 
       title = "Count of unique words found") + 
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        title = element_text(size = 14))



Bk8_words_clean %>% group_by(word) %>%  count(sort = TRUE) %>% head(25) %>%
  ggplot(aes(x = word, y = n)) +  
  geom_col(fill = "cyan", color = "blue") +  
  coord_flip() +
  labs(x = "Unique words", y = "Frequency" , 
       title = "Count of unique words found") + 
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        title = element_text(size = 14))



# N-gram analysis code. 

# Bigrams

Bk1_Trigrams_Step_One <- Bk1 %>%
  dplyr::select(V1) %>%
  unnest_tokens(trip_words, V1, token = "ngrams" , n = 3)





wordcloud(Bk1_Trigrams_Step_One ,scale=c(5,0.5), 
          max.words=50,
          random.order = FALSE,
          rot.per=0.35,  
          use.r.layout=FALSE , 
          colors=brewer.pal(8,"Dark2"))


          
          
          
Bk1_bigrams<- Bk1 %>%
  dplyr::select(10) %>%
  unnest_tokens(paired_words, 10, token = "ngrams", n = 2)


Bk1_bigrams_separated <- Bk1_bigrams %>%
  separate(paired_words, c("word1" , "word2"), sep = " ") 



Bk1_bigrams_separated <- Bk1_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 
  
Bk1_Bigrams <- Bk1_bigrams_separated %>%
  count(word1, word2, sort = TRUE) 

Bk1_Bigrams <- Bk1_Bigrams %>% na.omit()




Bk1_Bigrams_graph <- Bk1_Bigrams %>% 
  filter(n > 2) %>% 
  graph_from_data_frame() 


ggraph(Bk1_Bigrams_graph, layout = "fr") + 
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1,size=4)+ 
  theme_graph(base_size=10)+ 
  ggtitle("Network of words")




Bk1_trigrams<- Bk1 %>% 
  dplyr::select(V1) %>% 
  unnest_tokens(trip_words, V1, token = "ngrams", n = 3) book_1_3_grams_seperated <- book_1_3_grams %>% 
  separate(trip_words, c("word1", "word2","word3"), sep = " ") book_1_3_grams_seperated <- book_1_3_grams_seperated %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) book_1_tripgrams <- book_1_3_grams_seperated %>% 
  count(word1, word2,word3, sort = TRUE) book_1_tripgrams<-book_1_tripgrams %>% 
  na.omit() book_1_tripgrams_graph <- book_1_tripgrams %>% 
  filter(n > 1) %>% 
  graph_from_data_frame() ggraph(book_1_tripgrams_graph, layout = "fr") + geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1,size=4)+ 
  theme_graph(base_size=10)+ ggtitle("Network of words")


  

# Tokenize the data:

Bk1_words_zip<-Bk1 %>% select(V1) %>% unnest_tokens(word,V1) 
Bk2_words_zip<-Bk2 %>% select(V1) %>% unnest_tokens(word,V1) 
Bk3_words_zip<-Bk3 %>% select(V1) %>% unnest_tokens(word,V1) 
Bk4_words_zip<-Bk4 %>% select(V1) %>% unnest_tokens(word,V1) 
Bk5_words_zip<-Bk5 %>% select(V1) %>% unnest_tokens(word,V1) 
Bk6_words_zip<-Bk6 %>% select(V1) %>% unnest_tokens(word,V1) 
Bk7_words_zip<-Bk7 %>% select(V1) %>% unnest_tokens(word,V1)
Bk8_words_zip<-Bk8 %>% select(V1) %>% unnest_tokens(word,V1) 



Bk1_words_clean<-Bk1_words %>% anti_join(stop_words)
Bk2_words_clean<-Bk2_words %>% anti_join(stop_words)
Bk3_words_clean<-Bk3_words %>% anti_join(stop_words)
Bk4_words_clean<-Bk4_words %>% anti_join(stop_words)
Bk5_words_clean<-Bk5_words %>% anti_join(stop_words)
Bk6_words_clean<-Bk6_words %>% anti_join(stop_words)
Bk7_words_clean<-Bk7_words %>% anti_join(stop_words)
Bk8_words_clean<-Bk8_words %>% anti_join(stop_words)






Bk1_words_clean %>%group_by(word) %>% 
  count(sort=TRUE) %>%head(15) %>%
  ggplot( aes(x=word,y=n)) + 
  geom_col(fill="cyan",color="blue") + 
  coord_flip()+ labs(x="Uniquewords", y="Frequency", title="Countofuniquewordsfound")+
  theme(axis.text= element_text(size=12,color="black"), axis.title= element_text(size=12,color="black"), title= element_text(size=14)) 






Bk2_words_clean %>%group_by(word) %>%
count(sort=TRUE) %>%head(15) %>%
  ggplot( aes(x=word,y=n)) + 
  geom_col(fill="cyan",color="blue") + 
  coord_flip()+ labs(x="Uniquewords", y="Frequency", title="Countofuniquewordsfound")+
  theme(axis.text= element_text(size=12,color="black"), axis.title= element_text(size=12,color="black"), title= element_text(size=14)) 


Bk3_words_clean %>%group_by(word) %>%
  count(sort=TRUE) %>%head(15) %>%
  ggplot( aes(x=word,y=n)) + 
  geom_col(fill="cyan",color="blue") + 
  coord_flip()+ labs(x="Uniquewords", y="Frequency", title="Countofuniquewordsfound")+
  theme(axis.text= element_text(size=12,color="black"), axis.title= element_text(size=12,color="black"), title= element_text(size=14)) 





Bk4_words_clean %>%group_by(word) %>%
  count(sort=TRUE) %>%head(15) %>%
  ggplot( aes(x=word,y=n)) + 
  geom_col(fill="cyan",color="blue") + 
  coord_flip()+ labs(x="Uniquewords", y="Frequency", title="Countofuniquewordsfound")+
  theme(axis.text= element_text(size=12,color="black"), axis.title= element_text(size=12,color="black"), title= element_text(size=14)) 



Bk5_words_clean %>%group_by(word) %>%
  count(sort=TRUE) %>%head(15) %>%
  ggplot( aes(x=word,y=n)) + 
  geom_col(fill="cyan",color="blue") + 
  coord_flip()+ labs(x="Uniquewords", y="Frequency", title="Countofuniquewordsfound")+
  theme(axis.text= element_text(size=12,color="black"), axis.title= element_text(size=12,color="black"), title= element_text(size=14)) 



Bk6_words_clean %>%group_by(word) %>%
  count(sort=TRUE) %>%head(15) %>%
  ggplot( aes(x=word,y=n)) + 
  geom_col(fill="cyan",color="blue") + 
  coord_flip()+ labs(x="Uniquewords", y="Frequency", title="Countofuniquewordsfound")+
  theme(axis.text= element_text(size=12,color="black"), axis.title= element_text(size=12,color="black"), title= element_text(size=14)) 

Bk7_words_clean %>%group_by(word) %>%
  count(sort=TRUE) %>%head(15) %>%
  ggplot( aes(x=word,y=n)) + 
  geom_col(fill="cyan",color="blue") + 
  coord_flip()+ labs(x="Uniquewords", y="Frequency", title="Countofuniquewordsfound")+
  theme(axis.text= element_text(size=12,color="black"), axis.title= element_text(size=12,color="black"), title= element_text(size=14)) 



Bk8_words_clean %>%group_by(word) %>%
  count(sort=TRUE) %>%head(15) %>%
  ggplot( aes(x=word,y=n)) + 
  geom_col(fill="cyan",color="blue") + 
  coord_flip()+ labs(x="Uniquewords", y="Frequency", title="Countofuniquewordsfound")+
  theme(axis.text= element_text(size=12,color="black"), axis.title= element_text(size=12,color="black"), title= element_text(size=14)) 




Bk1_words_fd_idf<- Bk1_words_clean %>% mutate (book = "Bk1")
Bk2_words_fd_idf<- Bk2_words_clean %>% mutate (book = "Bk2")
Bk3_words_fd_idf<- Bk3_words_clean %>% mutate (book = "Bk3")
Bk4_words_fd_idf<- Bk4_words_clean %>% mutate (book = "Bk4")
Bk5_words_fd_idf<- Bk5_words_clean %>% mutate (book = "Bk5")
Bk6_words_fd_idf<- Bk6_words_clean %>% mutate (book = "Bk6")
Bk7_words_fd_idf<- Bk7_words_clean %>% mutate (book = "Bk7")
Bk8_words_fd_idf<- Bk8_words_clean %>% mutate (book = "Bk8")



all_books_tf_idf<- rbind(Bk1_words_fd_idf, Bk2_words_fd_idf, Bk3_words_fd_idf, Bk4_words_fd_idf, Bk5_words_fd_idf, Bk6_words_fd_idf, Bk7_words_fd_idf, Bk8_words_fd_idf)

  



tf_idf_df<-all_books_tf_idf %>% 
  count(book,word)%>% 
  bind_tf_idf(word,book,n) %>%  
  arrange(desc(tf_idf))



tf_idf_df %>%
  group_by(book)%>% 
  top_n(10,tf_idf) %>%
  ggplot(aes(x=word,y=tf_idf,fill=book))+ 
  geom_bar(stat="identity")+facet_wrap(.~book,scales="free")+ 
  coord_flip()+theme(legend.position ="none")



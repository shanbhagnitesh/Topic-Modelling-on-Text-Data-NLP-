# read in the libraries we're going to use
library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
library(MASS)
for (i in c('SnowballC','slam','tm','RWeka','Matrix')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

library(readr)

reviews <- read_csv("Yelp_review_sample.csv", n_max=)
#access library


##############################################################################
############### Unsupervised ML ##############################################
##############################################################################

top_terms_by_topic_LDA <- function(input_text, # should be a columm from a dataframe
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
{    
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- Corpus(VectorSource(input_text)) # make a corpus object
  Corpus <- tm_map(Corpus, removePunctuation)
  Corpus <- tm_map(Corpus, removeNumbers)
  Corpus <- tm_map(Corpus, stripWhitespace)
  Corpus <- tm_map(Corpus,content_transformer(tolower))
  
  
  #remove stopwords
  Corpus <- tm_map(Corpus, removeWords,stopwords('english'))
  Corpus <- tm_map(Corpus, removeWords,c("will","get","just","like","one"
                                         ,"dont","ive","didnt","bit","wasnt","also","back","even","well"
                                         ,"best","definitely","ever","much","got","really","can","nice"
                                         ,"love","find","say","came","well","always","everything","try"
                                         ,"way","know","best","first","two","went","took","going","come"
                                         ,"sure","make","right","ordered","never","think","still","give"
                                         ,"think","said","little","ask","minutes","table","now","want"
                                         ,"told","next","day","years","around","made","asked","car"
                                         ,"order","take","need","called","lot","pretty","see","else"
                                         ,"since","many","another","pretty","wanted","tired","since","wait"
                                         ,"better","every","times","new","amazing","thing","last","thing","recommend"
                                         ,"people","gave","cant","though","work","thats","looking","bad","cant","tried","wrong"
                                         ,"side","awesome","found" ,"enough","server","something","youre","looked","visit","nothing","enough"
                                         ,"actually","done","anything","away","something","hair","youre","things","long","felt"
                                         ,"hour","long","ill","check","use","eat","used","almost","overall","put","disappointed","favorite","feel","however"
                                         ,"thought","end","later","call","top","show","getting","busy","front","left","highly","half","coming"
                                         ,"maybe","usually","without","wont"
                                         ))
  
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
    
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(20, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
  
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label 
      coord_flip() # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}


###########################Run functions#################################
## Number of Topics is chosen as 6 as the previous Algorithm suggested that there are 6 Topics

top_terms_by_topic_LDA(reviews$text, number_of_topics = 6)

#########################################################################












#########################################
##########Tf-idf Supervised ML###########
#########################################

#read data
library(readr)
reviews <- reviews <- read_csv("Yelp_review_sample.csv", n_max=)

#################################
##########preprocessing##########
#################################

#Format to UTF-8
library(base)
Encoding(reviews$text) <- "UTF-8"

#Clean data
library(tm) # general text mining functions, making document term matrixes
reviews$text <- removePunctuation(reviews$text)
reviews$text <- removeNumbers(reviews$text)
reviews$text <- stripWhitespace(reviews$text)

#Standardized to lower case
reviews$text <- tolower(reviews$text)

#remove words
reviews$text <- removeWords(reviews$text, stopwords("english")) 
reviews$text <- removeWords(reviews$text, c("centurylink","mbps","limitword","josef","lashley","texturize"
                                            ,"clep","wuz","uuu","drawback","jpg","lockaid","insectek","limitWord"
                                            ,"ino","celp","jpg"))

#Break useful funny cool to Positive and Negative
reviews$useful <- ifelse(reviews$useful == 0 , "Not Useful", "Useful")
reviews$funny <- ifelse(reviews$funny == 0 , "Not Funny", "Funny")
reviews$cool <- ifelse(reviews$cool == 0 , "Not Cool", "Cool")


###################################
##########Tf-idf Function##########
###################################

library(rlang)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyverse)
top_terms_by_topic_tfidf <- function(text_df, text_column, group_column, plot = T){
  # name for the column we're going to unnest_tokens_ to
  # (you only need to worry about enquo stuff if you're
  # writing a function using using tidyverse packages)
  group_column <- enquo(group_column)
  text_column <- enquo(text_column)
  
  # get the count of each word in each review
  words <- text_df %>%
    unnest_tokens(word, !!text_column) %>%
    count(!!group_column, word) %>% 
    ungroup()
  
  # get the number of words per text
  total_words <- words %>% 
    group_by(!!group_column) %>% 
    summarize(total = sum(n))
  
  # combine the two dataframes we just made
  words <- left_join(words, total_words)
  
  # get the tf_idf & order the words by degree of relevence
  tf_idf <- words %>%
    bind_tf_idf(word, !!group_column, n) %>%
    select(-total) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))
  
  if(plot == T){
    # convert "group" into a quote of a name
    # (this is due to funkiness with calling ggplot2
    # in functions)
    group_name <- quo_name(group_column)
    
    # plot the 10 most informative terms per topic
    tf_idf %>% 
      group_by(!!group_column) %>% 
      top_n(10) %>% 
      ungroup %>%
      ggplot(aes(word, tf_idf, fill = as.factor(group_name))) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(reformulate(group_name), scales = "free") +
      coord_flip()
  }else{
    # return the entire tf_idf dataframe
    return(tf_idf)
  }
}


########################################
##########Run tf-idf functions##########
########################################

# Most informative words for stars
top_terms_by_topic_tfidf(text_df=reviews, # dataframe
                         text_column=text, # column with text
                         group_column=stars, # column with topic label
                         plot = T) # return a plot

# Most informative words for useful and not useful
top_terms_by_topic_tfidf(text_df = reviews, # dataframe
                         text_column = text, # column with text
                         group_column = useful, # column with topic label
                         plot = T) # return a plot

# Most informative words for funny and not funny
top_terms_by_topic_tfidf(text_df = reviews, # dataframe
                         text_column = text, # column with text
                         group_column = funny, # column with topic label
                         plot = T) # return a plot

# Most informative words for cool and not cool
top_terms_by_topic_tfidf(text_df = reviews, # dataframe
                         text_column = text, # column with text
                         group_column = cool, # column with topic label
                         plot = T) # return a plot






##############################################################################
##############################################################################
##############################################################################
##############################################################################
# Dictionary based Classification of Reviews for each categories ############# 
##############################################################################
##############################################################################

dictionary <- read_csv("Dictionary.csv", n_max=)


library(stringi)

f1 <- reviews$text
f2 <- dictionary$Food
reviews$Food <- ifelse(stri_count_regex(f1, paste(f2, collapse = '|')) >= 1,1,0)



s1 <- reviews$text
s2 <- dictionary$Service
reviews$Service <- ifelse(stri_count_regex(s1, paste(s2, collapse = '|')) >= 1,1,0)


s1 <- reviews$text
s2 <- dictionary$Ambience
reviews$Ambience <- ifelse(stri_count_regex(s1, paste(s2, collapse = '|')) >= 1,1,0)

s1 <- reviews$text
s2 <- dictionary$Worthiness
reviews$Worthiness <- ifelse(stri_count_regex(s1, paste(s2, collapse = '|')) >= 1,1,0)

s1 <- reviews$text
s2 <- dictionary$Drinks
reviews$Drinks <- ifelse(stri_count_regex(s1, paste(s2, collapse = '|')) >= 1,1,0)


s1 <- reviews$text
s2 <- dictionary$Positive
reviews$Positive <- ifelse(stri_count_regex(s1, paste(s2, collapse = '|')) >= 1,1,0)



Food <- sum(reviews$Food)
Service <- sum(reviews$Service)
Ambience <- sum(reviews$Ambience)
Worthiness <- sum(reviews$Worthiness)
Drinks <- sum(reviews$Drinks)
Positive <- sum(reviews$Positive)

vector <- c(Food,Service,Ambience,Worthiness,Drinks,Positive)
names(vector) <- c("Food","Service","Ambience","Worthiness","Drinks","Positive")
bar <- barplot(vector,
               names.arg = names(vector),
               xlab = "Topics", 
               ylab = "Number of Reviews Per Topic", 
               col = c("#E69F00", "#56B4E9", "#009E73","#E69F00", "#56B4E9", "#009E73","#E69F00", "#56B4E9"))

text(x = bar, y = vector, label = vector ,pos = 1, cex =1 , col = "black")



##### This Code is an Unsupervised ML way of Finding the number of Topics present in a Text Data. 
### Please note : This takes 2 hours to Run the entire Code ###


install.packages("ldatuning")
library("ldatuning")
library("tm")
library("readr")
reviews <- read_csv("Yelp_review_sample.csv", n_max=)

Corpus <- Corpus(VectorSource(reviews)) # make a corpus object
Corpus <- tm_map(Corpus, removePunctuation)
Corpus <- tm_map(Corpus, removeNumbers)
Corpus <- tm_map(Corpus, stripWhitespace)
Corpus <- tm_map(Corpus,content_transformer(tolower))


#remove stopwords
Corpus <- tm_map(Corpus, removeWords,stopwords('english'))
Corpus <- tm_map(Corpus, removeWords,c("will","great","get","just","like","one","good"
                                       ,"dont","ive","didnt","bit","wasnt",""))

dtm <- DocumentTermMatrix(Corpus) # get the count of words/document

# remove any empty rows in our document term matrix (if there are any 
# we'll get an error when we try to run our LDA)
unique_indexes <- unique(dtm$i) # get the index of each unique value
dtm <- dtm[unique_indexes,] # get a subset of only those indexes



result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c( "Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 20L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)


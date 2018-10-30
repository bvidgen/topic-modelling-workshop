
### Topic modelling workshop 1 - Script 1 ###
  # First go at implementing a topic model


### Load packages
  # If you don't have the packages installed, do so using the install.packages() command
install.packages('tm')
install.packages('topicmodels')

library(tm) # really useful for NLP in R
library(topicmodels) # the good stuff for topic modelling
options(scipen = 999) # so that we don't get annoying scientific notation


### Load data
setwd('~/Dropbox/topic modelling workshop/topic modelling - final materials') # change the filepath to wherever you have the workshop data downloaded
load('DATA - workshop 1 practical 1.RData')

View(df.petitions) # have a look at the dataframe
df.petitions$text[1:3] # some example instances of the text we want to model


### Prepare data
# Make a 'Corpus' object from the text variable
  # a 'Corpus' object is very lightweight and easy for R to interact with; it just re-formats the data 
petitions.text = tm::Corpus(tm::VectorSource(df.petitions$text))

# Remove any 'odd' characters (often, characters with diacritic marks)
petitions.text = tm::tm_map(petitions.text, function(x) iconv(enc2utf8(x), sub = "byte"))  # Solution taken from Stack Overflow: https://stackoverflow.com/questions/9637278/r-tm-package-invalid-input-in-utf8towcs
  # the 'tm' package needs the text to be in utf-8 format. With Windows installations the default seems to be to have the text in ASCII (with Macs you don't actually need to do this step)
  # there is also an interesting discussion about encodings for tm package here - https://stackoverflow.com/questions/43456177/utf-8-character-encoding-with-termdocumentmatrix


# We can look at the contents of a Corpus
df.petitions$text[[10]] # the 10th item in the df.petitions$text
as.character(petitions.text[[10]]) # the 10th item in the petitions.text object

# Create a Document-Term Matrix (DTM) from the text 
dtm.petitions = tm::DocumentTermMatrix(petitions.text)
  # Each row is a document (in our case, a petition) and each column is a unique word.
as.matrix(dtm.petitions)[1:10, 1:10] # if you want to look at the DTM you need to wrap it in as.matrix()
  # We are just showing the first 10 columns and 10 rows by using [1:10, 1:10]

# How many documents and words are there in the DTM?
dim(dtm.petitions)
  # 10,950 rows - each row is a separate petition (same number as in our original df.petitions dataframe)
  # 33,115 columns - each column is a unique word
    # Each column contains a count of the number of words in that petition


### Implement topic model
k = 10 # number of topics - 10 topics is often a good starting point

### *DON'T* run this now - it will take a little while to implement (20 minutes +)
  # in the .RData for this workshop we have a saved version of the topicmodel with these params

    ldaOut = topicmodels::LDA(dtm.petitions, # the DTM that we want to analyse
                              k, # the number of topics (very important parameter!)
                              method='Gibbs', # the sampling method we use - Gibbs sampling is very widely used
                              control=list(
                                alpha = 0.1, # documents/topics distribution
                                delta = 1, # topics/words distribution
                                nstart = 5, # the number of repeated random starts, if best=T then only the best fitting one is kept
                                seed = list(1,2,3,4,5), # keep a record of the set seeds for random starts to ensure replicability
                                best = TRUE, # keep only the best fitted random start
                                burnin = 4000, # number of omitted Gibbs iterations at the start of the sampling
                                iter = 2000,  # how many iterations for the Gibbs sampler
                                thin = 500)) # how many iterations ommitted in-between Gibbs samples


### Inspect the topic model output
# 1. Get the top terms in each topic
ldaOut.topics = as.data.frame(terms(ldaOut, 8)) # more terms gives us more information - but the top terms are the most important and we don't want to get distracted by too many lower ranked ones

# We can attach names to the topics like this:
colnames(ldaOut.topics) = c('topic 1', 'topic 2', 'topic 3', 'topic 4', 'topic 5',
                            'topic 6', 'topic 7', 'topic 8', 'topic 9', 'topic 10')

ldaOut.topics


# 2. Get the distribution over topics for each document
  # For each document, we get the distribution of topics as a vector
topicdf = as.data.frame(posterior(ldaOut)$topics)
colnames(topicdf) = colnames(ldaOut.topics) # rename the topics to match our labels
head(topicdf, 5) # inspect the first five petitions
  # each row is a petition
  # the columns are the ten topics
  # each row is a multinomial distribution and sums to 1

# We can look at the topic distribution for a single document, e.g. number 20:
  topicdf[20,] # 99% topic 9
# And then also look at the text of the petition:
  as.character(petitions.text[[20]]) 


### End of workshop 1 practical 1 ###


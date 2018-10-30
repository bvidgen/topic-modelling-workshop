
### Topic modelling workshop 1 - Script 2 ###
  # 1. Cleaning text data
  # 2. Transforming data into a Document Term Matrix for further analysis
  # 3. Creating a topic model
  # 4. Extracting useful information, like the topic probabilities per document and the word probabilities per topic
  # 5. Initial analyses, like the prevalence of topics


### Load packages
  # If you don't have the packages installed, do so using the install.packages() command
install.packages('tm')
install.packages('topicmodels')
install.packages('tidyr')
install.packages('tidytext')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('cowplot')
install.packages('devtools')
#install.packages('plyr')

library(tm) # really useful for NLP in R
library(topicmodels) # the good stuff for topic modelling
library(tidyr) # data manipulation
library(tidytext) # more data manipulation (this time specifically for text)
library(dplyr) # yet more data manipulation
library(ggplot2) # for making nice looking plots
library(cowplot) # makes ggplots look nice without having to do lots of work
library(devtools) # so we can download the package we made for this class
#library(plyr)
devtools::install_github("bvidgen/rBDA", force = F); library("rBDA") # some easy-to-use functions we made for fitting topic models
options(scipen = 999) # so that we don't get annoying scientific notation


### Load data
setwd('~/Dropbox/topic modelling workshop/topic modelling - final materials') # change the filepath to wherever you have the workshop data downloaded
load('DATA - workshop 1 practical 2.RData')

### Prepare data
# Make a 'Corpus' object from the text variable
# a 'Corpus' object is very lightweight and easy for R to interact with; it just re-formats the data 
petitions.text = tm::Corpus(tm::VectorSource(df.petitions$text))

### WINDOWS PEOPLE ###
  # Remove any 'odd' characters (often, characters with diacritic marks)
  petitions.text = tm::tm_map(petitions.text, function(x) iconv(enc2utf8(x), sub = "byte"))  # Solution taken from Stack Overflow: https://stackoverflow.com/questions/9637278/r-tm-package-invalid-input-in-utf8towcs
  # the 'tm' package needs the text to be in utf-8 format. With Windows installations the default seems to be to have the text in ASCII (with Macs you don't need to do this step)
  # there is also an interesting discussion about encodings for tm package here - https://stackoverflow.com/questions/43456177/utf-8-character-encoding-with-termdocumentmatrix


## Inspect data
# How many unique terms are there in the entire corpus?
dim(DocumentTermMatrix(petitions.text))
  # 10,950 rows = 10,950 documents
  # 33,115 columns = 33,115 unique terms

# How many words does each petition contain on average?
mean(rowSums(as.matrix(DocumentTermMatrix(petitions.text)))) # ~81 words

# Look at one of the entries
as.character(petitions.text[[50]])


## Clean the data
# These cleaning operations are pretty much always a good idea
petitions.text = petitions.text %>%
  tm_map(., content_transformer(tolower)) %>% # make all words lower case
  tm_map(., removeNumbers) %>% # remove all numbers
  tm_map(., stripWhitespace) # remove whitespace (this leaves singles spaces between words but removes duplicates, e.g. double spaces)

as.character(petitions.text[[50]]) # text looks a bit better but is still quite messy

# We can do more advanced text cleaning
# These operations reduce the overall amount of content; removing some terms whilst retaining all of the informative content
petitions.text = petitions.text %>%
  tm_map(., removeWords, stopwords('en')) %>% # remove stop words
  # tm_map(., stemDocument) # in-built function to stem text using the Porter stemming algorithm
  tm_map(., rBDA::stemText) # less-aggresive stemmer we created in the rBDA package - play with both and use whichever you think is most appropriate

as.character(petitions.text[[50]]) # text is now hard for humans to read - but the basic meaning is still there

# Tidy up the text - remove the punctuation, single words and strip whitespace
petitions.text = petitions.text %>%
  tm_map(., rBDA::replacePunctuation) %>% # Replace punctuation with spaces
  tm_map(., rBDA::replaceSingleWords) %>% # Replace any single floating letters with a space (some letters get freed up by the replacePunctuation command)
  tm_map(., stripWhitespace) # remove white space (white space gets created by the prior two functions)

as.character(petitions.text[[50]]) # text is now just mostly keywords

# Sometimes we have to run cleaning functions several times to get the text just right - look at the first word, 'undersigned'
petitions.text = petitions.text %>%
  tm_map(., rBDA::stemText) # after the additional cleaning, we now want to stem once more 

as.character(petitions.text[[50]]) # final bit of stemming has tidied up the last few words


## Manual cleaning
# We can manually remove words as well - BUT this is not required, and you should do it only if you think its needed (typically, from running a few topic models)
  # I normally split up the manual cleaning words into:
    # nonwords - nonsensical words that get created through cleaning or poor data entry. These are hard to identify unless you spend alot of time looking at your data
    # customstopwords - words that with certain sorts of data (e.g. academic papers or websites) consistently get created, and don't really impart much information
    # domainwords - words relevant to your area of study that appear in nearlly all documents and so aren't very useful for characterising them. E.g. if you are studying blockchain then it is likely that everything you analyse will contain the words 'blockchain' and 'bitcoin' and 'distributed'
nonwords = c('ccaaca', 'ccb', 'ccbc', 'ccf','ach', 'etc', 'eg', 'vrri', 'wtc', 'xxx','tey', 'ude', 'rnrnthe')
customstopwords = c('www', 'nhttp', 'http', 'com', 'org', 'co', 'nhttps', 'https', 'html', 'pdf', 'nwww') 
domainwords = c('like', 'will', 'come')

# Remove these words from the corpus
petitions.text = petitions.text %>%
  tm_map(., removeWords, customstopwords) %>%
  tm_map(.,removeWords, nonwords) %>%
  tm_map(.,removeWords, domainwords) %>%
  tm_map(., stripWhitespace) # basically always a good idea when manipulating text

as.character(petitions.text[[50]]) # text is pretty similar as last time, with a few terms removed

# Tidy up the workspace
rm(domainwords, customstopwords, nonwords)



### Create a Document Term Matrix (DTM)
dtm.petitions = tm::DocumentTermMatrix(petitions.text)

## Convert the rownames into the IDs of each petition. This will help us to identify each petition in our later analysis.
  # It's a particularly good idea to do this now, as some of our later analyses may delete some documents / move them around
head(rownames(dtm.petitions), 10)
rownames(dtm.petitions) = df.petitions$id_general 
head(rownames(dtm.petitions), 10)

# Now how many words does each petition contain on average?
mean(rowSums(as.matrix(dtm.petitions))) # 54 words on average (down from ~80 words previously)

## Remove sparse terms
  # These are terms which occur infrequently - they are hard to build a general model of the text from as they occur infrequently. This is the problem of 'overfitting'
  # We retain only terms which are 'more sparse' than the sparsity threshold we set - terms which are removed appear in a percentage of documents less than 1 - sparsity threshold
  # So if we set sparsity to 0.999 it means that we only retain terms which appear in at least 0.001% of documents (here, at least 31.7 petitions)
dtm.sparse = tm::removeSparseTerms(dtm.petitions, 0.999) # adjust to 0.9, 0.99, 0.9999 ...

# How many unique words are there in the cleaned sparse corpus now?
dim(dtm.sparse) # 5,050 words

# Now how many words does each petition contain on average?
mean(rowSums(as.matrix(dtm.sparse))) # ~49 words on average
  # Interestingly, whilst we have lost lots of terms overall (80% of the unique terms are gone), the average length of each petition has changed by far less

# We now have a cleaned Document Term Matrix which we can use for topic modelling
  # Deciding whether to use dtm.petitions or dtm.sparse is down to you and the results of your modelling. There is no 'right answer'.
  # That said, I've found that you are best off removing some of the sparse terms - if you're concerned, try setting sparsity to just 0.9999


## Look at the top terms
# Before we implement topic model let's just have a quick look at what are the most prevalent terms in the entire corpus

# get the word frequencies
term_freq = data.frame(term=names(colSums(as.matrix(dtm.sparse))),
                       frequency = colSums(as.matrix(dtm.sparse))) %>%
  dplyr::arrange(desc(frequency))

# Plot the word frequencies as a bar chart
term_freq %>%
  head(term_freq, n = 20) %>% # take just the top 20 terms
  dplyr::mutate(term = factor(term, levels = term[order(desc(frequency))])) %>% # order the variable so that it is plotted in descending order
  ggplot2::ggplot(aes(term,frequency)) + 
  ylab('Frequency') +
  ggtitle('Top 20 terms') +
  geom_bar(stat='identity', fill = 'light blue') +
  theme(axis.text.x=element_text(angle=75, hjust=1),  # angle the text so that it can be read
        axis.title.x = element_blank()) # Don't show the x axis label



### Implement topic model
k = 10 # number of topics 

### This will take a little while to run (20 minutes +)
  # So *DON'T* run it - in the .RData file for this workshop we have a saved version of the topicmodel with these params.

ldaOut.clean = topicmodels::LDA(dtm.sparse, # the DTM that we want to analyse
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
## 1. Get the top terms in each topic
ldaOut.topics = as.data.frame(terms(ldaOut.clean,8)) # more terms gives us more information - but the top terms are the most important and we don't want to get distracted by too many lower ranked ones
ldaOut.topics

# Label the topics
# This is one of the toughest parts of topic modelling and where you need to have some understanding of the subject area
# Here are some suggested labels for the topics - do you agree with them?
colnames(ldaOut.topics) = c('School', 'Media_Culture', 'Britain', 'Law_Order', 'Local_Government',
                            'Referendum', 'Animals', 'NHS', 'Work_Pay', 'Road_Driving')
ldaOut.topics
    # These topic labels are not perfect but they make sense (at least to me)
    # Ideally, we want terms to be mutually exclusive, i.e. they relate to distinct themes
    # We also want terms to be well defined, i.e. they refer to a specific topic and are not too general in nature
    # Typically, in the first attempt at implementing a topic model, 30% of topics can be too messy to label properly
    # We could also note that the term 'can' occurs in 3 topics - and 'make' appears 2 times
    # So we could now go back to our text cleaning and remove them from the corpus, doing this iteratively looking at the top terms to improve the quality of the model


## 2. Get the entire topic distributions over words
topics.words.dist = tidytext::tidy(ldaOut.clean, matrix='beta')
head(topics.words.dist, 10) # for each word in the vocabulary we get the probability of its occurrence within each topic - but the data is in long format
topics.words.dist = tidyr::spread(topics.words.dist, term, beta) # make the data 'short'
topics.words.dist = data.frame(t(topics.words.dist)) # flip the rows and columns round
topics.words.dist[1:10, 1:10] # each row is now a word, and each column is a topic

# inspect the topic distribution over words
colSums(topics.words.dist) # each column sums to 1 - this makes sense, as each topic is a multinomial distribution over the entire vocabulary
hist(log10(rowSums(topics.words.dist))) # long tailed disribution of each words' probability of occuring - basically, most words have a very low probability of being in *any* topic


## 3. Get the topic distribution for each petition
  # This is one of the most interesting and useful outputs that we will get
topicdf = as.data.frame(posterior(ldaOut.clean)$topics)
colnames(topicdf) = colnames(ldaOut.topics) # rename the topic with the labels we came up with above
head(topicdf, 5)
  # each row is a petition
  # columns are the ten topics
  # each row sums to one - as each petition is a combination of the ten topics


## 4. For a particular topic, get some petitions which are highly loaded on it just to check the output/illustrate the model
  # We will look at the topic 'Referendum'
top.ref = head(order(topicdf$Referendum, decreasing=T), 10) # row indexes of the top loaded petitions on topic 'Referendum'
topicdf$Referendum[top.ref] # probabilities of the petitions for the topic

df.petitions$text[top.ref][6] # Original text of the top loaded petitions 
  # Look at the top ten petitions - do you think they all are about the referendum?
  # Often you are in a better position to name the topics once you have inspected some of the heavily loaded documents


## 5. See which topics are most prevalent
topic.prevalence = data.frame(colSums(topicdf))
colnames(topic.prevalence) = 'prevalence'
topic.prevalence$topic = row.names(topic.prevalence); row.names(topic.prevalence) = 1:10
head(topic.prevalence, 10) # We can see straight awya that topics are not uniformly distributed across the petitions

ggplot2::ggplot(topic.prevalence, aes(topic, prevalence)) +
  geom_bar(stat = 'identity') +
  xlab('Topics') +
  ylab('Prevalence') +
  theme(axis.text.x = element_text(hjust=1, angle=75, family = "Trebuchet MS", size=10)) # angles the x axis text


### Summary of the objects we have created
  ldaOut.clean # the LDA model
  ldaOut.topics # top 8 terms in the 10 topics
  topics.words.dist # distribution of each topic over the entire vocabulary
  topicdf # document distribution over topics for each of the 10,950 petitions
  topic.prevalence # overall prevalence in the entire corpus for each of the 10 topics
  

### End of workshop 1 practical 2 ###


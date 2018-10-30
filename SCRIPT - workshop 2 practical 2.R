
### Topic modelling workshop 2 - Script 2 ###
  # 1. Topic proportions in the entire corpus
  # 2. How does topic prevalence fluctuate over time? Are there discernible patterns?
  # 3. What are the relationships between topics?
      # plot the cosine value matrix
      # plot the cosine value network
  # 4. Look at how the topic prevalences differ between two groups


### Load packages
  # If you don't have the packages installed, do so using the install.packages() command
install.packages('tidyr')
install.packages('tidytext')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('cowplot')
install.packages('lsa')
install.packages('igraph')
install.packages('corrplot')

library(tidyr) # data manipulation
library(tidytext) # more data manipulation (this time specifically for text)
library(dplyr) # yet more data manipulation
library(ggplot2) # for making nice looking plots
library(cowplot) # makes ggplots look nice without having to do lots of work
library(lsa) # for calculating cosine values
library(igraph) # for making network graphs
library(corrplot) # for making matrix plots of cosine values

options(scipen = 999) # so that we don't get annoying scientific notation


### Load data
setwd('~/Dropbox/topic modelling workshop/topic modelling - final materials') # change the filepath to wherever you have the workshop data downloaded
load('DATA - workshop 2 practical 2.RData') 


### Prepare data
topicdf = as.data.frame(posterior(ldaOut.clean)$topics)
colnames(topicdf) = c('School', 'Media_Culture', 'Britain', 'Law_Order', 'Local_Government',
                            'Referendum', 'Animals', 'NHS', 'Work_Pay', 'Road_Driving') # the same topic labels as yesterday

### Analyze data

### 1. topic prevalence fluctuations over time
topics_time = topicdf # make a new df for topics/time
topics_time$time = df.petitions$open.Date # add the date to the dataframe
head(topics_time, 6) # inspect the data
  # each row is a document (a petition)
  # each of the first ten columns is a topic
  # the final column is the date at which the petition was created, which we have called 'time'

topics_time = topics_time %>% # make data 'long'
  tidyr::gather(key = topic, # gather is a very useful function in R for moving between long and wide data formats
                value = probability, -time)
?gather
  # we need to pick a 'key' name and a 'value' name for the new columns
  # we also use '-time' as we don't want time to be included in the 'gather' command, we want it to be kept as a separate column
head(topics_time, 6)
  # Now - each row is each of the 10 topics for each petition
  # So we went from 10,950 rows to 109,500 (10,950 * 10)
  # Then we have the probability for each topic for each petition as a separate row - this is 'long' data

# BUT! we haven't combined by date/topic, so we have multiple entries for the same day and topic (but from different petitions) listed separately
  # We want to combine these so that for each date/topic we have just one entry
  # That way we can see for each day the total probability for each topic across multiple pettions
topics_time = topics_time %>%
  dplyr::group_by(time, topic) %>% # group into each day/topic combo
  dplyr::summarise(tot = sum(probability)) # take the sum of the topic probabilities for each day/topic combo
  # We can now plot the number of documents in each topic over time

head(topics_time, 6) # for each day we have the topics and the cumulative probability (i.e. number) of petitions attached to it 

# Plot the signatures per topic over time
ggplot2::ggplot(topics_time, aes(time, tot, group = topic, color = topic)) + 
  geom_line() +
  ggtitle('Emergence of Topics ') +
  #xlab('Time') +
  ylab('Numbber of documents') +
  theme(axis.title.x = element_blank(), # remove x axis label
        axis.text.x = element_text(hjust=1, angle=45), # angle x axis text
        legend.position = 'none') # remove the legend from the plot
  

# Fix the colours
  # You can fix the colors of the topics, which will allow you to use the same colors on all your plots - but this is quite complicated to do in R
  # Based on discussion from Stack Overflow https://stackoverflow.com/questions/5171263/changing-line-colors-with-ggplot
#palette(rainbow(10))
group.colors = c(Animals = "red", # for each topic label get a color (colors have been chosen arbitrarily)
                Britain = "orange", 
                Law_Order = "#CCFF00", 
                Local_Government = "#228B22", 
                Media_Culture = "#00FF66", 
                NHS = "cyan", 
                Referendum = "blue", 
                Road_Driving = "grey", 
                School = "#CC00FF", 
                Work_Pay = "#FF0099")
group.colors = as.data.frame(group.colors) # convert to dataframe
group.colors$topic = as.character(rownames(group.colors)) # make a 'topic' variable; the row names are the topic labels
colnames(group.colors) = c('colors', 'topic') # reset the names of the columns
group.colors$colors = as.character(group.colors$colors) # make sure the color column is of type character

# Populate topics_time with the colors we have assigned to each topic
topics_time = base::merge(topics_time, group.colors, by = 'topic')

# Now replot the graph using the colors we have fixed
ggplot2::ggplot(topics_time, aes(time, tot, group = topic, color = topic)) + 
  geom_line() +
  ggtitle('Emergence of Topics ') +
  ylab('Numbber of documents') +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(hjust=1, angle=45),
        legend.position = 'none') +
  scale_color_manual(values = group.colors$colors) # we can now stipulate the colors manually



### 2. Topic proportions - this time with the right colors
topic.prevalence = data.frame(colSums(topicdf)) # dataframe of the total prevalence of each topic
colnames(topic.prevalence) = 'prevalence' # rename the columns
topic.prevalence$topic = row.names(topic.prevalence); row.names(topic.prevalence) = 1:10 # attach the topic names
topic.prevalence = base::merge(topic.prevalence, group.colors, by = 'topic') # merge the colours into the dataset
head(topic.prevalence, 10) # inspect the dataframe

ggplot2::ggplot(topic.prevalence, aes(topic, prevalence)) +
  geom_bar(stat = 'identity', fill = topic.prevalence$colors) + # use the colors for the bars which we merged into topic.prevalence
  xlab('Topics') +
  ylab('Number of documents') +
  theme(axis.title.x = element_blank(),
    axis.text.x = element_text(hjust=1, angle=75))



### 3. topic connections
## 3.1 topics connected based on co-occurence in documents
# 3.1.1 cosine matrix plot
# Prepare the data
topicdf_m = as.matrix(topicdf) # convert to a matrix (needed for the cosine measurement)
cos_result = lsa::cosine(topicdf_m) # cosine similarity calculated over the columns of a matrix
cos_result = data.matrix(cos_result)

# Plot the cosine values as a heat map matrix
corrplot::corrplot(cos_result, # plot of cosine similarity based on topic occurence in petitions
         cl.lim = c(0, 1), # only show positive values - in this case, we can't have negative cosine values 
         #method = 'number', number.cex = 0.5,
         method = 'color', # different methods visualize the connections differently
         type = 'lower',
         tl.col = 'black', # the default color is red for some odd reason
         diag = F) # don't show diagonal values (always equal to 1)

?corrplot # view ways to adjust the output


# 3.1.2 cosine network plot
# Make a network graph from the cosine values
net = graph_from_adjacency_matrix(cos_result, mode = 'undirected', weighted = T)

# remove loops and multiple values
net = simplify(net, remove.loops=T, remove.multiple = T)

# Check the igraph object
length(E(net)) # 45 - once we remove the self-loops there are now 45 connections 
E(net) # inspect the edges
components(net)$membership # all nodes (i.e. topics) are in the same main component (which makes sense - every topic has some, even very weak, connection to every other topic)

# make a graph
plot.igraph(net) # BUT! That's not a wildly informative graph...

# get the names of the vertexes in the igraph object
  # It is *very* important that we have these in the right order for our later analyses
vertex.names = names(V(net)); vertex.names
vertex.order = match(vertex.names, topic.prevalence$topic); vertex.order # this bit of code tells us how to reorder the topic.prevalence$topic object to be in the same order as vertex.names

# So, for instance, topic.prevalence$topic is currently ordered like this:
topic.prevalence$topic # NOT the same as the vertex.names
# But we can do this to make it in the same order as vertex.names:
topic.prevalence$topic[vertex.order]
vertex.names
  # Hope that isn't too confusing - but this is important so that we can easily adjust the valuse in our igraph 'nets' object to plot a nicer graph

# set the size of the nodes
node_size = topic.prevalence$prevalence[vertex.order]

# set the colours of the nodes
V(net)$color = topic.prevalence$colors[vertex.order]

# Reset the weight of the edges
  # NOTE - we already have the edges weighted from out initial input dataframe; but the weights are so small they don't show up on the plot - we need to scale them up
E(net)$weight = E(net)$weight*50; E(net)$weight # I arrived at a value of 50 through trial and error - it is an arbitrary scaling weight

# make the label text black rather than the default blue
V(net)$label.color = 'black'

# plot a better network
plot.igraph(net, 
            edge.width = (E(net)$weight),
            vertex.size=node_size / 50) # through trial and error, dividing by about 50 seems to show the different node sizes nicely (at least on my computer). It is an arbitrary scaling weight


# Finally, we could also remove some of the weaker connections to make the important connections 'pop' more
cos_result_r = cos_result # make a new dataframe of the cosine values
cos_result_r[which(cos_result_r < quantile(sort(unlist(as.list(cos_result_r))), probs=0.7))] = 0 # anythingoutside the top 30th percentile set to 0

net_r = graph_from_adjacency_matrix(cos_result_r, mode = 'undirected', weighted = T)
net_r = simplify(net_r, remove.loops=T, remove.multiple = T)
vertex.names = names(V(net_r)); vertex.names
vertex.order = match(vertex.names, topic.prevalence$topic); vertex.order
node_size = topic.prevalence$prevalence[vertex.order]
V(net_r)$color = topic.prevalence$colors[vertex.order]
E(net_r)$weight = E(net_r)$weight*50; E(net_r)$weight 
V(net_r)$label.color = 'black'

plot.igraph(net_r, 
            edge.width = (E(net_r)$weight),
            vertex.size=node_size / 50)
# each time we plot the graph the algorithm will slightly change the positions of the nodes



## 3.2 topic connections based on similarity of topics' distributions over words
# 3.2.1 cosine matrix plot
# Prepare the data
topics_words = tidytext::tidy(ldaOut.clean, matrix='beta') # for each word and each topic get the probability
topics_words = tidyr::spread(topics_words, term, beta) # change the data format
topics_words = t(topics_words) # transpose so each column is a topic and each row a word
colnames(topics_words) = colnames(cos_result)
head(topics_words, 6)

cos_result_words = lsa::cosine(topics_words) # cosine similarity
cos_result_words = data.matrix(cos_result_words)

# Plot the cosine values as a heat map matrix
corrplot::corrplot(cos_result_words, # plot of cosine similarity based on topic occurence in petitions
                   cl.lim = c(0, 1), # only show positive values - in this case, we can't have negative cosine values 
                   method = 'color',
                   #method = 'number', number.cex = 0.5,
                   type = 'lower',
                   tl.col = 'black', # the default color is red for some reason
                   diag = F) # don't show diagonal values (always equal to 1)


# 3.2.2 cosine graph plot
# You could also recreate the network plot we made above for the topic prevalences in documents for the topic distributions over words - the code can easily be replicated



### 4. Look at topic prevalence for two separate groups
# We can split the dataset into those petitions which received a government response (i.e. got more than 10,000 signatures) and those which did not
topicdf.new = topicdf # make a duplicate of the topicdf dataframe
topicdf.new$id = rownames(topicdf.new) # take the rownames (which are the IDs) and attach as a separate column

# Petitions with a response
df.response = subset(df.petitions, !(is.na(threshold.Date)))$id_general # get the IDs of those entries which have a threshold Date
df.response = subset(topicdf.new, id %in% df.response) # get the petitions' topic distributions for those entries
df.response$id = NULL # we no longer need the ID values so delete the variable
result.response = colSums(df.response) / nrow(df.response) # normalised prevalence of each topic for petitions which received a gov't response

# Petitions without a response
df.none = subset(df.petitions, is.na(threshold.Date))$id_general # get the IDs of those entries which do not have a threshold Date
df.none = subset(topicdf.new, id %in% df.none) # get the petitions' topic distributions for those entries
df.none$id = NULL # we no longer need the ID values so delete the variable
result.none = colSums(df.none) / nrow(df.none) # normalised prevalence of each topic for petitions which received a gov't response

# Plot topic distributions for both groups on a single bar chart
df.short = cbind.data.frame(result.response, result.none) # combine the two vectors of topic prevalences we have calculated
colnames(df.short) = c('govt_threshold', 'no_response') # rename the columns
df.short$topic = rownames(df.short) # rownames are topic labels - attach them as a separate variable
df.long = df.short %>% # Currently our data is 'wide'. As earlier, we want it in 'long' format
  tidyr::gather(key = Legend,
                value = probability, -topic)

ggplot2::ggplot(df.long, aes(topic, probability, fill = Legend)) +
  ggtitle('Topic prevalence by group') +
  ylab('Probability') + 
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7) + # don't include 'dodge' to get a stacked bar chart
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(hjust=1, angle=45))

# tidy up work space - delete the objects we don't need to make the final plot
rm(result.none, df.none, result.response, df.response)


### End of workshop 2 ###





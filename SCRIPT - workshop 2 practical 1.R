
### Topic modelling workshop 2 - Script 1 ###
  # 1. Fit a much better topic model
    # Fit number of topics
    # Fit value of alpha
    # Fit value of beta


### Load packages
  # If you don't have the packages installed, do so using the install.packages() command
library(topicmodels)
library(tm)
install.packages('devtools')
library(devtools) # so we can download the 'BDA' package we made for this class
devtools::install_github("bvidgen/rBDA", force = F); library("rBDA") # some easy-to-use functions we made for fitting topic models
options(scipen = 999) # so that we don't get annoying scientific notation



### Load data
setwd('~/Dropbox/topic modelling workshop/topic modelling - final materials') # change the filepath to wherever you have the workshop data downloaded
load('DATA - workshop 2 practical 1.RData')

### Prepare data
dtm.test = dtm.sparse[1:10,] # we are using dtm.test, which contains just the first 10 petitions - so that, for this workshop, our script runs fast
  # For your own models you HAVE to test all your params on your actual data
  # This can take a seriously long time; you may even need to leave your script to run overnight

  # NOTE: If you are having any issue with this data , then please just load the data from the second practical in workshop 1

folds = 5 # how many 'folds' we want - 5 or 10 are typical values; more folds will take longer to run
  # for very very small dataframes (like dtm.test) a large number of folds won't work



### Model fitting
## 1. Fit topics (K) using perplexity
k.values = 2:10 # the values of k that we test for. It needs to be a vector of numbers

topics.perplexity = rBDA::fit.topics.perplexity(dtm = dtm.test, 
                                          fold = folds, 
                                          k.values = k.values)

## Visualise perplexity vs number of topics
rBDA::plot_perplexity(data.frame = topics.perplexity, # provide the dataframe with the calculated perplexity values straight from topics.perplexity()
                    value.write = 'K') # we need to stipulate what we are plotting against perplexity

# We can also annotate a line where we think the optimum number of topics falls
rBDA::plot_perplexity(data.frame = topics.perplexity, 
                    value.write = 'K',
                    xintercept.write = 9)

# Interpreting the Perplexity vs. K graph
  # The dots represent the output of testing for each fold - they giveus an idea of the spread. A large spread means the results are inconsistent, which is likely indicative of a dataset which is too small
  # The line is the average of each round of k-folds testing. This is what we want to interpret
  # Interestingly, when K increases we will nearly always see a reduction in Perplexity - but the rate of reduction will slow as each increase in K has a smaller effect
  # So, we have to decide how large the value of K should be, balancing the improved fit against the risk of overfitting
  # We use the (not) entirely scientific 'elbow' method - look for the point where the reduction in perplexity slows (you will have to use your judgement on this; rarely can an unambiguous cutoff be identified)
  # See here for more on the elbow method: https://en.wikipedia.org/wiki/Elbow_method_(clustering)
  # And here for a discussion on the link between the elbow and the second derivative: https://stackoverflow.com/questions/4471993/compute-the-elbow-for-a-curve-automatically-and-mathematically

  # Having fit and plotted the model, you may now decide to go back and re-test for new values of k - perhaps you haven't taken a big enough range, or you've gone for a big enough range but with too bigger spaces between values
  # Remember that you can pass fit.topics.perplexity any vector of numbers in 'k.values', such as k.values = seq(5, 100, by = 5) OR k.values = c(10,25,50,100)

# NOTE - we can also fit the number of topics using the 'ldatuning' package, which provides advanced methods for selecting k. These more advanced methods are really *not* required for nearly all social science research but can be useful
  # There is a good vignette: https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html


### 2. Fit alpha using perplexity
alpha.values = c(0.0001, 0.001, seq(from = 0.1, to = 1, by = 0.1))
alpha.perplexity = rBDA::fit.alpha.perplexity(dtm = dtm.test,
                                        fold = folds,
                                        k = 5, # use the value of k we calculated earlier
                                        alpha.values = alpha.values)

## Visualise perplexity vs alpha
rBDA::plot_perplexity(data.frame = alpha.perplexity, 
                    value.write = 'Alpha') # we need to stipulate what we are plotting against perplexity

# We can make the x-axis logarithmic, which is useful when plotting a wide range of values which are not evenly spaced (as here)
rBDA::plot_perplexity(data.frame = alpha.perplexity, 
                    value.write = 'Alpha',
                    logx = T)

# And add on the line for where alpha minimizes perplexity
colnames(alpha.perplexity)[which.min(colMeans(alpha.perplexity))] # looks more complicated than it is because of my verbose choice of names for objects
rBDA::plot_perplexity(data.frame = alpha.perplexity, 
                    value.write = 'Alpha',
                    logx = T,
                    xintercept.write = 0.6)

# Interpreting the Perplexity vs. Alpha graph
  # For alpha we just want to take the value which mimimizes perplexity - don't worry about the Elbow method
  # Often, low values of alpha are best - which is why we test for 0.0001, 0.001 and 0.005


### 3. Fit beta using perplexity
beta.values = c(0.0001, 0.001, 0.005, seq(from = 0.1, to = 1, by = 0.1))
beta.perplexity = rBDA::fit.beta.perplexity(dtm = dtm.test, 
                                      folds = folds,
                                      beta.values,
                                      k = 5, # use the value of k we calculated earlier
                                      alpha = 0.2) # use the value of alpha we calculated earlier

colnames(beta.perplexity)[which.min(colMeans(beta.perplexity))]  # to find the value of beta which minimizes perplexity
rBDA::plot_perplexity(data.frame = beta.perplexity, 
                    value.write = 'Beta',
                    logx = T,
                    xintercept.write = 0.1)

# Interpreting the Perplexity vs. Beta graph
  # For beta we just want to take the value which mimimizes perplexity - don't worry about the Elbow method
  # Often, low values of beta are best - which is why we tested for 0.0001, 0.001 and 0.005.


### 4. Fit final topic model using our calculated values
  # Run the parameter fitting commands on the full dataset
  # Use the best fitted parameters in your final model
alpha = 0.2
beta = 0.1
k = 5
  # Note - these values were based on a tiny portion of our data (just for the purposes of this workshop) so probably won't generalize to our full dataset


# We now have a properly fitted topic model!!
  # Ultimately, the value of a topic model is in its useability - so after you've created a fitted topic model, compare it with your original topic model
  # If you want to measure fit using topic coherence (Mimno et al. 2011), there is a function for this in the rBDA package - for details, ?topic.coherence
    # rBDA::topic.coherence(ldaOut, dtm.petitions, n.terms = 10)
      # You need an LDA model to run this function (the 'ldaOut' object)

### End of practical 1 ###

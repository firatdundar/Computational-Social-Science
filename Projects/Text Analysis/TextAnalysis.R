install.packages("tidyverse")
install.packages("gutenbergr")
install.packages("tidytext")
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("SnowballC")
install.packages("topicmodels")

library(tidyverse)
library(gutenbergr)
library(tidytext)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(topicmodels)

# Reading fact-check data from CSV
factchecks <- read_csv("./factchecks.csv")

# Tokenizing the claim_reviewed column
words <- factchecks %>%
  unnest_tokens(word, claim_reviewed)

# Creating a corpus
corpus <- Corpus(VectorSource(words$word))

# Preprocessing the corpus
corpus <- tm_map(corpus, content_transformer(tolower)) # Convert to lowercase
corpus <- tm_map(corpus, removePunctuation) # Remove punctuation
corpus <- tm_map(corpus, removeNumbers) # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("english")) # Remove common English stopwords
corpus <- tm_map(corpus, stripWhitespace) # Remove extra whitespaces
corpus <- tm_map(corpus, content_transformer(gsub), pattern = "http\\S+", replacement = "") # Remove URLs

# Create a vector of words to remove
words_to_remove <- c("says", "people", "percent", "president", "said", 
                     "years", "one", "million", "state", "new",
                     "will", "year", "states", "just", "country",
                     "now", "billion",
                     "united", "since", "going", "time", "money",
                     "pay", "like", "know",
                     "white", "claims",
                     "never", "last", "dont", "going",
                     "first", 
                     "every", "even", "get")

# Remove specified words from the corpus
corpus <- tm_map(corpus, removeWords, words_to_remove)

# Note: Overriding previously created objects
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

# Generate a word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))

# Selecting a subset of columns for analysis
small_fact <- factchecks %>%
  select(author_name, claim_reviewed, review_rating)

# Displaying the head of the selected data
head(small_fact)

# Further preprocessing for analysis
tidy_fact <- small_fact %>% 
  mutate(desc = tolower(claim_reviewed)) %>% # Convert all letters to lowercase
  unnest_tokens(word, desc, token = "regex", pattern = "\\s+|[[:punct:]]+|http.+ |http.+$") %>% # Remove punctuation and tokenize
  mutate(stem = wordStem(word)) %>%   # Stem words
  filter(str_detect(word, "[a-z]")) # Select only tokens (words) starting with a letter a-z, removing emojis, etc.

# Displaying the head of the processed data
head(tidy_fact)

# Removing stop words
tidy_fact <- anti_join(tidy_fact, stop_words)

# Counting word occurrences
tidy_fact <- tidy_fact %>% dplyr::count(claim_reviewed, word)

# Creating a document-term matrix
dtm_facts <- tidy_fact %>% cast_dtm("claim_reviewed", "word", "n")
dtm_facts

# Performing topic modeling with 10 topics
library(topicmodels)
lda_non <- LDA(dtm_facts, k = 10, method = 'Gibbs', control = list(seed = 1234))

# Displaying the top 10 terms for each topic
top10terms_5 <- as.matrix(terms(lda_non, 10))
top10terms_5

# Displaying summary information for the topics
lda.topics_non <- as.matrix(topics(lda_non))
summary(as.factor(lda.topics_non[, 1]))

# Displaying the alpha value used in the model
lda_non@alpha

# Iterating with 15 topics and alpha = 0.5
lda_05 <- LDA(dtm_facts, k = 15, method = 'Gibbs', control = list(seed = 1234, alpha = 0.5)) 
lda_05@alpha
top10terms_05 <- as.matrix(terms(lda_05, 10))
top10terms_05

# Iterating with 20 topics and alpha = 0.3
lda_03 <- LDA(dtm_facts, k = 20, method = 'Gibbs', control = list(seed = 1234, alpha = 0.3)) 
lda_03@alpha
top10terms_03 <- as.matrix(terms(lda_03, 10))
top10terms_03

# Discussion of Results
# ----------------------

# Initially, I experimented with 10 subjects using a general algorithm.
# The resulting topics were not very interpretable, and the alpha value was set to 0.5.
# Subsequently, I increased the topic count to 15 and lowered the alpha value.
# With these adjustments, more distinct topics emerged; for instance, Topic 12 was clearly associated with elections,
# featuring terms related to Democrats and Republicans. Topic 6 appeared to revolve around Trump and Obama's election campaigns.

# Further refinement was attempted with 20 topics and a smaller alpha value,
# leading to even more insightful and relevant topics. Two topics seemed to center around the financial aspects
# of trade between America and China. Additionally, Topic 13 coalesced around health systems and the Obamacare law.
# New topics surfaced, covering immigration and the legalization of marijuana in California.

# In conclusion, a more comprehensive and open interpretation was achieved with higher topic numbers and lower alpha values.
# Emphasizing the importance of preprocessing, a broad topic range, and model design, we eliminate unnecessary words
# through preprocessing, enhancing model quality.
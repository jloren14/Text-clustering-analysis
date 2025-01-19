library(tm)
library(tidytext)
library(dplyr)
library(stringr)
library(tokenizers)
library(stopwords)
library(proxy)
library(tidyverse)


setwd("")

# load data
load("~/Data/firm_dataset.Rdata")

################################################################
# CLEAN THE TEXT DATA
# We are going to make a loop where we iterate over each element of the section.1.business vector

cleaned_data <- c()

for (i in seq_along(section.1.business)) {
  # make the text lower and remove: numbers, punctuation, unnessecary whitespaces - on the each element of the vector
  article_text <- section.1.business[i] %>%
    tolower() %>%
    removePunctuation() %>%
    removeNumbers() %>%
    str_squish()
  
  # create a tibble out of the article text to unnest the tokens and perform filtering to:
  # 1) remove the stopwords
  # 2) consider only terms with 3-20 letters
  cleaned_tokens_vector <- article_text %>%
    as_tibble() %>%
    unnest_tokens(input = value, output = tokens) %>%
    filter(!tokens %in% stopwords()) %>%
    filter(nchar(tokens) >= 3 & nchar(tokens) <= 20) %>%
    pull(tokens)
  
  # append the cleaned article text to the vector with cleaned data
  cleaned_data <- c(cleaned_data, paste(cleaned_tokens_vector, collapse = " "))
  
}

# Check if the cleaning process worked - yes
cleaned_data[1]

################################################################
# TOKENIZE DATA INTO BIGRAMS

text_bigrams <- sapply(cleaned_data, function(text){
  tokenize_ngrams(text, n = 2, ngram_delim = "_")})

################################################################
# CREATE A DOCUMENT TERM MATRIX

corpus <- Corpus(VectorSource(text_bigrams))
dtm <- DocumentTermMatrix(corpus,
                          control = list(
                            bounds = list(global = c(5,100)))) # only including bi-grams that appear in more than 5 but less than 100 documents

################################################################
# IDENTIFY FIRMS IN THE OIL SECTOR AND FIRMS NOT IN THE OIL SECTOR

oil_firms <- raw.data %>%
  filter(industry.fama.french.49 == "30 Oil") %>%
  pull(cik)

non_oil_firms <- raw.data %>%
  filter(industry.fama.french.49 != "30 Oil") %>%
  pull(cik)

################################################################
# LOG-LIKELIHOOD OF BEING THE SUBSET OF OIL FIRMS AND THE REMAINING

# Log-likelihood funciton

# a = freq. of a bi-gram in the oil corpus
# b = freq. of a bi-gram in the non-oil corpus
# c = sum of all bi-grams in the oil corpus
# d = sum of all bi-grams in the non-oil corpus

calculate.ll <- function(a, b, c, d){
  e1 <- c*(a+b)/(c+d)
  e2 <- d*(a+b)/(c+d)
  ll <- 2*((a*log(a/e1)) + (b*log(b/e2)))
  return(ll)
}

# Get indices of oil and non-oil firms in the DTM
oil_indices <- which(raw.data$cik %in% oil_firms)
non_oil_indices <- which(raw.data$cik %in% non_oil_firms)

# Subset DTM into oil and non-oil firms
dtm_oil <- dtm[oil_indices, ]
dtm_non_oil <- dtm[non_oil_indices, ]

# Calculate total sum of bi-grams in each group (for 'c' and 'd')
c_sum <- sum(dtm_oil)
d_sum <- sum(dtm_non_oil)

# Get the bi-gram frequencies (for 'a' and 'b')
bi_gram_names <- colnames(dtm)  # Get the bi-gram names from the DTM

# Initialize a vector to store the log-likelihoods
log_likelihoods <- numeric(length(bi_gram_names))

# Loop over each bi-gram to calculate log-likelihood
for (i in seq_along(bi_gram_names)) {
  # a - frequency of the bi-gram in the oil corpus
  a <- sum(dtm_oil[, i])
  
  # b - frequency of the bi-gram in the non-oil corpus
  b <- sum(dtm_non_oil[, i])
  
  # Calculate the log-likelihood for i-th bi-gram
  log_likelihoods[i] <- calculate.ll(a, b, c_sum, d_sum)
  
}

# Create a data frame of bi-grams and their log-likelihoods
log_likelihood_df <- data.frame(
  bi_gram = bi_gram_names,
  log_likelihood = log_likelihoods
)

# Sort by log-likelihood in descending order and remove NA values
log_likelihood_df <- log_likelihood_df %>%
  arrange(desc(log_likelihood))

# Display the top tokens
head(log_likelihood_df, 5)

library(xtable)

# Export the table to LaTeX
print(xtable(head(log_likelihood_df, 5)), include.rownames = FALSE)

# Select the top 500 bi-grams by log-likelihood
top_500_bigrams <- log_likelihood_df %>%
  slice(1:500) %>%
  pull(bi_gram)

###########################################################################
# FOR EACH OIL FIRM, FIND 5 NON-OIL PEER FIRMS USING COSINE SIMILARITY

# Convert dtm to matrix for cosine similarity 
dtm_matrix <- as.matrix(dtm)

# Cosine similarity function
CosineSimilarity <- function(A, B) {
  sum(A*B)/
    sqrt(sum(A^2)*sum(B^2))
}

# Data frame to store cosine similarities 
cosine_similarities <- data.frame(oil_firm = character(), non_oil_firm = character(), similarity = numeric())

# Loop through each oil firm
for (oil_firm_id in oil_firms) {
  
  #G et a vector for the oil firm
  oil_vector <- as.vector(dtm[raw.data$cik == oil_firm_id,])
  
  # Loop through each non-oil firm
  for (non_oil_firm_id in non_oil_firms) {
    
    # Get a vector for the non-oil firm
    non_oil_vector <- as.vector(dtm[raw.data$cik == non_oil_firm_id,])
    
    # Compute cosine similarity 
    similarity <- CosineSimilarity(A = oil_vector, B = non_oil_vector)
    
    # Store the result in the data frame
    cosine_similarities <- rbind(cosine_similarities, 
                                 data.frame(oil_firm = oil_firm_id, 
                                            non_oil_firm = non_oil_firm_id, 
                                            similarity = similarity))
  }
}

# Print cosine similarities 
print(cosine_similarities)

##For each oil firm, find the top 5 non-oil firms with the highest cosine similarity
top_5_peers <- cosine_similarities %>%
  group_by(oil_firm) %>%
  arrange(desc(similarity)) %>%
  slice_head(n = 5)

raw.data.industries <- raw.data %>% select(cik, industry.fama.french.49)  # Select the correct columns from `raw.data`

# Get the industries of the non-oil firms
top_5_peers <- left_join(x = top_5_peers,
                         y = raw.data.industries,
                         by = c("non_oil_firm" = "cik"))

View(top_5_peers)

# Export the top_5_peers dataframe to LaTeX
latex_table <- xtable(top_5_peers, caption = "Top 5 Non-Oil Peers for Each Oil Industry Based on Cosine Similarity")

print(latex_table, 
      include.rownames = FALSE,  # Do not include row names
      caption.placement = "top",  # Place caption above the table
      table.placement = "ht")  # Add horizontal lines


################################################################################
# COMPUTE THE AVERAGE RETURN FOR THE OIL PORTFOLIO AND THE PEER GROUP FOR 2014

#Create vectors to store the returns for the oil and peer portfolios
oil_returns <- numeric()
peer_returns <- numeric()

#Loop through each oil firm
for (oil_firm_id in oil_firms) {
  oil_return <- raw.data %>%
    filter(cik == oil_firm_id) %>%
    select(return.monthly.NY.m01:return.monthly.NY.m12) %>%
    rowMeans()
  
  peer_firm_ids <- top_5_peers %>%
    filter(oil_firm == oil_firm_id) %>%
    pull(non_oil_firm)
  
  peer_return <- raw.data %>%
    filter(cik %in% peer_firm_ids) %>%
    select(return.monthly.NY.m01:return.monthly.NY.m12) %>%
    rowMeans()
  
  oil_returns <- c(oil_returns, oil_return)
  peer_returns <- c(peer_returns, peer_return)
}


#Calculate the average return for oil and peer portfolios
average_oil_return <- mean(oil_returns, na.rm = TRUE)
average_peer_return <- mean(peer_returns, na.rm = TRUE)

average_oil_return
average_peer_return

# exporting in LaTex
avg_returns <- data.frame(
  Category = c("Oil Firms", "Peer Firms"),
  Average_Return = c(-0.03463936, -0.002824757)
)

# Create a LaTeX table
latex_table <- xtable(avg_returns, caption = "Average Returns for Oil Firms and Their Peers", label = "tab:average_returns")

# Print the table in LaTeX format
print(latex_table, table.placement = "h", include.rownames = FALSE)

###########################################################################
# EVALUATE THE PERFORMANCE OF THE ALGORITHM WITH RMSE

##Compute the RMSE between the oil and peer portfolios
rmse <- sqrt(mean((oil_returns - peer_returns)^2, na.rm = TRUE))
rmse

rmse_data <- data.frame(
  Metric = "RMSE",
  Value = rmse
)

# Create a LaTeX table for RMSE
latex_rmse_table <- xtable(rmse_data, caption = "Root Mean Square Error (RMSE) between Oil and Peer Portfolios", label = "tab:rmse")

# Print the RMSE table in LaTeX format
print(latex_rmse_table, table.placement = "h", include.rownames = FALSE)

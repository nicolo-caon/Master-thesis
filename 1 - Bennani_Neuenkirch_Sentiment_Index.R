Sentiment index of Italian Central Bank based on the job of Bennani & Neuenkirch 2015
# Load necessary libraries
library(dplyr)
library(tidytext)
library(SnowballC)
library(stringr)
library(readr)
# Load the datasets with proper encoding
Statements <- read_csv("BoI Bollettini Eco Transl.csv", locale = locale(encoding = "ISO-8859-1"))
Bias_keywords <- read.csv("Keywords_Scores_Bennani_Neuenkirch.csv", fileEncoding = "UTF-8-BOM")
head(Statements)
# Preprocess keywords: lowercase and stem them
Bias_keywords <- Bias_keywords %>%
  mutate(keyword = tolower(keyword)) %>%
  mutate(keyword = wordStem(keyword))

# Define a function to clean and calculate the score for each statement
calculate_score <- function(text, keywords) {
  # Tokenize, remove punctuation, numbers, and stopwords, then stem words
  words <- data.frame(word = unlist(strsplit(tolower(text), "\\W+"))) %>%
    filter(!str_detect(word, "\\d")) %>%          # Remove numbers
    filter(word != "") %>%                        # Remove empty strings
    anti_join(stop_words, by = c("word" = "word")) %>%  # Remove stopwords
    mutate(word = wordStem(word)) %>%             # Stem words
    filter(word %in% keywords$keyword)            # Match against keywords
  
  # Join with the keywords to get their types
  matched_words <- words %>%
    inner_join(keywords, by = c("word" = "keyword"))
  
  # Calculate score: +1 for hawkish, -1 for dovish
  score <- sum(ifelse(matched_words$type == "Hawkish", 1, -1))
  
  # Calculate the total number of dovish and hawkish words
  total_words <- nrow(matched_words)
  
  # Normalize the score
  normalized_score <- ifelse(total_words > 0, score / total_words, 0)
  
  return(normalized_score)
}

# Apply the scoring function to each row in the dataset
Statements <- Statements %>%
  rowwise() %>%
  mutate(
    Score = calculate_score(`Announcement Translated`, Bias_keywords) 
  ) %>%
  ungroup()

# Specify the directory to save the file
output_directory <- .\\MyThesis\\sent index ita" 
output_file <- file.path(output_directory, "Macro_Proj__Score_Bennani_Neuenkirch.csv")

# Save the updated Statements dataset with scores
write.csv(Statements, output_file, row.names = FALSE)

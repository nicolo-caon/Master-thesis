#Sentiment index for the Italian Central Bank based on the work of Jardner et al. 2022. Developed as part of a group project, revised for the scope of my master thesis
#libraries
library(dplyr)
library(tm)
library(dplyr)
library(lubridate)

## Reading in the data 
cleaned_data <- read.csv(".\\Master Thesis\\Sent Index\\BoI Bollettini Eco Transl.csv")
List_of_Keywords_and_Their_Scores <- read.csv(".\\Master Thesis\\Sent Index\\List of Keywords and Their Scores.csv")
List_of_Modifiers_and_Their_Scores<-read.csv(".\\Master Thesis\\Sent Index\\List of Modifiers and Their Scores.csv")
#View(cleaned_data)
################################################################################################
## step 0 ## First need to clean the data from stop words etc.

## First convert to date format
cleaned_data$`Date.of.Announcement` <- mdy(cleaned_data$`Date.of.Announcement`)

# To corpus
cleaned_data <- cleaned_data[,-c(3)]

# Giving names to columns
names(cleaned_data) <- c("Date", "text")

# Convert text encoding to UTF-8 with error handling
cleaned_data$text <- tryCatch({
  iconv(cleaned_data$text, from = "", to = "UTF-8", sub = "")
}, warning = function(w) {
  message("Warning during encoding conversion: ", w)
  cleaned_data$text  # Return original text if conversion fails
}, error = function(e) {
  message("Error during encoding conversion: ", e)
  cleaned_data$text  # Return original text if conversion fails
})

# Create a VCorpus from the text column
corpus <- VCorpus(VectorSource(cleaned_data$text))

# Convert to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)

# Remove numbers
corpus <- tm_map(corpus, removeNumbers)

# Remove common English stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Update the "text" column in the original data frame with the cleaned text
cleaned_data$text <- sapply(corpus, function(x) paste(unlist(x), collapse = " "))

# Removing double spaces
cleaned_data$text <- gsub("\\s+", " ", cleaned_data$text)

################################################################################

#List of the keywords and modifiers
modifiers <-List_of_Modifiers_and_Their_Scores$modifier
keywords <-List_of_Keywords_and_Their_Scores$keyword

# We need - Statements, Date, The 4 sentiment indices + 1 overall sentiment indices
# Add 5 new columns with NA values using mutate
cleaned_data <- cleaned_data %>%
  mutate(sentiment_index = NA,
         index_inflation = NA,
         index_output = NA,
         index_labor = NA,
         index_financial = NA)

# Looping the length (amount of statements)
amount_of_statements <- length(cleaned_data[,2])

for (q in 1:amount_of_statements) {
text <- cleaned_data[q,2] ##The text is the i row (1-80 statements) and column = 2 which is where the statement text is
 
# Tokenize the text into words
words_in_text <- unlist(strsplit(tolower(text), "\\W+"))

# Create a dateframe where each word is a column
text_df <- data.frame(Word = words_in_text)

# Will need the words in their original order
text_df <- text_df %>%
  mutate(OrderOfWords = row_number())
text_df <- text_df %>%
  select(OrderOfWords, everything())

# Create a new column based on if there is a keyword match
text_df$KeyWordMatchIndicator <- apply(text_df, 1, function(row) {
  any(row %in% keywords)
})
text_df$KeyWordMatchIndicator <- as.numeric(text_df$KeyWordMatchIndicator)

# Create a new column based on modifier match
text_df$ModifierWordMatchIndicator <- apply(text_df, 1, function(row) {
  any(row %in% modifiers)
})
text_df$ModifierWordMatchIndicator <- as.numeric(text_df$ModifierWordMatchIndicator)


text_df$ClosestModifier <- NA
# Loop over the length of KeyWordMatchIndicator
for (i in seq_along(text_df$KeyWordMatchIndicator)) {
  if (text_df$KeyWordMatchIndicator[i] == 0) {
    # Nothing happens, we dont care
  } else if (text_df$KeyWordMatchIndicator[i] == 1) {
    matching_length_df <- list()
    
    # The position of the keyword = i. Now, we need to find the closest modifier
    for (j in seq_along(text_df$ModifierWordMatchIndicator)) {
      if (text_df$ModifierWordMatchIndicator[j] == 1) {
        absolute_distance <- abs(j - i)
        temp_data_frame <- data_frame(id = j, distance = absolute_distance)
        
        # Add the data frame to the list
        matching_length_df <- c(matching_length_df, list(temp_data_frame))
      }
      
    }
    
    combined_df <- do.call(rbind, matching_length_df)
    
    # Find the row with the minimum absolute distance
    min_distance_row <- combined_df[which.min(combined_df$distance), ]
    
    # here we write down the closet match for each keyword, we write down the ID which is which word is it 
    text_df$ClosestModifier[i] <- min_distance_row$id
  }
}

# Let's create the pairs
text_df$pairs <- NA
text_df$PairWord <- NA
for (r in seq_along(text_df$KeyWordMatchIndicator)) {
  if (text_df$KeyWordMatchIndicator[r] == 0) {
    # Nothing happens, we dont care
  } else if (text_df$KeyWordMatchIndicator[r] == 1) {
    
    # Word[i] this is the keyword. Word['id of closetmodifiers] is the modifier
   
    text_df$pairs[r] <- paste(text_df$Word[r], text_df$Word[text_df$ClosestModifier[r]], sep = '-')
    text_df$PairWord[r] <- paste(text_df$Word[text_df$ClosestModifier[r]])
  }
}
## Now we need to put a value on the pairs
names(List_of_Keywords_and_Their_Scores) <-c("Word", "KeyWordScore", "KeyWordTopic")
names(List_of_Modifiers_and_Their_Scores) <-c("PairWord", "ModifiersScore", "ModifiersTopic")

text_df <-merge(text_df,List_of_Keywords_and_Their_Scores, by = "Word", all.x = TRUE)
text_df <-merge(text_df,List_of_Modifiers_and_Their_Scores, by = "PairWord", all.x = TRUE)

# Calculate the product
text_df$product <- text_df$KeyWordScore * text_df$ModifiersScore

# Sentiment index for each topic
# We have 4 topics only
index_inflation <-0
index_output <-0
index_labor <-0
index_financial <-0

for (z in seq_along(text_df$PairWord)) {
  
  if (!is.na(text_df$KeyWordTopic[z])) { 
    
    if (text_df$KeyWordTopic[z] == "inflation") {
      index_inflation <- (index_inflation+text_df$product[z])
    } else if (text_df$KeyWordTopic[z] == "output") {
      index_output <- (index_output+text_df$product[z])
    } else if (text_df$KeyWordTopic[z] == "labor") {
      index_labor <- (index_labor+text_df$product[z])
    } else if (text_df$KeyWordTopic[z] == "financial") {
      index_financial <- (index_financial+text_df$product[z])
    }
    
  }
}
# Creating an overall sentiment score by summing up all these 4 indices
sentiment_index <- sum(index_inflation,index_output,index_labor,index_financial)

# Creating the index for each topic by diving by the numbers of words in the statement
num_words <- length(text_df$Word)
sentiment_index <- sentiment_index/sqrt(num_words)

index_inflation <- index_inflation/sqrt(num_words)
index_output <- index_output/sqrt(num_words)
index_labor <- index_labor/sqrt(num_words)
index_financial <- index_financial/sqrt(num_words)

# Putting the indices into the dataframe
cleaned_data$sentiment_index[q] <- sentiment_index

cleaned_data$index_inflation[q] <- index_inflation
cleaned_data$index_output[q] <- index_output
cleaned_data$index_labor[q] <- index_labor
cleaned_data$index_financial[q] <- index_financial

print("Statement:")
print(q)
print("Overall sentiment index:")
print(sentiment_index)
}

# Save the updated cleaned_data with sentiment_index to a new CSV file
write.csv(cleaned_data, ".\\Master Thesis\\Sent Index\\BoI Bollettini Eco Sent Index.csv", row.names = FALSE)

print("Updated CSV file with sentiment index has been saved.")

library(quanteda)

#
# Functions to tokenize text into sentences, and sentences into tokens.
#

# Make punctuation symbols uniform
uniformize_punct <- function(input_data) {
    t <- gsub('’', '\'', input_data)
    t <- gsub('”|“', '"', t)
    #t <- gsub('[\\w-]+@([\\w-]+\\.)+[\\w-]+', "", t, perl = TRUE) # emails, should we cut out?
    t <- gsub('(_)+', ' ', t, perl = TRUE) # cuts out underscores to avoid messing with tokenization
}

# Split data into words.
tokenize_sentences <- function(sentences) {
    tokenize(toLower(sentences),
             removeNumbers = TRUE,
             removePunct = TRUE,
             removeURL = TRUE,
             removeSymbols = TRUE)
}

# Split data into sentences
make_sentences <- function(input_data) {
    # when sentencing, removing punctuation, etc. does nothing.
    tokenize(input_data, what = "sentence", simplify = TRUE)
}
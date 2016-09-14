library(hash)
library(LaF)
library(quanteda)
library(data.table)
library(dplyr)
library(slam)
library(ggplot2)
library(grid)
library(gridExtra)
library(doParallel)

# for reproduceability
set.seed(928374239)

setwd(
    "D:/onedrive/OneDrive - Nokia/Training/Coursera/Data Science Specialization/Capstone Project"
)


# Sample training data.
# Value:
#   A list of lines, sampled from the training data.
sampleTrainData <- function(sample.pct = 0.1) {
    sn <- list(list(f = "work/train/en_US.blogs.txt", nl = 540061),
               list(f = "work/train/en_US.news.txt", nl = 605768),
               list(f = "work/train/en_US.twitter.txt", nl = 1416316))
    
    cr <- iconv(
        unlist(
            lapply(sn, function(z) { sample_lines(z$f, z$nl * sample.pct, z$nl) })
        ), 
        from = "UTF-8")
    cr
}

# Make punctuation symbols uniform
uniformize_punct <- function(input_data) {
    t <- gsub('’', '\'', input_data)
    t <- gsub('”|“', '"', t)
    #t <- gsub('[\\w-]+@([\\w-]+\\.)+[\\w-]+', "", t, perl = TRUE) # emails, should we cut out?
    t <- gsub('(_)+', ' ', t, perl = TRUE) # cuts out underscores to avoid messing with tokenization
}


# Split data into sentences
make_sentences <- function(input_data) {
    
    # when sentencing, removing punctuation, etc. does nothing.
    tokenize(input_data, what = "sentence", simplify = TRUE)
}

# Split data into words.
tokenize_sentences <- function(sentences) {
    tokenize(toLower(sentences),
             removeNumbers = TRUE,
             removePunct = TRUE,
             removeURL = TRUE,
             removeSymbols = TRUE)
}

# Calculate a documentxfrequency matrix of n_grams
calcDFM <- function(tok, n_gram = 1) {
    docFM <- dfm(ngrams(tok, n = n_gram))
    
    docFM
}

# compacts a corpus' DocumentFrequencyMatrix into a data.table of terms and frequencies, 
# ordered by frequency.
constructFrequencyTable <- function(docFM) {
    term_sums <- col_sums(docFM)
    
    dt <- data.table(ngram = names(term_sums), frequency = term_sums) %>%
        arrange(desc(frequency))
    
    if (length(unlist(strsplit(dt[1,'ngram'], '_'))) == 1) {
        # 1-gram
        dt <- mutate(dt, Last = "", Pref = "")
    } else {
        # n-gram
        dt$Last <- unname(sapply(dt[, 'ngram'], 
                                 function(x) { 
                                     last(unlist(strsplit(x, '_')))
                                 }))
        
        dt$Prefix <- unname(sapply(dt[, 'ngram'],
                                 function(x) {
                                     s <- unlist(strsplit(x, '_'))
                                     s <- s[1:(length(s)-1)]
                                     paste0(s, collapse = '_')
                                 }))
    }
    
    dt
}

export.ngrams <- function(ngrams) {
    
    sapply(seq(1,5), function(g) {
        write.table(col_sums(ngrams[[g]]), file = paste0("ngram", g, ".txt"), col.names = FALSE)    
    })
}

import.freq.tables <- function(table_dir) {
    
    lapply(seq(1,5), function(i) {
        fread(input = paste0(table_dir, "/p_ngram", i, ".csv"),
              header = FALSE,
              stringsAsFactors = FALSE,
              col.names = c("ngram", "frequency", "Last", "Prefix"))
    })
}


train_data <- sampleTrainData()
sentences <- make_sentences(train_data)
toks <- tokenize_sentences(uniformize_punct(sentences))

Ngrams <- sapply(seq(1,5), function(n) {
    message("Calculating ", n, "-gram...")
    t <- system.time({
        d <- calcDFM(toks, n)
    })
    message("Calculated ", n, "-gram table in ", round(t[3], 2), "seconds.")
    
    d
})

# freqTables <- lapply(seq(1,5), function(n) {
#     message("Calculating ", n, "-gram frequency table...")
#     t <- system.time({ f <- constructFrequencyTable(Ngrams[[n]]) })
#     message("Done in ", round(t[3], 2), " seconds.")
#     f
# })

# Guesses the next word in the phrase based on stupid backoff algorithm
guess_word <- function(in.phrase, freq.tables) {
    lambda <- 0.04
    
    tok_phrase <- (tokenize_sentences(uniformize_punct(in.phrase)))
    
    message("Tokenized phrase: ", tok_phrase)
    
    s <- max(1, length(tok_phrase[[1]])-3)
    e <- length(tok_phrase[[1]])
    ng <- length(tok_phrase[[1]]) - s+2 
    
    message("len:", length(tok_phrase[[1]]), " s:", s, " e:", e, " ng:", ng)
    
    while(s != e) {
        match.ngram.prefix <- paste0(tok_phrase[s:e], collapse = '_')
        input.n_1.gram <- paste0(tok_phrase[s:(e-1)], collapse = '_')
     
        matches.ngram <- filter(freq.tables[[ng]], Pref == match.ngram)
        matches.n_1.gram <- filter(freq.tables[[ng-1]], ngram == input.n_1.gram)
        
        count.match.ngram <- summarise(matches.ngram, c = sum(frequency))[1,1]
        
        for (i in seq(1, min(5, dim(matches.ngram)[2]))) {
            score[length(score)+1] <- matches.ngram[1, 'frequency'] /
        }
        
    }
    
}
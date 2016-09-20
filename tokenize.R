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
    lambda <- 0.4
    l <- 1
    
    tok_phrase <- (tokenize_sentences(uniformize_punct(in.phrase)))[[1]]
    
    message("Tokenized phrase: ", tok_phrase)
    
    s <- max(1, length(tok_phrase)-3)
    e <- length(tok_phrase)
    ng <- length(tok_phrase) - s+2 
    
    message("len:", length(tok_phrase), " s:", s, " e:", e, " ng:", ng)
    
    cand <- list()
    score <- list()
    
    while(ng > 1 && length(cand) < 5) {
        message("Iteration.. s:", s, " e:", e, " ng:", ng)
        match.ngram.prefix <- paste0(tok_phrase[s:e], collapse = '_')
     
        matches.ngram <- filter(freq.tables[[ng]], Prefix == match.ngram.prefix)
        message("  ...found ", dim(matches.ngram)[1], " ", ng, "-grams that match ", match.ngram.prefix)
        
        
        if (dim(matches.ngram)[1] != 0) {
            
            # If the n-gram was found, the n-1 gram must be present
            count.match.n_1.ngram <- filter(freq.tables[[ng-1]], ngram == match.ngram.prefix)[, 'frequency']
            message("  ...total ocurrences of ", ng-1, "-gram ", match.ngram.prefix, ": ", count.match.n_1.ngram)
            
            picked <- 0
            indx <- 1
            stp <- FALSE
            while (stp == FALSE) {
                c_cand <- matches.ngram[indx, 'Last']
                
                if (length(cand[cand == c_cand]) == 0) {
                    # The candidate word wasn't already picked by a previous iteration
                    cand[length(cand)+1] <- matches.ngram[indx, 'Last']
                    score[length(score)+1] <- round(l * matches.ngram[indx, 'frequency'] / count.match.n_1.ngram, 4)
                    picked <- picked + 1
                    
                    message("  ...picked [", indx, "]: ", c_cand, ", score: ", score[length(score)])
                }
                
                indx <- indx + 1
                
                if (indx > dim(matches.ngram)[1] || picked == 5) {
                    # we exausted the list or picked the top 5 from the ngram list
                    stp <- TRUE
                }
            }
        }
        
        s <- s+1
        l <- l * lambda
        ng <- ng-1
    }
    
    message("Stopped cycle at ", length(cand), " candidates, ", ng+1, "-grams.")
    
    # Return only the overall top 5 candidates
    (data.frame(word = unlist(cand), score = unlist(score)) %>% arrange(desc(score)))[1:5, ]
}


zz <- function(u) {
    for (i in seq(1, dim(bi)[1])) {
        ngr <- bi[i, 'ngram']
        t <- strsplit(ngr, '_')
        r <- sapply(t, function(z) {
            u[u$ngram == z, 'ind']
        })
        bi[i, 'ngram.num'] <- paste0(r, collapse='_')
    }
}




# Rprof("prof")
# replicate(n=10, guess_word("I have found enlightenment", freqT))
# Rprof(NULL)
# summaryRprof("prof")

library(data.table)
library(dplyr)
library(quanteda)

import.luts <- function(table_dir) {
    
    dict <- fread(input = paste0(table_dir, "/p_num_ngram_words.csv"),
                  header = FALSE,
                  stringsAsFactors = FALSE,
                  col.names = c("word", "index", "frequency"))
    
    two <- fread(input = paste0(table_dir, "/p_num_ngram2.csv"),
                 header = FALSE,
                 stringsAsFactors = FALSE,
                 col.names = c("W1", "W2", "frequency"))
    
    three <- fread(input = paste0(table_dir, "/p_num_ngram3.csv"),
                   header = FALSE,.
                   stringsAsFactors = FALSE,
                   col.names = c("W1", "W2", "W3", "frequency"))
    
    four <- fread(input = paste0(table_dir, "/p_num_ngram4.csv"),
                  header = FALSE,
                  stringsAsFactors = FALSE,
                  col.names = c("W1", "W2", "W3", "W4", "frequency"))
    
    five <- fread(input = paste0(table_dir, "/p_num_ngram5.csv"),
                  header = FALSE,
                  stringsAsFactors = FALSE,
                  col.names = c("W1", "W2", "W3", "W4", "W5", "frequency"))
    
    list(dict = dict, two = two, three = three, four = four, five = five)
}


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

cut_unknown <- function(word.indexes) {
    r <- list()
    w <- word.indexes
    
    # special case - if the very last word is unknown, then take preceding words (if known) as context
    if (is.na(w[length(w)])) {
        w <- w[1:(length(w)-1)]
    }
    
    i <- length(w)
    stp <- FALSE
    
    while(i > 0 && stp == FALSE) {
        if (! is.na(w[i])) {
            r[length(r) + 1] <- w[i]
        } else {
            stp <- TRUE
        }
        i <- i-1
    }
    
    rev(unlist(r))
}


# Guesses the next word in the phrase based on stupid backoff algorithm
guess_word <- function(in.phrase, look.up.tables) {
    lambda <- 0.4
    l <- 1
    
    tok_phrase <- (tokenize_sentences(uniformize_punct(in.phrase)))[[1]]
    message("Tokenized phrase: ", paste0(tok_phrase, collapse="_"))
    
    s <- max(1, length(tok_phrase)-3)
    e <- length(tok_phrase)
    ng <- length(tok_phrase) - s+2 
    
    match_cand <- sapply(tok_phrase[s:e], function(t) {
        z <- look.up.tables$dict[look.up.tables$dict$word == t]$index
        if (identical(z, integer(0))) {
            NA
        } else {
            z
        }
    })
    message("Match cand: ", paste0(match_cand, collapse = '_'))
    
    match_cand <- cut_unknown(match_cand)
    message("Match cand, cut: ", paste0(match_cand, collapse = '_'))
    
    ng <- length(match_cand)
    cand <- list()
    score <- list()
    
    if (ng == 4) {
        cands <- filter(look.up.tables$five, 
                        W1 == match_cand[1],
                        W2 == match_cand[2],
                        W3 == match_cand[3],
                        W4 == match_cand[4])
        
        message("  ...found ", dim(cands)[1], " candidates at ", ng+1)
        
        if (dim(cands)[1] > 0) {
            cnt.four_g <- filter(look.up.tables$four,
                                 W1 == match_cand[1],
                                 W2 == match_cand[2],
                                 W3 == match_cand[3],
                                 W4 == match_cand[4])$frequency
            
            for (i in seq(1, dim(cands)[1])) {
                cand[length(cand)+1] <- 
                    look.up.tables$dict[look.up.tables$dict$index == cands[i, 'W5']]$word
                score[length(score)+1] <- l * cands[i, 'frequency'] / cnt.four_g
                
                message("  ... candidate at ", ng+1, "-gram: ", cand[length(cand)], 
                        ", score: ", score[length(score)])
            }
        }
        
        l <- l * lambda
        ng <- ng - 1
        match_cand <- match_cand[2:length(match_cand)]
    }
    
    if (ng == 3 && length(cand) < 5) {
        cands <- filter(look.up.tables$four,
                        W1 == match_cand[1],
                        W2 == match_cand[2],
                        W3 == match_cand[3])
        
        message("  ...found ", dim(cands)[1], " candidates at ", ng+1)
        
        if (dim(cands)[1] > 0) {
            cnt.three_g <- filter(look.up.tables$three,
                                 W1 == match_cand[1],
                                 W2 == match_cand[2],
                                 W3 == match_cand[3])$frequency
            
            for (i in seq(1, dim(cands)[1])) {
                c <- look.up.tables$dict[look.up.tables$dict$index == cands[i, 'W4']]$word
                
                if (length(cand[cand == c]) == 0) {
                    # candidate word 'c' isn't already picked
                    
                    cand[length(cand)+1] <- c
                    score[length(score)+1] <- l * cands[i, 'frequency'] / cnt.three_g
                    
                    message("  ... candidate at ", ng+1, "-gram: ", cand[length(cand)], 
                            ", score: ", score[length(score)])
                }
            }
        }
        
        l <- l * lambda
        ng <- ng - 1
        match_cand <- match_cand[2:length(match_cand)]
    }
    
    if (ng == 2 && length(cand) < 5) {
        cands <- filter(look.up.tables$three,
                        W1 == match_cand[1],
                        W2 == match_cand[2])
        
        message("  ...found ", dim(cands)[1], " candidates at ", ng+1)
        
        if (dim(cands)[1] > 0) {
            cnt.two_g <- filter(look.up.tables$two,
                                  W1 == match_cand[1],
                                  W2 == match_cand[2])$frequency
            
            for (i in seq(1, dim(cands)[1])) {
                c <- look.up.tables$dict[look.up.tables$dict$index == cands[i, 'W3']]$word
                
                if (length(cand[cand == c]) == 0) {
                    # candidate word 'c' isn't already picked
                    
                    cand[length(cand)+1] <- c
                    score[length(score)+1] <- l * cands[i, 'frequency'] / cnt.two_g
                    
                    message("  ... candidate at ", ng+1, "-gram: ", cand[length(cand)], 
                            ", freq: ", cands[i, 'frequency'], ", cnt:", cnt.two_g,
                            ", score: ", score[length(score)])
                }
            }
        }
        
        l <- l * lambda
        ng <- ng - 1
        match_cand <- match_cand[2:length(match_cand)]
    }
    
    if (ng == 1 && length(cand) < 5) {
        cands <- filter(look.up.tables$two,
                        W1 == match_cand[1])
        
        message("  ...found ", dim(cands)[1], " candidates at ", ng+1)
        
        if (dim(cands)[1] > 0) {
            cnt.one_g <- filter(look.up.tables$dict,
                                index == match_cand[1])$frequency
            
            message("  ...count one: ", cnt.one_g)
            
            for (i in seq(1, dim(cands)[1])) {
                c <- look.up.tables$dict[look.up.tables$dict$index == cands[i, 'W2']]$word
                
                if (length(cand[cand == c]) == 0) {
                    # candidate word 'c' isn't already picked
                    
                    cand[length(cand)+1] <- c
                    score[length(score)+1] <- l * cands[i, 'frequency'] / cnt.one_g
                    
                    message("  ... candidate at ", ng+1, "-gram: ", cand[length(cand)], 
                            ", score: ", score[length(score)])
                }
            }
        }
        
        l <- l * lambda
        ng <- ng - 1
        match_cand <- match_cand[2:length(match_cand)]
    }
    
    message("Stopped at ", length(cand), " candidates, ", ng+1, "-grams.")

    # Return only the overall top 5 candidates
    (data.frame(word = unlist(cand), score = unlist(score)) %>% arrange(desc(score)))[1:5, ]
}








#luts <- import.luts("d:/code/internal_tools/wordParser")


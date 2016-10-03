library(data.table)
library(dplyr)
library(quanteda)

source("tokenize.R")

#
# Functions to predict words based on Look-Up Tables.
#


# Reads-in LUTs from disk.
import.luts <- function(table_dir) {
    
    dict <- fread(input = paste0(table_dir, "/p_num_ngram_words.csv"),
                  header = FALSE,
                  stringsAsFactors = FALSE,
                  col.names = c("word", "index", "frequency")) %>% arrange(desc(frequency))
    
    two <- fread(input = paste0(table_dir, "/p_num_ngram2.csv"),
                 header = FALSE,
                 stringsAsFactors = FALSE,
                 col.names = c("W1", "W2", "frequency")) %>% arrange(desc(frequency))
    
    three <- fread(input = paste0(table_dir, "/p_num_ngram3.csv"),
                   header = FALSE,
                   stringsAsFactors = FALSE,
                   col.names = c("W1", "W2", "W3", "frequency")) %>% arrange(desc(frequency))
    
    four <- fread(input = paste0(table_dir, "/p_num_ngram4.csv"),
                  header = FALSE,
                  stringsAsFactors = FALSE,
                  col.names = c("W1", "W2", "W3", "W4", "frequency")) %>% arrange(desc(frequency))
    
    five <- fread(input = paste0(table_dir, "/p_num_ngram5.csv"),
                  header = FALSE,
                  stringsAsFactors = FALSE,
                  col.names = c("W1", "W2", "W3", "W4", "W5", "frequency")) %>% arrange(desc(frequency))
    
    list(dict = dict, two = two, three = three, four = four, five = five)
}

# Cuts unknown words from the given token list.
# All words before the unknown word are also discarded.
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
# Assumes given LUTs are ordered by frequency.
guess_word <- function(in.phrase, look.up.tables) {
    lambda <- 0.4
    l <- 1
    
    tok_phrase <- (tokenize_sentences(uniformize_punct(in.phrase)))[[1]]
    #message("Tokenized phrase: ", paste0(tok_phrase, collapse="_"))
    
    s <- max(1, length(tok_phrase)-3)
    e <- length(tok_phrase)
    ng <- length(tok_phrase) - s+2 
    
    match_cand <- sapply(tok_phrase[s:e], function(t) {
        z <- look.up.tables$dict[look.up.tables$dict$word == t, ]$index
        if (identical(z, integer(0))) {
            NA
        } else {
            z
        }
    })
    #message("Match cand: ", paste0(match_cand, collapse = '_'))
    
    match_cand <- cut_unknown(match_cand)
    #message("Match cand, cut: ", paste0(match_cand, collapse = '_'))
    
    rm(tok_phrase)
    
    ng <- length(match_cand)
    cand_lst <- list()
    score <- list()
    
    if (ng == 4) {
        cands <- filter(look.up.tables$five, 
                        W1 == match_cand[1],
                        W2 == match_cand[2],
                        W3 == match_cand[3],
                        W4 == match_cand[4])
        
        #message("  ...found ", dim(cands)[1], " candidates at ", ng+1)
        
        if (dim(cands)[1] > 0) {
            # Take the top 5 candidates
            cands <- cands[1:(min(5, dim(cands)[1])), ]
            # Find the frequency of the matching 4-gram
            cnt.four_g <- filter(look.up.tables$four,
                                 W1 == match_cand[1],
                                 W2 == match_cand[2],
                                 W3 == match_cand[3],
                                 W4 == match_cand[4])$frequency
            
            # Calculate the score for the candidate match
            for (i in seq(1, dim(cands)[1])) {
                cand_lst[length(cand_lst)+1] <- 
                    look.up.tables$dict[look.up.tables$dict$index == cands[i, ]$W5, ]$word
                score[length(score)+1] <- l * cands[i, ]$frequency / cnt.four_g
                
                #message("  ... candidate at ", ng+1, "-gram: ", cand_lst[length(cand_lst)], 
                #        ", score: ", score[length(score)])
            }
        }
        
        # Prepare the next cycle
        l <- l * lambda
        ng <- ng - 1
        match_cand <- match_cand[2:length(match_cand)]
    }
    
    #message("cand_list has ", length(cand_lst), " elements")
    
    if (ng == 3 && length(cand_lst) < 5) {
        cands <- filter(look.up.tables$four,
                        W1 == match_cand[1],
                        W2 == match_cand[2],
                        W3 == match_cand[3]) %>% slice(1:5)
        
        #message("  ...found ", dim(cands)[1], " candidates at ", ng+1)
        
        if (dim(cands)[1] > 0) {
            cands <- cands[1:(min(5, dim(cands)[1])), ]
            cnt.three_g <- filter(look.up.tables$three,
                                 W1 == match_cand[1],
                                 W2 == match_cand[2],
                                 W3 == match_cand[3])$frequency
            
            for (i in seq(1, dim(cands)[1])) {
                c <- look.up.tables$dict[look.up.tables$dict$index == cands[i, ]$W4, ]$word
                
                if (length(cand_lst[cand_lst == c]) == 0) {
                    # candidate word 'c' isn't already picked
                    
                    cand_lst[length(cand_lst)+1] <- c
                    score[length(score)+1] <- l * cands[i, ]$frequency / cnt.three_g
                    
                    #message("  ... candidate at ", ng+1, "-gram: ", cand_lst[length(cand_lst)], 
                    #        ", score: ", score[length(score)])
                }
            }
        }
        
        l <- l * lambda
        ng <- ng - 1
        match_cand <- match_cand[2:length(match_cand)]
    }
    
    #message("cand_list has ", length(cand_lst), " elements")
    
    if (ng == 2 && length(cand_lst) < 5) {
        cands <- filter(look.up.tables$three,
                        W1 == match_cand[1],
                        W2 == match_cand[2]) %>% slice(1:5)
        
        #message("  ...found ", dim(cands)[1], " candidates at ", ng+1)
        
        if (dim(cands)[1] > 0) {
            cands <- cands[1:(min(5, dim(cands)[1])), ]
            cnt.two_g <- filter(look.up.tables$two,
                                  W1 == match_cand[1],
                                  W2 == match_cand[2])$frequency
            
            for (i in seq(1, dim(cands)[1])) {
                c <- look.up.tables$dict[look.up.tables$dict$index == cands[i, ]$W3, ]$word
                
                if (length(cand_lst[cand_lst == c]) == 0) {
                    # candidate word 'c' isn't already picked
                    
                    cand_lst[length(cand_lst)+1] <- c
                    score[length(score)+1] <- l * cands[i, ]$frequency / cnt.two_g
                    
                    #message("  ... candidate at ", ng+1, "-gram: ", cand_lst[length(cand_lst)], 
                    #        ", freq: ", cands[i, ]$frequency, ", cnt:", cnt.two_g,
                    #        ", score: ", score[length(score)])
                }
            }
        }
        
        l <- l * lambda
        ng <- ng - 1
        match_cand <- match_cand[2:length(match_cand)]
    }
    
    #message("cand_list has ", length(cand_lst), " elements")
    
    if (ng == 1 && length(cand_lst) < 5) {
        cands <- filter(look.up.tables$two,
                        W1 == match_cand[1]) %>% slice(1:5)
        
        #message("  ...found ", dim(cands)[1], " candidates at ", ng+1)
        
        if (dim(cands)[1] > 0) {
            cands <- cands[1:(min(5, dim(cands)[1])), ]
            cnt.one_g <- filter(look.up.tables$dict,
                                index == match_cand[1])$frequency
            
            for (i in seq(1, dim(cands)[1])) {
                c <- look.up.tables$dict[look.up.tables$dict$index == cands[i, ]$W2, ]$word
                
                if (length(cand_lst[cand_lst == c]) == 0) {
                    # candidate word 'c' isn't already picked
                    
                    cand_lst[length(cand_lst)+1] <- c
                    score[length(score)+1] <- l * cands[i, ]$frequency / cnt.one_g
                    
                    #message("  ... candidate at ", ng+1, "-gram: ", cand_lst[length(cand_lst)], 
                    #        ", score: ", score[length(score)])
                }
            }
        }
        
        l <- l * lambda
        ng <- ng - 1
        match_cand <- match_cand[2:length(match_cand)]
    }
    
    #message("cand_list has ", length(cand_lst), " elements")
    
    if (ng == 0 && length(cand_lst) < 5) {
        cands <- look.up.tables$dict[1:5, ]
        
        #message("  ...found ", dim(cands)[1], " candidates at ", ng+1)
        
        for (i in seq(1, dim(cands)[1])) {
            c <- cands[i, ]$word
            
            if (length(cand_lst[cand_lst == c]) == 0) {
                # candidate word 'c' isn't already picked
                cand_lst[length(cand_lst)+1] <- c
                score[length(score)+1] <- l * (1 / cands[i, ]$frequency)
                
                #message("  ... candidate at ", ng+1, "-gram: ", cand_lst[length(cand_lst)], 
                #        ", score: ", score[length(score)])
            }
        }
    }
    
    #message("cand_list has ", length(cand_lst), " elements")
    
    message("Stopped at ", length(cand_lst), " candidates, ", ng+1, "-grams.")

    # Return only the overall top 5 candidates
    (data.frame(word = unlist(cand_lst), score = unlist(score)) %>% arrange(desc(score)))[1:5, ]
}

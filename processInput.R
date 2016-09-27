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

#
# Processes the original input into n-gram tables, and exports them to disk.
#

# for reproduceability
set.seed(928374239)


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

# Calculate a document x frequency matrix of n_grams
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

# Exports ngrams to .txt files
export.ngrams <- function(ngrams) {
    
    sapply(seq(1,5), function(g) {
        write.table(col_sums(ngrams[[g]]), file = paste0("ngram", g, ".txt"), col.names = FALSE)    
    })
}


## Start

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

export.ngrams(Ngrams)

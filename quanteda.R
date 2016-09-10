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

simpleLC <- function() {
    sample.pct <- 0.001
    l1 <- sample_lines("final/en_US/en_US.blogs.txt", n = 899288 * sample.pct, nlines = 899288)
    l2 <- sample_lines("final/en_US/en_US.news.txt", n = 1010242 * sample.pct, nlines = 1010242)
    l3 <- sample_lines("final/en_US/en_US.twitter.txt", n = 2360148 * sample.pct, nlines = 2360148)
    
    corpus(c(l1,l2,l3))
}



calcDFM <- function(corpus, n_gram = 1) {
    docFM <- dfm(corpus, 
                 toLower = TRUE,
                 removeNumbers = TRUE,
                 removePunct = TRUE,
                 removeURL = TRUE,
                 removeSeparators = TRUE,
                 language = "english",
                 ngrams = n_gram)
    
    docFM
}

# compacts a corpus' DocumentTermMatrix into a data.table of terms and frequencies, 
# ordered by frequency
calcCorpusFreq <- function(docFM) {
    term_sums <- col_sums(docFM)
    total_features <- sum(term_sums)
    
    dt <- data.table(feature = names(term_sums), frequency = term_sums) %>%
        arrange(desc(frequency)) %>%
        mutate(pct_full = frequency / total_features)
    
    dt$rank <- seq(1:length(dt$frequency))
    
    if (length(unlist(strsplit(dt[1,1], '_'))) == 1) {
        # unigram
        dt <- mutate(dt, Last = "", Pref = "") %>% 
            mutate(Prob = frequency / sum(frequency))
    } else {
        # ngram
        dt$Last <- unname(sapply(dt[, 1], 
                                 function(x) { 
                                     last(unlist(strsplit(x, '_')))
                                 }
                                 )
                          )
        dt$Pref <- unname(sapply(dt[, 1],
                                 function(x) {
                                     s <- unlist(strsplit(x, '_'))
                                     s <- s[1:(length(s)-1)]
                                     paste0(s, collapse = '_')
                                 }))
    }
    
    dt
}

# calculates the MLE probability for a table of n-grams, given
# the table for (n-1)-grams
calcMLEProb <- function(freqN, freqN_1) {
    clc_prob <- function (p, f) {
        
        ret <- list()
        
        for(i in 1:length(p)) {
            n_1_f <- filter(freqN_1, feature == p[i])['frequency']
            r <- 0
            
            if (dim(n_1_f)[1] > 0 && n_1_f[1,1] != 0) {
                 r <- f[i] / n_1_f[1,1]
            }
            
            ret[length(ret) + 1] <- r
            
            # print(paste0("Prefix: ", p[i], 
            #              ", frequency: ", f[i], 
            #              ", n-1 count: ", n_1_f, 
            #              ", prob: ", ret[i]))
        }
        
        unlist(ret)
    }

    freqN <- freqN %>% mutate(Prob = clc_prob(Pref, frequency))
}


# find the indexes (how many words) needed to cover 50% and 90% of all words in the data
findWordCoverages <- function(df) {
    z<-0
    x<-0
    total <- sum(df$frequency)
    mid <- -1
    ninety <- -1
    
    for (i in df$frequency) { 
        x <- x + i 
        if(mid == -1 && x > total/2) { 
            mid <- z
        }
        if (ninety == -1 && x > total * 0.9) {
            ninety <- z
            break
        }
        z <- z+1;
    }
    
    if (ninety == -1) { ninety <- z }
    
    data.table(Coverage = c("50%", "90%", "Total words"),
               Index = c(mid, ninety, total))
}

plot.cFreq.top <- function(cFreq, top.n) {
    g <- ggplot(head(cFreq, top.n), aes(feature, frequency)) + 
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    
    g
}

plot.cFreq.byRank <- function(cFreq, top.n, logScale = FALSE) {
    a <- aes(rank, frequency)
    
    if (logScale) {
        a <- aes(log(rank), log(frequency))
    }
    
    g <- ggplot(head(cFreq, top.n), a) + geom_point()
    
    g
}


corpus <- simpleLC()

corpSummary <- summary(corpus)

cl <- makeCluster(4)
registerDoParallel(cl)

docFM <- foreach(i = 1:4, .combine = c, .packages = "quanteda") %dopar% {
    calcDFM(corpus, i)
}

cFreq <- foreach(i = 1:4, 
                 .packages = c("quanteda", "slam", "data.table", "dplyr")) %dopar% {
    calcCorpusFreq(res[[i]])
}

pr <- foreach(i = 2:4, .packages = "dplyr") %dopar% {
    calcMLEProb(cFreq[[i]], cFreq[[i-1]])
}

stopCluster(cl)

#wrdCov.1gram <- findWordCoverages(cFreq.1gram)
#wrdCov.2gram <- findWordCoverages(cFreq.2gram)
#wrdCov.3gram <- findWordCoverages(cFreq.3gram)
#wrdCov.4gram <- findWordCoverages(cFreq.4gram)


if (!file.exists("allData.Rdata")) {
    save(docFM.1gram, docFM.2gram, docFM.3gram, 
         cFreq.1gram, cFreq.2gram, cFreq.3gram,
         wrdCov.1gram, wrdCov.2gram, wrdCov.3gram,
         corpSummary,
         file = "allData.Rdata")
}

guess_word <- function(phrase, ngrams) {
    toks <- tokenize(toLower(phrase), 
                     removePunct = TRUE,
                     removeURL = TRUE)[[1]]
    
    r <- data.frame(Last = character(), Prob = numeric(), Pref = character())
    
    start.len <- min(length(ngrams), length(toks))
    
    print(toks)
    print(start.len)
    
    for (i in start.len:1) {
        pref <- paste0(toks[(length(toks)-i+1):length(toks)], collapse = '_')
        
        #print(paste0("going to test \'", pref, "\' on iteration ", i))
        
        g <- filter(ngrams[[i]], Pref == pref) %>% select(Last, Prob, Pref)
        
        #print(paste0("Results: ", dim(g)[1]))
        
        r <- rbind(r,g)
    }
    
    arrange(r, desc(Prob), desc(Pref))
}


makePlots <- function() {

    t1 <- plot.cFreq.top(cFreq.1gram, 10)
    t2 <- plot.cFreq.top(cFreq.2gram, 10)
    t3 <- plot.cFreq.top(cFreq.3gram, 10)
    
    grid.arrange(grobs = list(t1, t2, t3), nrow = 3, top = "Frequency of top 10 features (1-,2-,3- grams)")
    
    r1 <- plot.cFreq.byRank(cFreq.1gram, 1000, TRUE)
    r2 <- plot.cFreq.byRank(cFreq.2gram, 1000, TRUE)
    r3 <- plot.cFreq.byRank(cFreq.3gram, 1000, TRUE)
    
    grid.arrange(grobs = list(r1, r2, r3), nrow = 3, top = "1,2,3-gram frequency by rank, log scale")
}


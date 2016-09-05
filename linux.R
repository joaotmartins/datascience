library(LaF)
library(parallel)
library(quanteda)
library(data.table)
library(dplyr)
library(slam)
library(ggplot2)
library(grid)
library(gridExtra)

set.seed(928374239)

setwd("/mnt/Capstone Project")

sampleOriginalFiles <- function() {
  samp.pct <- 0.01
  
  smpl_f <- function(orig_file, dest_file, s_lines, tot_lines) {
    if (! file.exists(dest_file)) {
      s <- sample_lines(orig_file, s_lines, tot_lines)
      writeLines(s, dest_file)
    }
  }
  
  smpl_f("final/en_US/en_US.blogs.txt", "blogs.txt", samp.pct * 899288, 899288)
  smpl_f("final/en_US/en_US.news.txt", "news.txt", samp.pct * 1010242, 1010242)
  smpl_f("final/en_US/en_US.twitter.txt", "tweets.txt", samp.pct * 2360148, 2360148)
}

loadSampleCorpus <- function() {
  samp_files <- textfile("*.txt")
  corpus <- corpus(samp_files)
  
  corpus
}

calcDFM <- function(corpus, n_gram = 1) {
  docFM <- dfm(corpus, 
               toLower = TRUE,
               removeNumbers = TRUE,
               removePunct = TRUE, 
               stem = TRUE, 
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
  
  dt <- data.table(feature = names(term_sums), frequency = term_sums)
  dt <- arrange(dt, desc(frequency))
  dt$feature.by.freq <- factor(dt$feature, levels = dt$feature)
  dt$rank <- seq(1:length(dt$frequency))
  dt <- mutate(dt, pct_full = frequency / total_features)
  
  dt
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
  g <- ggplot(head(cFreq, top.n), aes(feature.by.freq, frequency)) + geom_bar(stat = "identity")
  
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

sampleOriginalFiles()

corpus <- loadSampleCorpus()

cl <- makeCluster(detectCores())

corpSummary <- summary(corpus)

docFM.1gram <- calcDFM(corpus, 1)
docFM.2gram <- calcDFM(corpus, 2)
docFM.3gram <- calcDFM(corpus, 3) 

cFreq.1gram <- calcCorpusFreq(docFM.1gram)
cFreq.2gram <- calcCorpusFreq(docFM.2gram)
cFreq.3gram <- calcCorpusFreq(docFM.3gram)

wrdCov.1gram <- findWordCoverages(cFreq.1gram)
wrdCov.2gram <- findWordCoverages(cFreq.2gram)
wrdCov.3gram <- findWordCoverages(cFreq.3gram)

stopCluster(cl)

# if (!file.exists("allData.Rdata")) {
#   save(docFM.1gram, docFM.2gram, docFM.3gram, 
#        cFreq.1gram, cFreq.2gram, cFreq.3gram,
#        wrdCov.1gram, wrdCov.2gram, wrdCov.3gram,
#        corpSummary,
#        file = "allData.Rdata")
# }

t1 <- plot.cFreq.top(cFreq.1gram, 10)
t2 <- plot.cFreq.top(cFreq.2gram, 10)
t3 <- plot.cFreq.top(cFreq.3gram, 10)

#grid.arrange(grobs = list(t1, t2, t3), nrow = 3, top = "Frequency of top 10 features (1-,2-,3- grams)")

r1 <- plot.cFreq.byRank(cFreq.1gram, 1000, TRUE)
r2 <- plot.cFreq.byRank(cFreq.2gram, 1000, TRUE)
r3 <- plot.cFreq.byRank(cFreq.3gram, 1000, TRUE)

#grid.arrange(grobs = list(r1, r2, r3), nrow = 3, top = "1,2,3-gram frequency by rank, log scale")

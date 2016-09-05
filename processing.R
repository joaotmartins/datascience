library(parallel)
library(parallelsugar)
library(tm)
library(slam)
library(data.table)
library(dplyr)
library(ggplot2)
library(NLP)

# creates a corpus from the original data
loadData <- function () {
  ds <- DirSource("final/en_US")
  enCorp <- PCorpus(ds, 
                   readerControl = list(reader = readPlain, 
                                        language = "en_US", 
                                        load = TRUE),
                   dbControl = list(useDb = TRUE,
                                    dbName = "c:/temp/enUSdb",
                                    dbType = "DB1"))
  
  enCorp
}

# produces a wordcount dataframe given a corpus and its TDM
calcDocumentWordCounts <- function(corpus, tdm) {
  counts <- col_sums(tdm)
  fileNames <- unlist(sapply(names(counts), 
                             function(x) { 
                               meta(crp[meta(crp, "id") == x], "heading") 
                               }
                             )
                      )
  
  names(fileNames) <- NULL
  names(counts) <- NULL
  
  data.frame(file.name = fileNames, word.count = counts)
}

# compacts a corpus' TermDocumentMatrix into a dataframe of terms and frequencies, ordered by frequency
calcTermMatrix <- function(tdm) {
  term_sums <- row_sums(tdm)
  
  df <- data.frame(term = names(term_sums), freq = term_sums)
  df <- arrange(df, desc(freq))
  df$term_freq <- factor(df$term, levels = df$term)
  
  df
}

# find the indexes (how many words) needed to cover 50% and 90% of all words in the data
findWordCoverages <- function(df) {
  z<-0
  x<-0
  total <- sum(df$freq)
  mid <- -1
  ninety <- -1
  
  for (i in df$freq) { 
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
  
  list(total = total, midpoint = mid, ninety = ninety)
}


# returns an n-gram tokenizer
GetNgramTokenizer <- function(n) {
  function(x) {
    unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)
  }
}


setwd("D:/onedrive/OneDrive - Nokia/Training/Coursera/Data Science Specialization/Capstone Project")
crp <- loadData()

#data("crude")
#crp <- crude


#cl <- makeCluster(4)

# Process corpus into terms
tdm <- TermDocumentMatrix(crp, control = list(stemming = TRUE, 
                                              removePunctuation = TRUE))
termMatrix <- calcTermMatrix(tdm)
cts <- calcDocumentWordCounts(crp, tdm)
covPts <- findWordCoverages(termMatrix)
g_plot_simple <- ggplot(filter(termMatrix, freq > 15), aes(x = term_freq, y = freq)) + 
  geom_bar(stat = "identity")



# Process corpus into 2-grams
tdm_2gram <- TermDocumentMatrix(crp, control = list(stemming = TRUE,
                                                    removePunctuation = TRUE,
                                                    tokenize = GetNgramTokenizer(2)))
cts_2gram <- calcDocumentWordCounts(crp, tdm_2gram)
term_2gram <- calcTermMatrix(tdm_2gram)
cov_2gram <- findWordCoverages(term_2gram)
g_plot_2gram <- ggplot(filter(term_2gram, freq > 10), aes(term_freq, freq)) + 
  geom_bar(stat = "identity")

#stopCluster(cl)

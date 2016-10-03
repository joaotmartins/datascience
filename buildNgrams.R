source("tokenize.R")
source("ngrams.R")

#
# Processes the original input into n-gram tables, and exports them to disk.
#

# for reproduceability
set.seed(928374239)

## Start

train_data <- sampleTrainData()
sentences <- make_sentences(train_data)
toks <- tokenize_sentences(uniformize_punct(sentences))
Ngrams <- calculate_ngrams(toks)
Ngrams <- filter_ngrams(Ngrams, c(0,0,2,2,2))
export.ngrams(Ngrams)

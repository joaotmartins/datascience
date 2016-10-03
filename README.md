# Data Science Capstone Project
Repository for the Coursera Data Science Capstone Project

This repository contains the code for an application that predicts the next word given an input 
phrase. It is the capstone project deliverable.

The application is an R shiny app, that can run locally or on shinyapps.io.

The actual implementation uses a 5-gram model together with the "Stupid Backoff" algorithm from
[here](http://www.aclweb.org/anthology/D07-1090.pdf).

## Outline
These are the main artifacts in the repository:

* An intermediate report that simply analyses the source data.
* A final report slideset that summarizes the work.
* A set of pre-processing steps that transform the source data into look-up tables for prediction.
* A shiny application that predicts words based on the stupid backoff algorithm and the LUTs.
* A common tokenization module.

## Intermediate report
The report is contained in the `milestoneReport.Rmd` R markdown file.

## Final slideset
The final slideset is contained in the `finalPresentation.Rpres` file; an accompaning .css file
is also present that tweaks the code display slightly.

## Pre-processing
For efficiency, the pre-processing had to be divided into a chain of R, Unix and Java tools; these 
tools are run in sequence to produce the final prediction tables. All of these tools produce their
outputs under sub-directories of the `work` directory.

1. The `downloadData.R` script pulls the original data and decompresses it into `original`.
1. The `processOriginal.sh` script uses the Unix `split` command to divide the data into train, test
and validation sets, each set into its own folder.
1. The `buildNgrams.R` script takes the data in `train` and constructs the n-gram tables, placing 
them in the `tables` directory, as `ngram[1-5].txt` files.
1. The `wordParser.sh` invokes the `wordParser.jar` Java application to build the final LUTs from 
the n-gram tables, these are output as `p_num_ngram[2-5].csv`, as well as the 
`p_num_ngram_words.csv` that contains the word dictionary.

## Shiny application
The shiny application is contained on the `server.R` and `ui.R` files. It uses the functions in 
`lut_predict.R` to do the actual prediction.

## Common modules
The following modules are used:

* `tokenize.R` is used by the pre-processing and prediction applications. It 
contains the tokenization functions that are applied to both the train data and the inputs provided
for prediction.
* `ngrams.R` contains the functions to load data and build n-gram tables. It is used both for 
pre-processing and in the milestone report.

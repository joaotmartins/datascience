library(LaF)
library(quanteda)
library(data.table)
library(dplyr)
library(slam)
library(ggplot2)
library(grid)
library(gridExtra)

# splits the corpus into train, test and validation sets
splitCorpus <- function(trainPct, testPct, validationPct) {
    if (round((trainPct + testPct + validationPct), 1) != 1.0) {
        stop("Given percentages don't add up to 100%")
    }
    
    
    drs <- c("train", "test", "validation")
    pcts <- c(trainPct, testPct, validationPct)
    crp <- list(list(f.name = "final/en_US/en_US.blogs.txt", f.lines = 899288),
                list(f.name = "final/en_US/en_US.news.txt", f.lines = 1010242),
                list(f.name = "final/en_US/en_US.twitter.txt", f.lines = 2360148))
    
    lapply(drs, function(d) {
        if (! dir.exists(d)) { dir.create(d) }
    })
    
    lapply(crp, function(fs, pts, pcts) {
        c <- file(fs$f.name, "rb")
        
        r <- list()
        
        for (i in 1:3) {
            out <- paste0(pts[i], "/", basename(fs$f.name))
            smp.lines <- round(fs$f.lines * pcts[i], 0)
            rval <- list(set.name = pts[i], f.name = out, f.lines = smp.lines)
            r[[length(r)+1]] <- rval
            
            if (! file.exists(out)) {
                print(out)
                o <- file(out, "wb", encoding = "UTF-8")
                writeLines(readLines(c, n = smp.lines, encoding = "UTF-8", skipNul = TRUE), o)
                close(o)
            }
        }
        
        close(c)
        
        list(f.orig.name = fs$f.name, f.orig.lines = fs$f.lines, sets = r)
        
    },  drs, pcts)
}

# sample a percentage of the train files
sampleTrainFiles <- function(crpDesc, samp.pct = 0.005) {
    
    if (! dir.exists("sample")) {
        dir.create("sample")
    }
    
    lapply(crpDesc, function(c) {
        for(i in 1:3) {
            if (c$sets[[i]]$set.name == "train") {
                tSet <- c$sets[[i]]
                s <- sample_lines(tSet$f.name, tSet$f.lines * samp.pct, tSet$f.lines)
                fname <- paste0("sample/", basename(tSet$f.name))
                if (file.exists(fname)) {
                    unlink(fname)
                }
                writeLines(s, fname)
            }
        }
    })
}

loadSampleCorpus <- function() {
    samp_files <- textfile("sample/*.txt", encoding = "UTF-8")
    corpus <- corpus(samp_files)
    
    corpus
}

corpus.to.ngrams <- function(crp) {
    cs <- tokenize(corpus, what = "sentence", simplify = TRUE)
    a <- paste0('_S_ ', toLower(cs), ' _E_')
    
    r <- foreach(n = 1:3, .packages = "quanteda") %do% {
        dfm(corpus(a),
            what = "word",
            removeNumbers = TRUE,
            removePunct = TRUE, 
            removeURL = TRUE,
            removeSeparators = TRUE,
            ngrams = n)
    }
    r
}

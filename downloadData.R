library(utils)

#
# Downloads and decompresses original data
#

createDir <- function() {
    cond_create <- function(d) {
        if (! dir.exists(d)) {
            dir.create(d)
        } else {
            message(d, " directory exists, skipping creation")
        }
    }
    
    cond_create("originalData")
    cond_create("work")
    cond_create("work/original")
}

pullAndUnzipData <- function() {
    zip_file <- "originalData/Coursera-SwiftKey.zip"
    
    if (! file.exists(zip_file)) {
        download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                      zip_file)
    } else {
        message("File ", zip_file, " exists, skipping download")
    }
    
    unzip(zip_file, files = c("final/en_US/en_US.blogs.txt",
                              "final/en_US/en_US.news.txt",
                              "final/en_US/en_US.twitter.txt"),
          junkpaths = TRUE, exdir = "work/original")
}

createDir()
pullAndUnzipData()

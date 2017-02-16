
### -----------------------------------------------------
### --- Goran S. Milovanovic, Phd
### --- 08 Feb 2017
### --- SmartCat: https://www.smartcat.io/
### --- SmartCat Blog, February.
### --- Topic Models in R: Part 1 LDA w. {maptpx}
### -----------------------------------------------------

### -----------------------------------------------------
### --- Part 1.A: Source Material Web Scraping w. w. {tm.plugin.webmining}
### --- Description: news/media reports on Data Science/
### --- Big Data
### --- List of search phrases: DataSciSearch.csv
### --- Sources from which the search phrases were built:
### --- (1) http://bigdata-madesimple.com/big-data-a-to-zz-a-glossary-of-big-data-terminology/
### --- (2) http://www.datascienceglossary.org/
### -----------------------------------------------------

# - clear all
rm(list = ls())
# - libraries + data
library(tm)
library(tm.plugin.webmining)
searchQueries <- read.csv('DataSciSearch.csv',
                          header = T,
                          check.names = F,
                          stringsAsFactors = F)
searchQueries <- tolower(unique(as.character(searchQueries$SearchPhrase)))

# - scrape Google News w. {tm.plugin.webmining}
googleNews <- list()
# - raw data dir:
setwd(paste0(getwd(),'/rawData'))
for (i in 1:length(searchQueries)) {
  squery <- searchQueries[i]
  # - comment in:
  print(paste("Searching for phrase: ", 
              paste0(as.character(i),"/", as.character(length(searchQueries)), ": ", squery), 
              "---"), 
        sep="")
  # - grab Google News:
  googleNewsSRC <- GoogleNewsSource(squery,
                                    params = list(hl = "en",
                                                  q = squery,
                                                  ie = "utf-8",
                                                  num = 100,
                                                  output = "rss"))
  # -  as {tm} Corpus:
  googleNews <- WebCorpus(googleNewsSRC)
  # - comment found:
  print(paste('FOUND: ',length(googleNews),' documents',sep=""))
  # - store:
  f = file(paste("googleNews_", 
                 as.character(i), 
                 ".RData",
                  sep=""))
  save(googleNews,file=f)
  close(f)
  # - comment out:
  print("And save. Now clear.")
  # - remove:
  rm(googleNews); 
  rm(googleNewsSRC); gc()
  # - comment out:
  print(paste('Completed GoogleNews for Issue:',
              i,
              '=',
              squery,
              sep=' '))
  print("Pausing 10 secs before querying next issue...")
  # - wait:
  Sys.sleep(10)
}

### -----------------------------------------------------
### --- Part 1.B: Collect all files -> {tm} Corpus
### -----------------------------------------------------

# - clear all
rm(list = ls())

# - libraries + data
library(tm)

# - raw data dir
setwd(paste0(getwd(),'/rawData'))
files <- list.files(getwd())

# - {tm} Corpus
dsCorpus <- list()
# Corpus Plain Text Docs Counter
c <- 0
for (i in 1:length(files)) {
  load(files[i])
  if (!(length(googleNews)==0)) {
    for (j in 1:length(googleNews)) {
      c <- c+1
      dsCorpus[[c]] <- googleNews[[j]]
    }
  }
}
rm(googleNews)

# - colect some metadata:
heading <- sapply(dsCorpus, function(x) x$meta$heading)
url <- sapply(dsCorpus, function(x) x$meta$origin)

# - as {tm} Corpus
dsCorpus <- Corpus(VectorSource(dsCorpus))
# - doc level metadata:
meta(dsCorpus, tag="heading", type="local") <- heading
meta(dsCorpus, tag="origin", type="local") <- url

# - Simple duplicate detection: heading data check
wHeading <- which(!(duplicated(unlist(meta(dsCorpus, tag = "heading")))))
dsCorpus <- dsCorpus[wHeading]

# - save {tm} corpus:
setwd("../")
saveRDS(dsCorpus, file = "dsCorpus.Rds")



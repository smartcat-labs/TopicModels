
### -----------------------------------------------------
### --- Goran S. Milovanovic, Phd
### --- 08 Feb 2017
### --- SmartCat: https://www.smartcat.io/
### --- SmartCat Blog, February.
### --- Topic Models in R: Part 1 LDA w. {maptpx}
### -----------------------------------------------------

### -----------------------------------------------------
### --- Part 2: Corpus Pre-Processing w. {tm}
### --- Description: news/media reports on Data Science/
### --- Big Data
### --- Using: dsCorpus.Rds
### --- (obtained from SC_TopicModelsR_Part1.R)
### -----------------------------------------------------

# - clear all
rm(list = ls())

# - libraries + data
library(tm)
library(SnowballC)
library(stringr)
library(snowfall)

# - load corpus
dsCorpus <- readRDS("dsCorpus.Rds")

### -----------------------------------------------------
### --- Part 2.A: Prepare the Terminological Model
### -----------------------------------------------------

# - load the terminological model
termModel <- read.csv("DataSciTermModel.csv",
                      header = T,
                      check.names = F,
                      stringsAsFactors = F)
termNames <- unique(tolower(termModel$Term))
termModel <- termNames
# - stem termModel:
termModel <- unname(sapply(termModel, function(x) {
  x <- strsplit(x, split = "\\s")[[1]]
  x <- wordStem(x, language = "english")
  paste(x, collapse = " ")
}))

### -----------------------------------------------------
### --- Part 2.B: Term-Document Matrix Extraction
### -----------------------------------------------------

# - stematize corpus:
dsCorpus <- tm_map(dsCorpus, stemDocument, language = "english") 

# - the mapping function:
termModelMap <- function(tmod, doc) {
  unlist(lapply(tmod, function(x) {
    str_count(tolower(doc), paste0("\\b", x, "\\b"))
  }))
}

# - initiate cluster:
sfInit(parallel = TRUE, cpus = 2, type="SOCK")
# - export {stringr}:
sfLibrary(stringr)
# - export functions and data:
sfExport("termModel")
sfExport("dsCorpus")
sfExport("termModelMap")
# - call: 
tdMatrix <- sfClusterApplyLB(dsCorpus, function(x) {
  termModelMap(termModel, x$content)
})
# - stop cluster:
sfStop()

# - collect:
tdMatrix <- do.call(rbind, tdMatrix)
colnames(tdMatrix) <- termModel
rownames(tdMatrix) <- unlist(meta(dsCorpus, tag = "heading"))

# - save raw tdMatrix:
write.csv(tdMatrix, file = "tdMatrix_RAW.csv")

# - fix counts for embedded terms:
fixEmbedded <- function(term, tdMat) {
  wTerm <- which(colnames(tdMat) == term)
  wEmb <- sapply(colnames(tdMat), function(x) {
    grepl(paste0("\\b", term, "\\b"), x)
    })
  wEmb <- which(wEmb)
  wEmb <- setdiff(wEmb, wTerm)
  if (length(wEmb) > 1) {
    return(rowSums(tdMat[ ,wEmb]))
    } else if (length(wEmb) == 1) {return(tdMat[ ,wEmb])
      } else {return(rep(0, dim(tdMat)[1]))} 
}

# - initiate cluster:
sfInit(parallel = TRUE, cpus = 2, type="SOCK")
# - export functions and data:
sfExport("termModel")
sfExport("tdMatrix")
sfExport("fixEmbedded")
# - call: 
fixMatrix <- sfClusterApplyLB(termModel, 
                              function(x) {fixEmbedded(x, tdMatrix)})
# - stop cluster:
sfStop()
fixMatrix <- do.call(cbind, fixMatrix)
tdMatrix <- tdMatrix - fixMatrix

### -----------------------------------------------------
### --- Part 2.C: Term-Document Matrix Clean Up + Save
### -----------------------------------------------------

# - light fix for termNames:
wSingle <- which(grepl('^[[:alpha:]]$', termNames))
termNames[wSingle] <- toupper(termNames[wSingle])

# - real term names go back in:
colnames(tdMatrix) <- termNames

# - remove documents w. less than 5 terms:
wDocs <- unname(which(rowSums(tdMatrix) >= 5))
tdMatrix <- tdMatrix[wDocs , ]
# - remove non-occurring terms:
wTerms <- which(colSums(tdMatrix)==0)
tdMatrix <- tdMatrix[ , -wTerms]

# - sort tdMatrix by term frequency:
tdMatrix <- tdMatrix[, names(sort(colSums(tdMatrix), decreasing = T))]

# - save:
write.csv(tdMatrix, file = "tdMatrix.csv")

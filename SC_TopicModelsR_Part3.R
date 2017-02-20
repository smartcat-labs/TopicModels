
### -----------------------------------------------------
### --- Goran S. Milovanovic, Phd
### --- 08 Feb 2017
### --- SmartCat: https://www.smartcat.io/
### --- SmartCat Blog, February.
### --- Topic Models in R: Part 1 LDA w. {maptpx}
### -----------------------------------------------------

### -----------------------------------------------------
### --- Part 3.A: Topic Model w. {maptpx}
### --- Description: news/media reports on Data Science/
### --- Big Data
### --- Using: tdMatrix.csv
### --- (obtained from SC_TopicModelsR_Part2.R)
### -----------------------------------------------------

# - clear all
rm(list = ls())

# - libraries
library(maptpx)
library(ggplot2)
library(igraph)

# - load the term-document matrix:
tdMatrix <- read.csv("tdMatrix.csv",
                     header = T,
                     check.names = F,
                     row.names = 1,
                     stringsAsFactors = F)
tdMatrix <- as.matrix(tdMatrix)

### -----------------------------------------------------
### --- Part 3.A: Topic Models w. {maptpx}
### -----------------------------------------------------

# - ensure reproducibility:
set.seed(10071974)

t1 <- Sys.time()

# - and run on K = seq(2,20) semantic topics:
topicModel <- maptpx::topics(tdMatrix, K = seq(2,20),
                             shape = NULL,
                             initopics = NULL,
                             tol = 0.01,
                             bf = T, kill = 0,
                             ord = TRUE,
                             verb = 2)

t2 <- Sys.time()
# - time diff:
t2-t1

### ------------------------------------------------------------
### --- Part 3.B: Optimal Topic Model Inspection + Visualization
### ------------------------------------------------------------

# - Inspect topic model w. K = optimal number of topics
topicModel$K

# - Inspect log(Bayes Factor) for each model K = 2, 3, ..., 20:
logBF <- as.data.frame(topicModel$BF)
logBF$Topics <- seq(2,20)
colnames(logBF) <- c('logBF', 'Topics')
ggplot(logBF, aes(x = Topics, y = logBF)) +
  geom_path(color = "Darkblue") +
  geom_point(size = 1.5, color = "Darkblue") +
  geom_point(size = .5, color = "White") +
  ggtitle("Log Bayes Factor\nTopic Model vs. Single Topic Null Model") +
  theme_bw() + 
  theme(plot.title = element_text(size = 8)) +
  theme(axis.title.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 8)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(axis.text.y = element_text(size = 8))
  
# - Inspect top 30 important terms in each topic (K = 9):
topTerms <- as.data.frame(apply(topicModel$theta, 2, function(x) {
  names(sort(x, decreasing = T))[1:30]
  }))
colnames(topTerms) <- paste0("Topic_", seq(1,topicModel$K))
write.csv(topTerms, "topicLeadTerms.csv")

topicNames <- c('MathStats', 'Development', 'IoT', 'Cloud',
                'AI', 'Social Media', 'Big Data', 'Databases',
                'BI', 'User Data')

### --- Visualize the intra-topic document similarity structure
# - Document similarity:
docCorrelation <- cor(t(topicModel$omega))
diag(docCorrelation) <- 0
# - document network w. {igraph}
docStruct1 <- apply(docCorrelation, 1, which.max)
docStruct <- data.frame(outgoing = seq(1:length(docStruct1)),
                        incoming1 = docStruct1)
docTopics <- unname(apply(topicModel$omega, 1, which.max))
selectTopic <- 5
docNet <- graph.data.frame(docStruct[which(docTopics == selectTopic), ], 
                           directed = T)
V(docNet)$size <- degree(docNet)*1.25
# - plot w. {igraph}
par(mai=c(rep(0,4)))
plot(docNet,
     vertex.color = "orange",
     vertex.shape = "circle",
     vertex.label = NA,
     vertex.frame.color = "grey",
     edge.width = .75,
     edge.color = "grey",
     edge.arrow.size = 0.15,
     margin = c(rep(0,4)))

### ---  Visualize the inter-topic term similarity structure
# - Term similarity:
termCorrelation <- cor(t(topicModel$theta))
diag(termCorrelation) <- 0
# - document network w. {igraph}
termStruct1 <- apply(termCorrelation, 1, which.max)
termStruct <- data.frame(outgoing = rownames(termCorrelation),
                        incoming1 = rownames(termCorrelation)[termStruct1])
termTopics <- unname(apply(topicModel$theta, 1, which.max))
selectTopic <- 5
termNet <- graph.data.frame(termStruct[which(termTopics == selectTopic), ], 
                           directed = T)
V(termNet)$size <- degree(termNet)*1.25
# - plot w. {igraph}
par(mai=c(rep(0,4)))
plot(termNet,
     edge.width = .75,
     edge.color = "darkcyan",
     edge.arrow.size = 0.15,
     vertex.size = 2,
     vertex.color = "white",
     vertex.label.color = "black",
     vertex.label.font = 1,
     vertex.label.family = "sans",
     vertex.label.cex = .55,
     vertex.label.dist = .25,
     vertex.label.dist = .45,
     edge.curved = 0.5,
     margin = c(rep(0,4)))



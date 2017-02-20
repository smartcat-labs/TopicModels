# TopicModels

Description: using {maptpx} to perform fast topic modeling in R.
=================================================================

Files:

**Data sets**

+ *DataSciSearch.csv*
Google News search queries.

+ *DataSciTermModel.csv*
A terminological model.

+ *tdMatrix_RAW.csv*
Raw term-document matrix.

+ *tdMatrix.csv*
Clean term-document matrix.

+ *dsCorpus.Rds*
{tm} Corpus as Rds file.

**R**

+ *SC_TopicModelsR_Part1.R*
Scrapes Google News results obtained from querying from {tm.plugin.webmining} with terms in DataSciSearch.csv
Corpus formation takes place here.

+ *SC_TopicModelsR_Part2.R*
Text pre-processing w. {tm} and routines to adjust the counts for embedded terms.
Uses the following terminological model: DataSciTermModel.csv

+ *SC_TopicModelsR_Part3.R*
Topic models, k = 2,.., 20, w. {maptpx}.
Visualizations w. {ggplot2} and {igraph}

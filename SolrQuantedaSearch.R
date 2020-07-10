library(quanteda)
library(solrium)
library(microbenchmark)

setwd("~/GitHub/TFM-Development")
source(paste0(getwd(), "/modules/multisearch.R"))
source(paste0(getwd(), "/modules/multisearch_solr.R"))

nameCorpus <- "Covid5000"
corpusPath <- paste0(getwd(), "/data/corpus_data/", nameCorpus)
corpusTokens <- readRDS(paste0(corpusPath, "/processed/corpus/corpusTokens.rds"))
corpusPagesTokens <- readRDS(paste0(corpusPath, "/processed/corpus/corpusPagesTokens.rds"))

(conn <<- SolrClient$new())

term <- "virus"

delayQuanteda <- system.time(
  resultQuanteda <- multisearch(term, corpusTokens, corpusPagesTokens))

delaySolrComplete <- system.time(
  resultSolrComplete <- multisearch_solr(term, nameCorpus, conn, 0, 100000000))

delaySolrPartial <- system.time(
  resultSolrPartial <- multisearch_solr(term, nameCorpus, conn, 0, 10))

cat(paste("Quanteda - Tiempo (elapsed):", delayQuanteda["elapsed"], "\n"))
cat(paste("Quanteda - Tamaño de resultado", format(object.size(resultQuanteda), units="MB"), "\n"))
cat(paste("Quanteda - Número de resultados:", nrow(resultQuanteda$datatable), "\n"))
cat(paste("Quanteda - La primera página es la página", resultQuanteda$datatable$Page[1], "del documento", resultQuanteda$datatable$Doc[1], "\n"))
cat("\n")
cat(paste("Solr - Tiempo (elapsed):", delaySolrComplete["elapsed"], "\n"))
cat(paste("Solr - Tamaño de resultado", format(object.size(resultSolrComplete), units="MB"), "\n"))
cat(paste("Solr - Número de resultados:", nrow(resultSolrComplete$datatable), "\n"))
cat(paste("Solr - La primera página es la página", resultSolrComplete$datatable$Page[1], "del documento", resultSolrComplete$datatable$Doc[1], "\n"))
cat("\n")
cat(paste("Solr Parcial - Tiempo (elapsed):", delaySolrPartial["elapsed"], "\n"))
cat(paste("Solr Parcial - Tamaño de resultado", format(object.size(resultSolrPartial), units="MB"), "\n"))
cat(paste("Solr Parcial - Número de resultados:", nrow(resultSolrPartial$datatable), "\n"))
cat(paste("Solr Parcial - La primera página es la página", resultSolrPartial$datatable$Page[1], "del documento", resultSolrPartial$datatable$Doc[1], "\n"))

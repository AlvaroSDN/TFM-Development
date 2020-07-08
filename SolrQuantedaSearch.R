library(quanteda)
library(solrium)
library(microbenchmark)

setwd("~/GitHub/TFM-Development")
source(paste0(getwd(), "/modules/multisearch.R"))
source(paste0(getwd(), "/modules/multisearch_solr.R"))

nameCorpus <- "Covid_200"
corpusPath <- paste0(getwd(), "/data/corpus_data/", nameCorpus)
corpus <- readRDS(paste0(corpusPath, "/processed/corpus/corpus.rds"))
corpusTokens <- readRDS(paste0(corpusPath, "/processed/corpus/corpusTokens.rds"))
corpusPagesTokens <- readRDS(paste0(corpusPath, "/processed/corpus/corpusPagesTokens.rds"))
limiteTokens <- 10000000
comparativaResultados <- TRUE

(conn <<- SolrClient$new())

cat("Escriba el término que desee buscar:")
term <- scan("", what = character(), 1)

if(comparativaResultados == TRUE) {
  delayQuanteda <- system.time(
    resultQuanteda <- multisearch(term, corpusTokens, corpusPagesTokens))
  
  delaySolr <- system.time(
    resultSolr <- multisearch_solr(term, nameCorpus, conn, 0, 100000000))
  
  cat(paste("Quanteda - Tiempo (elapsed):", delayQuanteda["elapsed"], "\n"))
  cat(paste("Quanteda - Tamaño de resultado", format(object.size(resultQuanteda), units="MB"), "\n"))
  cat(paste("Quanteda - Número de resultados:", nrow(resultQuanteda$datatable), "\n"))
  cat(paste("Quanteda - La primera página es la página", resultQuanteda$datatable$Page[1], "del documento", resultQuanteda$datatable$Doc[1], "\n"))
  cat("\n")
  cat(paste("Solr - Tiempo (elapsed):", delaySolr["elapsed"], "\n"))
  cat(paste("Solr - Tamaño de resultado", format(object.size(resultSolr), units="MB"), "\n"))
  cat(paste("Solr - Número de resultados:", nrow(resultSolr$datatable), "\n"))
  cat(paste("Solr - La primera página es la página", resultSolr$datatable$Page[1], "del documento", resultSolr$datatable$Doc[1], "\n"))
  
} else if(statistics$NumberTokens <= limiteTokens) {
  delay <- system.time(
    result <- multisearch(term, corpusTokens, corpusPagesTokens))
  
  cat(paste("Tiempo (elapsed):", delay["elapsed"], "\n"))
  cat(paste("Tamaño de resultado", format(object.size(result), units="MB"), "\n"))
  cat(paste("Número de resultados:", nrow(result$datatable), "\n"))
  cat(paste("La primera página es la página", result$datatable$Page[1], "del documento", result$datatable$Doc[1], "\n"))
} else {
  delay <- system.time(
    result <- multisearch_solr(term, nameCorpus, conn, 0, 10))
  
  cat(paste("Tiempo (elapsed):", delay["elapsed"], "\n"))
  cat(paste("Tamaño de resultado", format(object.size(result), units="MB"), "\n"))
  cat(paste("Número de resultados:", nrow(result$datatable), "\n"))
  cat(paste("La primera página es la página", result$datatable$Page[1], "del documento", result$datatable$Doc[1], "\n"))
}

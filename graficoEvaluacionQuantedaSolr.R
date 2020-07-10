library(pdftools)
library(tidyverse)
library(quanteda)
library(solrium)
library(dplyr)
library(microbenchmark)
library(ggplot2)

setwd("~/GitHub/TFM-Development")

#Variables
listMeanTimesQuanteda <- c()
listMeanTimesSolrComplete <- c()
listMeanTimesSolrPartial <- c()

listTokens <- c()
repeticionesBenchmark <- 20
term <- "virus"
query <- paste0('text: "', term, '"')

files <- list.files(path=paste0(getwd(), "/corpus/"), full.names = TRUE, recursive = TRUE)
files <- files[1:12]
(conn <- SolrClient$new())

numberCorpus <- length(files)

for (x in files){
  filepath <- strsplit(x, "/")
  fname <- filepath[[1]][length(filepath[[1]])]
  fname <- substring(fname, 1, nchar(fname)-4)
  cat(fname, "\n")
  
  corpus <- readRDS(x)
  
  mbmQuanteda <- microbenchmark("benchmark" = {kwic(corpus, pattern = term)},
                                times = repeticionesBenchmark)
  mbmSolrPartial <- microbenchmark("benchmark" = {conn$search(fname, params = list(q = query, fl = "file, page, count", start = 0, rows = 10))},
                                   times = repeticionesBenchmark)
  mbmSolrComplete <- microbenchmark("benchmark" = {conn$search(fname, params = list(q = query, fl = "file, page, count", start = 0, rows = 1000000000))},
                                   times = repeticionesBenchmark)
  
  tokens <- as.list(corpus)
  numberTokens <- sum(unlist(lapply(tokens, length)))
  
  listMeanTimesQuanteda <<- c(listMeanTimesQuanteda, mean(mbmQuanteda$time))
  listMeanTimesSolrComplete <<- c( listMeanTimesSolrComplete, mean(mbmSolrComplete$time))
  listMeanTimesSolrPartial <<- c(listMeanTimesSolrPartial, mean(mbmSolrPartial$time))

  listTokens <<- c(listTokens, numberTokens)
  
}


measureType <- c(rep("Media Quanteda", numberCorpus), rep("Media Solr Completa", numberCorpus), rep("Media Solr Parcial", numberCorpus))
tokens <- c(rep(c(listTokens), 3))
times <- c(listMeanTimesQuanteda, listMeanTimesSolrComplete, listMeanTimesSolrPartial)
datosGrafica <- data.frame(measureType, tokens, times)
saveRDS(datosGrafica, paste0(getwd(), "/datosComparativa.rds"))

# EN EL CASO DE QUE YA SE DISPONGAN DE DATOS:
datosGrafica <- readRDS(paste0(getwd(), "/datosComparativa.rds"))

ggplot(datosGrafica, aes(x=tokens, y=(times/1000000000), group = measureType, colour = measureType )) + 
  geom_line(size = 2)  + 
  geom_point( size=3, shape=21, fill="black") + 
  geom_text(size = 7, aes(label= paste0(substring((times/1000000000), 0, 5), " s"), hjust=0.1, vjust=1.4)) +
  theme_light() +
  theme(text = element_text(size=32), plot.title = element_text(hjust = 0.5)) +
  labs(x = "Número de tokens", y = "Tiempo de respuesta (segundos)", colour = "Tipo:")

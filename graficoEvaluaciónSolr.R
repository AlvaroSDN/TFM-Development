library(tidyverse)
library(solrium)
library(dplyr)
library(microbenchmark)
library(ggplot2)

setwd("~/GitHub/TFM-Development")

#Variables
listMeanTimes <- c()
listMinTimes <- c()
listMaxTimes <- c()
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
  
  mbm <- microbenchmark("benchmark" = {conn$search(fname, params = list(q = query, fl = "file, page, count", start = 0, rows = 10))},
                        times = repeticionesBenchmark)
  
  tokens <- as.list(corpus)
  numberTokens <- sum(unlist(lapply(tokens, length)))
  
  listMeanTimes <<- c(listMeanTimes, mean(mbm$time))
  listMinTimes <<- c(listMinTimes, min(mbm$time))
  listMaxTimes <<- c(listMaxTimes, max(mbm$time))
  listTokens <<- c(listTokens, numberTokens)
  
}

# listMeanTimes <- paste0(substring((listMeanTimes/1000000000), 0, 5), " s")
# listMinTimes <- paste0(substring((listMinTimes/1000000000), 0, 5), " s")
# listMaxnTimes <- paste0(substring((listMaxnTimes/1000000000), 0, 5), " s")

measureType <- c(rep("Tiempo medio", numberCorpus), rep("Tiempo mínimo", numberCorpus), rep("Tiempo máximo", numberCorpus))
tokens <- c(rep(c(listTokens), 3))
times <- c(listMeanTimes, listMinTimes, listMaxTimes)
datosGrafica <- data.frame(measureType, tokens, times)
saveRDS(datosGrafica, paste0(getwd(), "/datosEvaluacionSolr.rds"))

# EN EL CASO DE QUE YA SE DISPONGAN DE DATOS:
datosGrafica <- readRDS(paste0(getwd(), "/datosEvaluacionSolr.rds"))

ggplot(datosGrafica, aes(x=tokens, y=(times/1000000000), group = measureType, colour = measureType )) + 
  geom_line(size = 2)  + 
  geom_point( size=3, shape=21, fill="black") + 
  theme_light() +
  theme(text = element_text(size=32), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Solr - Tiempos de respuesta en base al tamaño de corpus", x = "Número de tokens", y = "Tiempo de respuesta (segundos)", colour = "Tipo:")


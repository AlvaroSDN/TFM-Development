library(quanteda)
library(microbenchmark)
library(ggplot2) #Lo necesita autoplot

setwd("~/GitHub/TFM-Development")

currentCorpus <- "Airbus"
corpusPath <- paste0(getwd(), "/data/corpus_data/", currentCorpus)
corpus <- readRDS(paste0(corpusPath, "/processed/corpus/corpus.rds"))
corpusTokens <- readRDS(paste0(corpusPath, "/processed/corpus/corpusTokens.rds"))
corpusPagesTokens <- readRDS(paste0(corpusPath, "/processed/corpus/corpusPagesTokens.rds"))

numRepetitions <- 50
term1 <- "page"
term2 <- "aluminium"

delay <- system.time(#Expresión a medir
  result <<- kwic(corpusTokens, pattern = term1)
)
cat(paste0("== Quanteda == Búsqueda del termino: ", term1, "\n"))
cat(paste("Tiempo (elapsed):", delay["elapsed"], "\n"))
cat(paste("Tamaño de resultado", format(object.size(result), units="MB"), "\n"))
cat(paste("Número de resultados:", nrow(result), "\n"))

cat("\n")

delay <- system.time(#Expresión a medir
  result <<- kwic(corpusTokens, pattern = term2)
)
cat(paste0("== Quanteda == Búsqueda del termino: ", term2, "\n"))
cat(paste("Tiempo (elapsed):", delay["elapsed"], "\n"))
cat(paste("Tamaño de resultado", format(object.size(result), units="MB"), "\n"))
cat(paste("Número de resultados:", nrow(result), "\n"))

cat("\n")

cat(paste ("== Benchmark con", numRepetitions, " repeticiones ===\n"))
mbm <- microbenchmark("aircraft" = {kwic(corpusTokens, pattern = term1)},
                      "aluminium" = {kwic(corpusTokens, pattern = term2)},
                      times = numRepetitions
)

ggplot <- autoplot(mbm)#Pinta usando ggplot2
ggplot + 
  labs(title=paste0("Tiempo de respuesta para buscar los términos ", "'", paste0(term1, " | ", term2, collapse="' '"), "'")) + 
  theme(text = element_text(size=25))


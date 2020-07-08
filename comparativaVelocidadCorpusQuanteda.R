library(microbenchmark)
library(ggplot2) #Lo necesita autoplot

setwd("~/GitHub/TFM-Development")

corpus200 <- "Covid_200"
corpus700 <- "Covid_700"
corpus2000 <- "Covid_2000"
corpus5000 <- "Covid_5000"
corpusPath200 <- paste0(getwd(), "/data/corpus_data/", corpus200)
corpusPath700 <- paste0(getwd(), "/data/corpus_data/", corpus700)
corpusPath2000 <- paste0(getwd(), "/data/corpus_data/", corpus2000)
corpusPath5000 <- paste0(getwd(), "/data/corpus_data/", corpus5000)
corpusTokens200 <- readRDS(paste0(corpusPath200, "/processed/corpus/corpusTokens.rds"))
# corpusPagesTokens0 <- readRDS(paste0(corpusPath0, "/processed/corpus/corpusPagesTokens.rds"))
corpusTokens700 <- readRDS(paste0(corpusPath700, "/processed/corpus/corpusTokens.rds"))
# corpusPagesTokens1 <- readRDS(paste0(corpusPath1, "/processed/corpus/corpusPagesTokens.rds"))
corpusTokens2000 <- readRDS(paste0(corpusPath2000, "/processed/corpus/corpusTokens.rds"))
# corpusPagesTokens2 <- readRDS(paste0(corpusPath2, "/processed/corpus/corpusPagesTokens.rds"))
corpusTokens5000 <- readRDS(paste0(corpusPath5000, "/processed/corpus/corpusTokens.rds"))
# corpusPagesTokens3 <- readRDS(paste0(corpusPath3, "/processed/corpus/corpusPagesTokens.rds"))

numRepetitions <- 30
term <- "virus"

delay <- system.time(#Expresión a medir
  result <<- kwic(corpusTokens200, pattern = term)
)

cat(paste0("== Quanteda == Búsqueda del termino '", term, "' en el corpus de ", corpus200, "\n"))
cat(paste("Tiempo (elapsed):", delay["elapsed"], "\n"))
cat(paste("Tamaño de resultado", format(object.size(result), units="MB"), "\n"))
cat(paste("Número de resultados:", nrow(result), "\n"))

cat("\n")

delay <- system.time(#Expresión a medir
  result <<- kwic(corpusTokens700, pattern = term)
)

cat(paste0("== Quanteda == Búsqueda del termino '", term, "' en el corpus de ", corpus700, "\n"))
cat(paste("Tiempo (elapsed):", delay["elapsed"], "\n"))
cat(paste("Tamaño de resultado", format(object.size(result), units="MB"), "\n"))
cat(paste("Número de resultados:", nrow(result), "\n"))

cat("\n")

delay <- system.time(#Expresión a medir
  result <<- kwic(corpusTokens2000, pattern = term)
)

cat(paste0("== Quanteda == Búsqueda del termino '", term, "' en el corpus de ", corpus2000, "\n"))
cat(paste("Tiempo (elapsed):", delay["elapsed"], "\n"))
cat(paste("Tamaño de resultado", format(object.size(result), units="MB"), "\n"))
cat(paste("Número de resultados:", nrow(result), "\n"))

cat("\n")

delay <- system.time(#Expresión a medir
  result <<- kwic(corpusTokens5000, pattern = term)
)

cat(paste0("== Quanteda == Búsqueda del termino '", term, "' en el corpus de ", corpus5000, "\n"))
cat(paste("Tiempo (elapsed):", delay["elapsed"], "\n"))
cat(paste("Tamaño de resultado", format(object.size(result), units="MB"), "\n"))
cat(paste("Número de resultados:", nrow(result), "\n"))

cat("\n")

cat(paste ("== Benchmark con", numRepetitions, " repeticiones ===\n"))
mbm <- microbenchmark("200 doc" = {kwic(corpusTokens200, pattern = term)},
                      "700 doc" = {kwic(corpusTokens700, pattern = term)},
                      "2000 doc" = {kwic(corpusTokens2000, pattern = term)},
                      "5000 doc" = {kwic(corpusTokens5000, pattern = term)},
                      times = numRepetitions
)

ggplot <- autoplot(mbm)#Pinta usando ggplot2
ggplot +  
  labs(title="Comparativa de los tiempos de respuesta en corpus de 200, 700, 2000 y 5000 documentos") + 
  theme(text = element_text(size=25))

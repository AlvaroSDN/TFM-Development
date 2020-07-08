library(solrium)
library(microbenchmark)
library(ggplot2) #Lo necesita autoplot

setwd("~/GitHub/TFM-Development")

collection_name200 <- "Covid_200"
collection_name700 <- "Covid_700"
collection_name2000 <- "Covid_2000"
collection_name5000 <- "Covid_5000"
(conn <- SolrClient$new())

numRepetitions <- 30
term <- "virus"
query <- paste0('text: "', term, '"')

delay <- system.time(
  result <<- conn$search(collection_name200, params = list(q = query, fl = "file, page, count", start = 0, rows = 10))
)

cat(paste0("== Solr == Búsqueda del termino '", term, "' en el corpus de ", collection_name200, "\n"))
cat(paste("Tiempo (elapsed):", delay["elapsed"], "\n"))
cat(paste("Tamaño de resultado", format(object.size(result), units="MB"), "\n"))
cat(paste("Número de resultados obtenidos:", nrow(result), "\n"))
cat(paste("Número de resultados totales:", attributes(result)$numFound, "\n"))
cat(paste("La primera página es la página", result$page[1], "del documento", result$file[1], "\n"))

cat("\n")

delay <- system.time(#Expresión a medir
  result <<- conn$search(collection_name700, params = list(q = query, fl = "file, page, count", start = 0, rows = 10))
)

cat(paste0("== Solr == Búsqueda del termino '", term, "' en el corpus de ", collection_name700, "\n"))
cat(paste("Tiempo (elapsed):", delay["elapsed"], "\n"))
cat(paste("Tamaño de resultado", format(object.size(result), units="MB"), "\n"))
cat(paste("Número de resultados obtenidos:", nrow(result), "\n"))
cat(paste("Número de resultados totales:", attributes(result)$numFound, "\n"))
cat(paste("La primera página es la página", result$page[1], "del documento", result$file[1], "\n"))

cat("\n")

delay <- system.time(
  result <<- conn$search(collection_name2000, params = list(q = query, fl = "file, page, count", start = 0, rows = 10))
)

cat(paste0("== Solr == Búsqueda del termino '", term, "' en el corpus de ", collection_name2000, "\n"))
cat(paste("Tiempo (elapsed):", delay["elapsed"], "\n"))
cat(paste("Tamaño de resultado", format(object.size(result), units="MB"), "\n"))
cat(paste("Número de resultados obtenidos:", nrow(result), "\n"))
cat(paste("Número de resultados totales:", attributes(result)$numFound, "\n"))
cat(paste("La primera página es la página", result$page[1], "del documento", result$file[1], "\n"))

cat("\n")

delay <- system.time(
  result <<- conn$search(collection_name5000, params = list(q = query, fl = "file, page, count", start = 0, rows = 10))
)

cat(paste0("== Solr == Búsqueda del termino '", term, "' en el corpus de ", collection_name5000, "\n"))
cat(paste("Tiempo (elapsed):", delay["elapsed"], "\n"))
cat(paste("Tamaño de resultado", format(object.size(result), units="MB"), "\n"))
cat(paste("Número de resultados obtenidos:", nrow(result), "\n"))
cat(paste("Número de resultados totales:", attributes(result)$numFound, "\n"))
cat(paste("La primera página es la página", result$page[1], "del documento", result$file[1], "\n"))

cat("\n")

cat(paste ("== Benchmark con", numRepetitions, " repeticiones ===\n"))
mbm <- microbenchmark("200 doc" = {conn$search(collection_name200, params = list(q = query, fl = "file, page, count", start = 0, rows = 10))},
                      "700 doc" = {conn$search(collection_name700, params = list(q = query, fl = "file, page, count", start = 0, rows = 10))},
                      "2000 doc" = {conn$search(collection_name2000, params = list(q = query, fl = "file, page, count", start = 0, rows = 10))},
                      "5000 doc" = {conn$search(collection_name5000, params = list(q = query, fl = "file, page, count", start = 0, rows = 10))},
                      times = numRepetitions
)

ggplot <- autoplot(mbm)#Pinta usando ggplot2
ggplot + 
  labs(title="Comparativa de los tiempos de respuesta en corpus de 200, 700, 2000 y 5000 documentos") + 
  theme(text = element_text(size=25))


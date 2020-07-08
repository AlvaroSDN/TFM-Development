library(solrium)
library(microbenchmark)
library(ggplot2) #Lo necesita autoplot

setwd("~/GitHub/InnoSpace")

collection_name <- "Airbus"
(conn <- SolrClient$new())

numRepetitions <- 50
term1 <- "aircraft"
term2 <- "aluminium"
query1 <- paste0('text: "', term1, '"')
query2 <- paste0('text: "', term2, '"')

delay <- system.time(
  result <<- conn$search(collection_name, params = list(q = query1, fl = "file, page, count", start = 0, rows = 10000000))
)

cat(paste0("== Solr === Búsqueda del termino: ", term1, "\n"))
cat(paste("Tiempo (elapsed):", delay["elapsed"], "\n"))
cat(paste("Tamaño de resultado", format(object.size(result), units="MB"), "\n"))
cat(paste("Número de resultados obtenidos:", nrow(result), "\n"))
cat(paste("Número de resultados totales:", attributes(result)$numFound, "\n"))
cat(paste("La primera página es la página", result$page[1], "del documento", result$file[1], "\n"))

cat("\n")

delay <- system.time(#Expresión a medir
  result <<- conn$search(collection_name, params = list(q = query2, fl = "file, page, count", start = 0, rows = 100000000))
)

cat(paste0("== Solr === Búsqueda del termino: ", term2, "\n"))
cat(paste("Tiempo (elapsed):", delay["elapsed"], "\n"))
cat(paste("Tamaño de resultado", format(object.size(result), units="MB"), "\n"))
cat(paste("Número de resultados obtenidos:", nrow(result), "\n"))
cat(paste("Número de resultados totales:", attributes(result)$numFound, "\n"))
cat(paste("La primera página es la página", result$page[1], "del documento", result$file[1], "\n"))

cat("\n")

cat(paste ("== Benchmark con", numRepetitions, " repeticiones ===\n"))
mbm <- microbenchmark("aircraft" = {conn$search(collection_name, params = list(q = query1, fl = "file, page, count", start = 0, rows = 10))},
                      "aluminium" = {conn$search(collection_name, params = list(q = query2, fl = "file, page, count", start = 0, rows = 10))},
                      times = numRepetitions
)

ggplot <- autoplot(mbm)#Pinta usando ggplot2
ggplot + 
  labs(title=paste0("Tiempo de respuesta para buscar los términos ", "'", paste0(term1, " | ", term2, collapse="' '"), "'")) + 
  theme(text = element_text(size=25))


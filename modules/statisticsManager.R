
insertStatisticsTab <- function() {
  insertTab(inputId = "inNavbar", tab = tabPanel(`Encoding<-`("Datos estadísticos", "UTF-8"), value = "stat", icon = icon("list"),
                                                 #h2(strong("Datos estadisticos")),
                                                 h5(`Encoding<-`("Información estadística relacionada con el corpus inicial de los documentos.", "UTF-8")),
                                                 br(),
                                                 wellPanel( style = "background: white",
                                                            h4("Metadatos del corpus actual:"),
                                                            dataTableOutput("Metadata")
                                                 ),
                                                 br(),
                                                 tableOutput("stat"),
  ),
  
  
  target = "home", position = c("after"))
}

setStatisticsTable <- function(output, statisticsSession, dtMetadata) {
  output$Metadata = DT::renderDataTable({
    dtMetadata
  })
  
  output$stat <- renderTable({
    getStatisticsDatatable(statisticsSession)
  }, rownames = TRUE, colnames = FALSE)
}

generateStatistics <- function(corpusPagesTokens, corpusTokens) {
  statistics <- list()
  documents <- corpusPagesTokens
  indexPage <- regexpr(pattern = '@Page', documents, fixed = TRUE)
  documents <- substring(documents, 1, indexPage)
  documents <- substring(documents, 1, nchar(documents)-1)
  uniqueDocuments <- unique(documents)
  
  tokens <- as.list(corpusTokens)
  numberTokens <- sum(unlist(lapply(tokens, length)))
  
  statistics["NumberDocuments"] <- as.character(length(uniqueDocuments))
  
  statistics["NumberPages"] <- summary(corpusPagesTokens)[1]
  
  statistics["NumberTokens"] <- as.character(numberTokens)
  
  return(statistics)
}

getStatisticsDatatable <- function(statisticsSession) {
  statistics <- matrix(c(statisticsSession$NumberDocuments, statisticsSession$NumberPages, statisticsSession$NumberTokens), ncol = 1)
  row.names(statistics) <- c (`Encoding<-`("Número de documentos", "UTF-8"), 
                              `Encoding<-`("Número total de páginas", "UTF-8"), `Encoding<-`("Número total de tokens", "UTF-8"))
  
  return(statistics)
}
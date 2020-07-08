library(solrium)
library(pdftools)

setwd("~/GitHub/TFM-Development")

collection_name <- "covid_6000"
(conn <- SolrClient$new())

# corpusDir <- paste0(getwd(), "/data/corpus_data/", collection_name, "/raw/documents")

files <- list.files(path="D:/Documentos/CovidGrafica", full.names = TRUE, recursive = TRUE)
files <- files[1:6000]
numDocuments <- length(files)
indexDocument <- 1

counter <- 0

for(file in files) {
  filepath <- strsplit(file, "/")
  fname <- filepath[[1]][length(filepath[[1]])]
  cat(paste0(indexDocument, " / ", numDocuments, " documentos indexados." ), "\n")

  fileTextInPages <- pdf_text(paste(file,sep = "")) 
  fileNumPages <- length(fileTextInPages)
  file_df <- data.frame(file= rep(fname, fileNumPages),
                        page= 1:fileNumPages,
                        count = (counter + 1):(counter + fileNumPages),
                        text= fileTextInPages,
                        stringsAsFactors=FALSE  #OJO!
  ) 
  conn$add(file_df,  #Envío el data frame a solr
           collection_name #A esta collection
  )
  counter <<- counter + fileNumPages
  indexDocument <<- indexDocument + 1
}

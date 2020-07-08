library(pdftools)
library(tidyverse)
library(solrium)
library(dplyr)
library(extrafont)

setwd("~/GitHub/TFM-Development")

#Variables

path_of_data_raw = "D:/Documentos/CovidGrafica"
muestreo = 500

(conn <- SolrClient$new())

collections <- list.files(path=paste0(getwd(), "/corpus/"), full.names = TRUE, recursive = TRUE, )
files <- list.files(path=path_of_data_raw, full.names = TRUE, recursive = TRUE)

#Se leen los documentos y se estructuran (tarda un poco, de 2 minutos a 30 minutos)
numberDocs = 1
counter <- 0
#Bucle por cada documentos
for (x in files){
  #Obtenemos el nombre del documento
  filepath <- strsplit(x, "/")
  fname <- filepath[[1]][length(filepath[[1]])]
  cat(fname, "\n")
  #Leemos el documento (Lista de paginas (identificador), y su texto asociado)
  
  fileTextInPages <- pdf_text(paste(x,sep = "")) 
  fileNumPages <- length(fileTextInPages)
  file_df <- data.frame(file= rep(fname, fileNumPages),
                        page= 1:fileNumPages,
                        count = (counter + 1):(counter + fileNumPages),
                        text= fileTextInPages,
                        stringsAsFactors=FALSE  #OJO!
  ) 
  
  for(collection in collections) {
    collectionPath <- strsplit(collection, "/")
    collectionName <- collectionPath[[1]][length(collectionPath[[1]])]
    collectionName <- substring(collectionName, 1, nchar(collectionName)-4)
    cat(collectionName, "\n")
    
    conn$add(file_df,  #Envío el data frame a solr
             collectionName #A esta collection
    )
  }
  
 if(numberDocs %% muestreo == 0) {
    print(numberDocs)
    collections <<- collections[-1]
  }
  
  counter <<- counter + fileNumPages
  numberDocs <<- numberDocs + 1
}

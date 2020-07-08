library(pdftools)
library(tidyverse)
library(quanteda)
library(dplyr)
library(extrafont)

setwd("~/GitHub/TFM-Development")

#Variables
corpus_raw = data.frame()
path_of_data_raw = "D:/Documentos/CovidGrafica"
hilosDeProcesamiento = 14
muestreo = 500

# Listamos todos los documentos de la ruta de los datos
files <- list.files(path=path_of_data_raw, full.names = TRUE, recursive = TRUE)

#Se leen los documentos y se estructuran (tarda un poco, de 2 minutos a 30 minutos)
posindex = 1
numberDocs = 1
#Bucle por cada documentos
for (x in files){
  #Obtenemos el nombre del documento
  filepath <- strsplit(x, "/")
  fname <- filepath[[1]][length(filepath[[1]])]
  cat(fname, "\n")
  #Leemos el documento (Lista de paginas (identificador), y su texto asociado)
  pdf_text(paste(x,sep = "")) -> document_text
  
  pageindex = 1
  #Por cada pagina del documento
  for (y in document_text){
    #añadimos los identificadores de cada pagina de un documento, y su texto asocidado en un string
    document <- data.frame("title" = fname, "page" = paste0("Page", pageindex), "pos" = paste0("Pos", posindex), "text" = toString(y), stringsAsFactors = FALSE)
    pageindex <<- pageindex + 1
    posindex <<- posindex + 1
    
    colnames(document) <- c("title", "page", "pos", "text")
    #Añadimos la información al corpus que contiene todos los datos leidos hasta ahora
    corpus_raw <<- rbind(corpus_raw,document) 
    
  }
 
  if(numberDocs %% muestreo == 0) {
    print(x)
    quanteda_options(threads = hilosDeProcesamiento)
    corpus <- corpus(corpus_raw)
    doc_id <- paste(corpus_raw$title, corpus_raw$page, corpus_raw$pos, sep = "@")
    docnames(corpus) <- doc_id
    
    corpusTokens <- tokens(corpus)
    saveRDS(corpusTokens, paste0(getwd(),"/corpus/covid_", numberDocs, ".rds"))
    
  }
  
  numberDocs <<- numberDocs + 1
}

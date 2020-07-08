#-------------------------------------------------------------
# Function: multisearch
#
# Search terms in quanteda corpus and returns the needed
# info to plot. In multi terms, it will do the intersection
# of searchs.
# ------------------------------------------------------------
# inputs:
#   - x: term to search.
#   - quantok: doc corpus.
#   - quantokPages: pages corpus.
#
# outputs:
#   - result: datatable with required info and kawick result.
#-------------------------------------------------------------

library(dplyr) #Para summarise y group_by
library(rlist) #Para list.names

multisearch <- function(x, quantok, quantokPages) {
  if(length(x) == 0) {
    return -1
  }
  
  all_words <- unique(x)
  result_search <- data.frame()
  
  first_word <- all_words[1]
  result_search <- kwic(quantok, pattern = phrase(first_word))
  result_search <- data.frame("docname" = result_search$docname)
  
  ocurrTable <- summarise(group_by(result_search,docname), length(docname))#dlyr::summarise
  
  if(dim(result_search)[1] == 0) {
    
    datatable <- data.frame("Doc" = "No documents", "Page" = "N/A", "Pos" = "N/A", "Oc" = "N/A", stringsAsFactors = FALSE)
    
    result_norows <- data.frame(
      "docname" = "No documents", 
      "from" = NA,
      "to" = NA,
      "pre" = "NoRows",
      "keyword" = "",
      "post" = "NoRows",
      "pattern" = "",
      stringsAsFactors = FALSE
    )
    
    rownames(result_norows) <- NULL
    attr(result_norows, "ntoken") <- ntoken(quantokPages)
    class(result_norows) <- c("kwic", "data.frame")
    
    return(list("datatable" = datatable, 
                "plot_corpus" = result_norows))
  }
  if(length(all_words) > 1) {
    multiwords <- all_words[2:length(all_words)]
    for(multiword in multiwords) {
      quantokUpdated <- updateCorpus(result_search, quantok, quantokPages)
  
      result_search <- kwic(quantokUpdated, pattern = phrase(multiword))
      result_search <- data.frame("docname" = result_search$docname)
      
      if(dim(result_search)[1] == 0) {
        
        datatable <- data.frame("Doc" = "No documents", "Page" = "N/A", "Pos" = "N/A", "Oc" = "N/A", stringsAsFactors = FALSE)
        
        result_norows <- data.frame(
          "docname" = "No documents", 
          "from" = NA,
          "to" = NA,
          "pre" = "NoRows",
          "keyword" = "",
          "post" = "NoRows",
          "pattern" = "",
          stringsAsFactors = FALSE
        )
        
        rownames(result_norows) <- NULL
        attr(result_norows, "ntoken") <- ntoken(quantokPages)
        class(result_norows) <- c("kwic", "data.frame")
        
        return(list("datatable" = datatable, 
                    "plot_corpus" = result_norows))
        
      }
      #ordena
      ocurrTable <- ocurrTable %>% arrange(docname)
      #resume
      newOcurr <- summarise(group_by(result_search,docname), length(docname))
      #Ordena
      newOcurr <- newOcurr %>% arrange(docname)
      #Selecciona
      definitivo <- ocurrTable[which(ocurrTable$docname %in% newOcurr$docname), ]
      #Presenta
      definitivo <- definitivo %>% arrange(docname)
      #view(definitivo, "definitivo")
      suma <- newOcurr[2] + definitivo[2]
      definitivo[2] <- suma
      rm(ocurrTable)
      ocurrTable <<- definitivo
    }
    #Varios terminos 
    
    datatable <- getDataTable(result_search, ocurrTable)
    
    nrowKwic <- nrow(datatable)
    docnameKwic <- rep("Corpus", nrowKwic)
    posKwic <- as.integer(datatable$Pos)
    keywordKwic <- rep("", nrowKwic)
    prePostKwic <- rep("PrePost", nrowKwic)
    
    result_kwic <- data.frame(
      "docname" = docnameKwic, 
      "from" = posKwic,
      "to" = posKwic,
      "pre" = prePostKwic,
      "keyword" = keywordKwic,
      "post" = prePostKwic,
      "pattern" = keywordKwic,
      stringsAsFactors = FALSE
    )
    
    #result_kwic$from = rep(0, nrow(result_kwic))
    #result_kwic$to = rep(0, nrow(result_kwic))
    #result_kwic$pre = rep("X", nrow(result_kwic))
    #result_kwic$keyword = rep("X", nrow(result_kwic))
    #result_kwic$post = rep("X", nrow(result_kwic))
    #result_kwic$pattern = rep("X", nrow(result_kwic))
    
    rownames(result_kwic) <- NULL
    attr(result_kwic, "ntoken") <- ntoken(quantokPages)
    class(result_kwic) <- c("kwic", "data.frame")
    
    return(list("datatable" = datatable[order(datatable$Oc, decreasing = TRUE),], 
                "plot_corpus" = result_kwic))
  }
  
  #Solo un termino
  result_search <- data.frame(result_search)
  
  datatable <- getDataTable(result_search, ocurrTable)
  
  nrowKwic <- nrow(datatable)
  docnameKwic <- rep("Corpus", nrowKwic)
  posKwic <- as.integer(datatable$Pos)
  keywordKwic <- rep("", nrowKwic)
  prePostKwic <- rep("PrePost", nrowKwic)
  
  result_kwic <- data.frame(
    "docname" = docnameKwic, 
    "from" = posKwic,
    "to" = posKwic,
    "pre" = prePostKwic,
    "keyword" = keywordKwic,
    "post" = prePostKwic,
    "pattern" = keywordKwic,
    stringsAsFactors = FALSE
  )
  
  #result_kwic$from = rep(0, nrow(result_kwic))
  #result_kwic$to = rep(0, nrow(result_kwic))
  #result_kwic$pre = rep("X", nrow(result_kwic))
  #result_kwic$keyword = rep("X", nrow(result_kwic))
  #result_kwic$post = rep("X", nrow(result_kwic))
  #result_kwic$pattern = rep("X", nrow(result_kwic))
  
  rownames(result_kwic) <- NULL
  attr(result_kwic, "ntoken") <- ntoken(quantokPages)
  class(result_kwic) <- c("kwic", "data.frame")
  
  return(list("datatable" = datatable[order(datatable$Oc, decreasing = TRUE),], 
              "plot_corpus" = result_kwic))
}

#-------------------------------------------------------------
# Function: getDatatable
#
# Generates a dataframe containing the document name, page
# number relative to the document, position in the page and 
# number of occurrences in the page.
# ------------------------------------------------------------
# inputs:
#   - dt: dataframe containing the resulting kwic from search.
#   - ocurrences: dataframe containing number of ocurences per 
#     docname.
#
# outputs:
#   - datatable: dataframe with structured search info to display.
#-------------------------------------------------------------

getDataTable <- function(dt, ocurrences) {

  docnameDT <- ocurrences$docname  
  ocurrencesDT <- ocurrences[2]
  
  indexPage <- regexpr(pattern = '@Page', docnameDT, fixed = TRUE)
  documentDT <- substring(docnameDT, 1, indexPage)
  documentDT <- substring(documentDT, 1, nchar(documentDT)-1)
  pagePosDT <- substring(docnameDT, indexPage)
  pagePosDT <- substring(pagePosDT, 6)
  indexPos <- regexpr(pattern = "@Pos", pagePosDT, fixed = TRUE)
  pageDT <- substring(pagePosDT, 1, indexPos)
  pageDT <- substring(pageDT, 1, nchar(pageDT)-1)
  posDT <- substring(pagePosDT, indexPos)
  posDT <- substring(posDT, 5)
  
  datatable <- data.frame("Doc" = documentDT, "Page" = pageDT, "Pos" = posDT, "Oc" = ocurrencesDT, stringsAsFactors = FALSE)
  names(datatable)[4] <- "Oc"
  
  return(datatable)
}


#-------------------------------------------------------------
# Function: updateCorpus
#
# For search with multi terms, this function generates the
# intersection of sucesive searches.
# ------------------------------------------------------------
# inputs:
#   - result: result of the search.
#   - quantok: corpus indexed by document. 
#   - quantokPages: corpus indexed by page.
#
# outputs:
#   - quantokUpdated: reduced corpus formed by the intersection.
#-------------------------------------------------------------

updateCorpus <- function(result, quantok, quantokPages) {
  resultPages <- result$docname
  quantokPages <- list.names(quantok)
  indexPages <- which(quantokPages %in% resultPages)
  quantokUpdated <- quantok[indexPages]
  return(quantokUpdated)
}

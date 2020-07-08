#-------------------------------------------------------------
# Function: multisearch_SOLR
#
# Search terms in solr collection and returns the needed
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


multisearch_solr <- function(x, nameCorpus, conn, start, rows) {
  if(length(x) == 0) {
    return -1
  }
  
  collection_name <- nameCorpus
  
  all_words <- unique(x)
  
  if(length(all_words) == 1) {
    first_word <- all_words[1]
    result_search <- data.frame()
    
    query <- paste0('text: "', first_word, '"')
    result_search <- conn$search(collection_name, params = list(q = query, fl = "file, page, count", start = start, rows = rows))
    
    if(dim(result_search)[1] == 0) {
      
      datatable <- data.frame("Doc" = "No documents", "Page" = "N/A", "Pos" = "N/A", "Oc" = "N/A", stringsAsFactors = FALSE)
      
      # result_norows <- data.frame(
      #   "docname" = "No documents", 
      #   "from" = NA,
      #   "to" = NA,
      #   "pre" = "NoRows",
      #   "keyword" = "",
      #   "post" = "NoRows",
      #   "pattern" = "",
      #   stringsAsFactors = FALSE
      # )
      
      return(list("datatable" = datatable))
    }
    else {
      
      nrow <- nrow(result_search)
      ocurrences <- rep(0, nrow)
      result_search <- data.frame("Doc" = result_search$file, "Page" = as.character(result_search$page), "Pos" = as.character(result_search$count), "Oc" = as.integer(ocurrences), stringsAsFactors = FALSE)
      
      result_search <- data.frame(result_search)
      
      datatable <- result_search
      
      # nrowKwic <- nrow
      # docnameKwic <- rep("Corpus", nrowKwic)
      # posKwic <- as.integer(datatable$Pos)
      # keywordKwic <- rep("", nrowKwic)
      # prePostKwic <- rep("PrePost", nrowKwic)
      # 
      # result_kwic <- data.frame(
      #   "docname" = docnameKwic, 
      #   "from" = posKwic,
      #   "to" = posKwic,
      #   "pre" = prePostKwic,
      #   "keyword" = keywordKwic,
      #   "post" = prePostKwic,
      #   "pattern" = keywordKwic,
      #   stringsAsFactors = FALSE
      # )
      
      #result_kwic$from = rep(0, nrow(result_kwic))
      #result_kwic$to = rep(0, nrow(result_kwic))
      #result_kwic$pre = rep("X", nrow(result_kwic))
      #result_kwic$keyword = rep("X", nrow(result_kwic))
      #result_kwic$post = rep("X", nrow(result_kwic))
      #result_kwic$pattern = rep("X", nrow(result_kwic))
      
      return(list("datatable" = datatable))
    }
  }
  
  else if(length(all_words) > 1) {

    result_search <- data.frame()
    query <- paste0('text:("', all_words[1], '"')
    
    for(word in all_words) {
      query <- paste0(query, ' AND "', word, '"')
    }
    
    query <- paste0(query, ')')
    result_search <- conn$search(collection_name, params = list(q = query, fl = "file, page, count", start = start, rows = rows))
    
    if(dim(result_search)[1] == 0) {
      
      datatable <- data.frame("Doc" = "No documents", "Page" = "N/A", "Pos" = "N/A", "Oc" = "N/A", stringsAsFactors = FALSE)
      
      # result_norows <- data.frame(
      #   "docname" = "No documents", 
      #   "from" = NA,
      #   "to" = NA,
      #   "pre" = "NoRows",
      #   "keyword" = "",
      #   "post" = "NoRows",
      #   "pattern" = "",
      #   stringsAsFactors = FALSE
      # )
      
      return(list("datatable" = datatable))
    }
    else {
      
      nrow <- nrow(result_search)
      ocurrences <- rep(0, nrow)
      result_search <- data.frame("Doc" = result_search$file, "Page" = as.character(result_search$page), "Pos" = as.character(result_search$count), "Oc" = as.integer(ocurrences), stringsAsFactors = FALSE)
      
      result_search <- data.frame(result_search)
      
      datatable <- result_search
      
      # nrowKwic <- nrow
      # docnameKwic <- rep("Corpus", nrowKwic)
      # posKwic <- as.integer(datatable$Pos)
      # keywordKwic <- rep("", nrowKwic)
      # prePostKwic <- rep("PrePost", nrowKwic)
      # 
      # result_kwic <- data.frame(
      #   "docname" = docnameKwic, 
      #   "from" = posKwic,
      #   "to" = posKwic,
      #   "pre" = prePostKwic,
      #   "keyword" = keywordKwic,
      #   "post" = prePostKwic,
      #   "pattern" = keywordKwic,
      #   stringsAsFactors = FALSE
      # )
      
      #result_kwic$from = rep(0, nrow(result_kwic))
      #result_kwic$to = rep(0, nrow(result_kwic))
      #result_kwic$pre = rep("X", nrow(result_kwic))
      #result_kwic$keyword = rep("X", nrow(result_kwic))
      #result_kwic$post = rep("X", nrow(result_kwic))
      #result_kwic$pattern = rep("X", nrow(result_kwic))
      
      return(list("datatable" = datatable))
    }
  }
  
  
}
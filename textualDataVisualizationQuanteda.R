library(tidyverse)
library(quanteda)
library(rlist)
library(pdftools)
library(extrafont)
library(pryr)
library(dplyr)
library("ggplot2")
theme_set(theme_bw())

setwd("~/GitHub/TFM-Development")

currentCorpus <- "Covid_200"
corpusPath <- paste0(getwd(), "/data/corpus_data/", currentCorpus)
corpus <- readRDS(paste0(corpusPath, "/processed/corpus/corpus.rds"))
corpusTokens <- readRDS(paste0(corpusPath, "/processed/corpus/corpusTokens.rds"))
corpusPagesTokens <- readRDS(paste0(corpusPath, "/processed/corpus/corpusPagesTokens.rds"))

dfm <- dfm(corpus, remove = stopwords("spanish"), remove_punct = TRUE)


# WORDCLOUD
textplot_wordcloud(dfm, min_freq = 6, random_order = FALSE,
                   rotation = .25,
                   colors = RColorBrewer::brewer.pal(8, "Dark2"))


# LEXICAL DISPERSION PLOTS
result <- kwic(corpus, pattern = "Coronavirus")
g <- textplot_xray(result)

g + theme(text = element_text(size=20))

textplot_xray(
  kwic(corpus, pattern = "coronavirus"),
  kwic(corpus, pattern = "vaccine")
)

g <- textplot_xray(
  kwic(corpus, pattern = "coronavirus"),
  kwic(corpus, pattern = "vaccine"),
  kwic(corpus, pattern = "laboratory")
)

g + aes(color = keyword) +
  scale_color_manual(values = c("red", "forestgreen", "blue")) +
  theme(text = element_text(size=20), legend.position = "none") +
  theme(legend.position = "none")

document <- texts(corpus)[12]
names(document) <- summary(corpus)$Text[12]

g <- textplot_xray(
  kwic(document, pattern = "coronavirus"),
  kwic(document, pattern = "Virus"),
  kwic(document, pattern = "vaccine")
)

g + theme(text = element_text(size=20))

# FREQUENCY PLOTS
features <- textstat_frequency(dfm, n = 50)
features$feature <- with(features, reorder(feature, -frequency))

ggplot(features, aes(x = feature, y = frequency,)) +
  geom_point() +
  theme(text = element_text(size=25), axis.text.x = element_text(angle = 90, hjust = 1))

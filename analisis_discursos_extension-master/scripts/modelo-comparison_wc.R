# WordCloud 

library(tidyverse) 
library(tidytext)
library(tm)
library(extrafont)

menta <- c("#179E8D","#A7DCDE","#57BEC3","#FFFD38",
           "#9FE73B", "#E13586","#C226FB","#FC8024")

# WORD FREQUENCY (CON DISCURSO COMPLETO)


stopwords_es <- read_csv("stopwords/stopwords_es.csv") 
stopword_es_char <- data.frame(lapply(stopwords_es, as.character), stringsAsFactors=FALSE)

#sacar <- c("¿", '"', "•","●", "es", "|", "_", "\r\n","º", "ciento", "$", "g")

discursos_texto <- read_csv("data/discursos_texto_limpio.csv") %>% 
 # mutate(txt = removeWords(txt,sacar)) %>% 
  #  filter(!str_detect(discursos,"ASU")) %>%
  #  mutate(txt = removePunctuation(txt),
  #         txt = removeNumbers(txt),
  #         txt = removeWords(txt, stopword_es_char$value),
  #         txt = removeWords(txt, sacar)) %>% 
  group_by(presidente) %>% 
  summarise(txt = paste0(txt, collapse = ", ")) %>% 
  print()

#### opcion 1 ####
#library(reshape2)
#library(tm)
#library(wordcloud)
#
#
#discursos_corpus <- Corpus(VectorSource(discursos_texto$txt))  #limpio la data con la librería tm
##  #tm_map(x = ., FUN = PlainTextDocument) %>%
##  tm_map(x = ., FUN = removePunctuation) %>%
##  tm_map(x = ., FUN = removeNumbers) %>%
##  tm_map(x = ., FUN = removeWords, stopwords(kind = 'es')) %>%
##  tm_map(x = ., FUN = stripWhitespace)
##
#
#document_tm <- TermDocumentMatrix(discursos_corpus) #Creo una matrix (rows = words / columns = words / values = freq count)
#
#document_tm_mat <- as.matrix(document_tm)
#colnames(document_tm_mat) <- unique(discursos_texto$presidente)
#
#head(document_tm_mat)
#
#png("#102_1_comparison_cloud_top_2000_wordsv4.png", width = 600, height = 600)
#comparison.cloud(document_tm_mat,max.words=2000,random.order=FALSE,c(4,0.4), title.size=1.2, colors= menta, title.bg.colors = menta)
#dev.off()

### opcion 2 ####
library(quanteda) # ESTA LIBRERIA AFECTA EL USO DE DPLYR

discursos_texto <- discursos_texto 
corpus <- corpus(discursos_texto,  text_field = "txt")


dfm_corpus <- dfm(corpus,
                  remove = stopwords("spanish"), remove_punct = TRUE, groups = "presidente") %>%
  dfm_trim(min_termfreq = 3)

png("plots/comparisonwc3.png",  width = 1000, height = 1000, bg="black")
#
textplot_wordcloud(dfm_corpus, comparison = TRUE, max_words = 1500, font = "AvenirNext LT Pro Bold",max_size = 1.5, labelsize = 2, labelcolor = 'white',
                   color = menta, random_order = FALSE, bg="black")
#
dev.off()
#


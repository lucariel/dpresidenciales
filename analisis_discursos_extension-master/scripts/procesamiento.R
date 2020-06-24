#######
#######    Script exploratorio incial
#######   


# Cargo librerias 

library(tidyverse)
library(tidytext)
library(tm)
library(lubridate)
library(ngram)


# Importo nombres de archivos de discursos 

all_txts <- list.files(pattern = ".txt$", path = "discursos/") 


setwd("discursos/")   #### CREO CORPUS CON PALABRAS DE DISCURSOS

discursos <- map_df(all_txts, ~ data_frame(txt = read_file(.x)) %>%
         mutate(filename = basename(.x)) %>%
         unnest_tokens(word, txt))

setwd("../")

### GUARDO CORPUS EN OBJETO NUEVO
write_csv(discursos, "data/corpus_discursos.csv")

# IMPORTO STOP WORDS (las que usamos para el buzz, tm y de rladiesmex)

stopwords_es <- read_csv("stopwords/stopwords_es.csv") 

palabras_comunes <- c("hoy","ciento","argentina","argentinos","hoy", ":","º","año", "años", "quiero", "sigue", "además","día","dia","días","días", "mil","millones", "honorable", "¿", '"', "•","●", "es", "|", "_", "pais","país",
                      "señores","senadores","diputados") 

# Importo CORPUS Y ORDENO PARA CONTEO ABSOLUTO DE PALABRAS POR DISCURSO

corpus_completo <- read_csv("data/corpus_discursos.csv") %>% #1,4M  
  transmute(discurso = str_sub(filename, 1, nchar(filename)-4), 
            word, 
            presidente = str_sub(filename, 6, nchar(filename)-4), 
            year = str_sub(filename, 1, 4)) %>% 
  print()


write_csv(corpus_completo, "data/corpus_discursos_completo.csv")


# LIMPIO CORPUS

corpus <- read_csv("data/corpus_discursos.csv") %>%  #1,4M
  transmute(discurso = str_sub(filename, 1, nchar(filename)-4), 
    word, 
    presidente = str_sub(filename, 6, nchar(filename)-4), 
    year = str_sub(filename, 1, 4)) %>% 
  anti_join(stopwords_es,by = c("word"="value")) %>% #saco los stopwords #673,4K
  anti_join(as_tibble(palabras_comunes), by = c("word"="value")) %>% #653,4K
  mutate(word= removePunctuation(word),  # saco punctuation
         number = as.numeric(word),     # creo una nueva variable para sacar los números  
         ncharacters = nchar(word)) %>%  # creo una nueva variable para sacar las palabras con pocos caracteres
  filter(is.na(number)) %>%  # filtro los que no son números  #633,7K 
  filter(!ncharacters==1 ) %>% # filtro los que tienen un único caracter #626,1K
  select(-number, - ncharacters) %>% 
  print()

write_csv(corpus, "data/corpus_discursos_limpio.csv")

corpus %>% 
  group_by(discurso) %>% 
  mutate(palabras_discurso = n()) 

corpus %>% 
  group_by(discurso) %>% 
  mutate(palabras_discurso = n()) %>% 
  select(discurso, presidente, palabras_discurso) %>% 
  distinct() %>% 
  group_by(presidente) %>% 
  mutate(promedio_presidente = mean(palabras_discurso), 
         diferencia = (palabras_discurso - promedio_presidente)/promedio_presidente*100) %>% 
  print(n = Inf)


# TOKEN BY NGRAM
setwd("discursos/")

discursos_bigram <- map_df(all_txts, ~ data_frame(txt = read_file(.x)) %>%
                             mutate(filename = basename(.x)) %>%
                             unnest_tokens(bigram, txt, token = "ngrams", n=2))

setwd("../")           


stopword_es_char <- data.frame(lapply(stopwords_es, as.character), stringsAsFactors=FALSE)

bigrams <- discursos_bigram %>% 
  transmute(discurso = str_sub(filename, 1, nchar(filename)-4), 
            bigram, 
            presidente = str_sub(filename, 6, nchar(filename)-4), 
            year = str_sub(filename, 1, 4)) %>% #1,4M
  mutate(bigram = str_to_lower(bigram),
         bigrams = removeWords(bigram, stopword_es_char$value),
         bigrams = removeWords(bigrams, palabras_comunes),
         bigrams = removePunctuation(bigrams),
         bigrams = removeNumbers(bigrams),
         bigrams = trimws(bigrams), #sacar el espacio de adeltante
         nwords= sapply(strsplit(bigrams, " "), length)) %>% #cantidad de palabras
  filter(!bigrams ==" " & !bigrams =="" & nwords == 2) %>% #222.815
  select(-bigrams, -nwords) %>% 
  print()

write_csv(bigrams,"data/corpus_bigram_limpio.csv")  


#SAVE .CSV CON DISCURSOS NO TOKENIZADOS 

setwd("discursos/")
discursos <- map_df(all_txts, ~ data_frame(txt = read_file(.x)))
setwd("../")

name_files <- all_txts %>% as_tibble() %>% rename("discursos"="value")

discursos_texto <- discursos %>% bind_cols(name_files) 

write_csv(discursos_texto, "data/discursos_texto.csv")

discursos <- read_csv("data/discursos_texto.csv")

discursos_limpio <- discursos %>%
  transmute(discursos = str_sub(discursos, 1, nchar(discursos)-4), 
            txt, 
            presidente = str_sub(discursos, 6, 1000), 
            year = str_sub(discursos, 1, 4)) %>% 
  mutate(txt = str_to_lower(txt),
         txt = removeWords(txt, stopword_es_char$value),
         txt = removeWords(txt, palabras_comunes),
         txt = removePunctuation(txt),
         txt = removeNumbers(txt)) %>%  #cantidad de palabras
  select(-discursos) %>% 
  print()


write_csv(discursos_limpio,"data/discursos_texto_limpio.csv")

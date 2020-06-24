# Pyramid Plot

library(tidyverse)
library(plotrix)

menta <- c("#179E8D","#A7DCDE","#57BEC3","#FFFD38",
           "#9FE73B", "#E13586","#C226FB","#FC8024")

# Inspirados en http://blog.statgraphics.com/wordclouds-0 los gráficos muestran la frecuencia relativa de las palabras que aparecen  más de 20 veces en los discursos de 
# Apertura de Sesiones de MM Y AF (combinados) 

corpus <- read_csv("data/corpus_discursos_limpio.csv")

#COMPARISON MM-16 y AF-20
mm_af <- corpus %>% 
  filter(year == 2016 | year == 2020) %>% 
  group_by(presidente) %>% 
  mutate(total_by_pres = n()) %>% 
  ungroup() %>% 
  group_by(word,presidente) %>% 
  mutate(n_by_pres = n(),
         freq = n_by_pres/total_by_pres*100) %>% 
  arrange(desc(n_by_pres)) %>% 
  ungroup() %>% 
  group_by(word) %>% 
  mutate(n_both = n()) %>% 
  distinct() %>%
  filter(n_both >=20) %>% 
  filter(!word == "queremos") %>% 
  ungroup() %>% 
  print()

mm_speech <- mm_af %>% 
  filter(presidente == "MM") %>% 
  arrange(n_both,word) %>% 
  select(freq) %>%
  unlist() %>% 
  as.vector() %>% 
  print()

af_speech <- mm_af %>% 
  filter(presidente == "AF") %>% 
  arrange(n_both,word) %>% 
  select(freq) %>% 
  unlist() %>% 
  as.vector() %>%
  print()

words <- mm_af %>% 
  arrange(n_both,word) %>% 
  select(word) %>% 
  distinct() %>% 
  unlist() %>% 
  as.vector() %>% 
  print()

pyramid_mm16_af20 <- pyramid.plot(mm_speech,af_speech, laxlab = c(0,0.4,0.8),  top.labels=c("MM-2016","","AF-2020"),raxlab = c(0,0.4,0.8), labels = words,
             space= 0.15, gap  =0.3, labelcex = 1, unit="",lxcol="#A7DCDE", rxcol="#179E8D")

png("plots/pyramid_mm16_af20.png", width = 1000, height = 1000)

pyramid.plot(mm_speech,af_speech, laxlab = c(0,0.4,0.8),  top.labels=c("MM-2016","","AF-2020"),raxlab = c(0,0.4,0.8), labels = words,
                                  space= 0.15, gap  =0.3, labelcex = 2, unit="",lxcol="#E13586", rxcol="#179E8D")

dev.off()

# Comparison MM-19 y AF-20

mm_af2 <- corpus %>% 
  filter(year == 2019 | year == 2020) %>% 
  group_by(presidente) %>% 
  mutate(total_by_pres = n()) %>% 
  ungroup() %>% 
  group_by(word,presidente) %>% 
  mutate(n_by_pres = n(),
         freq = n_by_pres/total_by_pres*100) %>% 
  arrange(desc(n_by_pres)) %>% 
  ungroup() %>% 
  group_by(word) %>% 
  mutate(n_both = n()) %>% 
  distinct() %>%
  filter(n_both >=20) %>% 
  filter(!word == "queremos") %>% 
  ungroup() %>% 
  print()

mm_speech2 <- mm_af2 %>% 
  filter(presidente == "MM") %>% 
  arrange(n_both,word) %>% 
  select(freq) %>%
  unlist() %>% 
  as.vector() %>% 
  print()

af_speech2 <- mm_af2 %>% 
  filter(presidente == "AF") %>% 
  arrange(n_both,word) %>% 
  select(freq) %>% 
  unlist() %>% 
  as.vector() %>%
  print()

words2 <- mm_af2 %>% 
  arrange(n_both,word) %>% 
  select(word) %>% 
  distinct() %>% 
  unlist() %>% 
  as.vector() %>% 
  print()

png("plots/pyramid_mm19_af20.png", width = 1000, height = 1000)

pyramid.plot(mm_speech2,af_speech2, laxlab = c(0,0.4,1),  top.labels=c("MM-2019","","AF-2020"),raxlab = c(0,0.4,1), labels = words2,
             space= 0.15, gap  =0.3, labelcex = 2, unit="",lxcol="#E13586", rxcol="#179E8D")

dev.off()


# Pyramid Plot

library(tidyverse)
library(plotrix)

menta <- c("#179E8D","#A7DCDE","#57BEC3","#FFFD38",
           "#9FE73B", "#E13586","#C226FB","#FC8024")

# Inspirados en http://blog.statgraphics.com/wordclouds-0 los gráficos muestran la frecuencia relativa de las palabras que aparecen  más de 20 veces en los discursos de 
# Apertura de Sesiones de MM Y AF (combinados) 

corpus <- read_csv("data/corpus_discursos_limpio.csv")
comparison_by_two(corpus,2013,1955, T,15)

#COMPARISON MM-16 y AF-20
comparison_by_two <-function(corpus, year.1, year.2, plot = T,nwords=10){ 
  mm_af<-corpus %>% 
    filter(year == year.1 | year == year.2) %>% 
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
    ungroup()  



  mm_speech <- mm_af %>% 
    filter(year == year.1) %>% 
    arrange(n_both,word) %>% 
    select(word,freq,n_both)
    
  af_speech <- mm_af %>% 
    filter(year == year.2) %>% 
    arrange(n_both,word,n_both) %>% 
    select(word,freq) 
  
  colnames(af_speech)[2]<-"y2"
  colnames(mm_speech)[2]<-"y1"
  
  
  if(plot){
    
    merged_speechs_freq<-merge(mm_speech,af_speech) %>% arrange(n_both,word) %>% head(nwords)
    
    words <- merged_speechs_freq$word
    
    pres1 = unique(mm_af[mm_af$year==year.1,]$presidente)
    pres2 = unique(mm_af[mm_af$year==year.2,]$presidente)
    pyramid_mm16_af20 <- pyramid.plot(merged_speechs_freq$y1*10,merged_speechs_freq$y2*10, laxlab = c(0,0.4,0.8),  top.labels=c(paste0(year.1, pres1),"",paste0(year.2, pres2)),raxlab = c(0,0.4,0.8), labels = words,
    space= 0.15, gap  =0.55, labelcex = 1, unit="",lxcol="#A7DCDE", rxcol="#179E8D")
  }
}


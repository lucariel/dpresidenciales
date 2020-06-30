#ESTE SCRIPT ESTÁ BASADO EN 

library(tidyverse)
library(ggplot2)
library(tidytext)
library(tm)
library(ggrepel)
library(extrafont)

CoR<-function(palabra,discurso){
  
}

palabra<-c("nacion","politica","economia","gobierno","democracia","trabajo",
           "estado","pueblo","educación","produccion","crecimiento","sociedad",
           "inversion","justicia","provincias","desarrollo","plan","crisis",
           "programa","instituciones")
alfonsin.CoR.palabras<-c(4.7,4.14,2.36,2.86,3.87,1.18,1.48,1.97,1.22,1.19,
                         0.66,2.08,0.53,1,0.82,1.07,0.67,1.06,0.74,1.32)



font_import(pattern = "Avenir", prompt = F)
menta <- c("#179E8D","#CAFFFF","#A8FF00","#FFFD38",
           "#6CCEFF", "#E13586","#C226FB","#FC8024")

#OPCION 1 ####
# WORD FREQUENCY

discursos_texto <- read_csv("../data/corpus_discursos_completo.csv") 
total_nw_xpresidente<-discursos_texto %>% group_by(year,presidente) %>% summarise(n=n())
crecimiento<-discursos_texto %>% filter(word=='crecimiento') %>% group_by(year,presidente) %>% 
  summarise(w=n()) %>% right_join(total_nw_xpresidente) %>% mutate(
    CoR = w/n*100
  )  
crecimiento$CoR[is.na(crecimiento$CoR)]<-0

crecimiento%>% ggplot(aes(year,CoR))+geom_line(alpha = 0.5)+geom_point()

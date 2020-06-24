# LIBRERIAS ####

library(tidyverse) 
library(tidytext)
library(tm)
library(extrafont)
library(ggforce)
library(patchwork)
library(topicmodels)


font_import(pattern = "Avenir", prompt = FALSE)

menta <- c("#179E8D","#A7DCDE","#57BEC3","#FFFD38",
           "#9FE73B", "#E13586","#C226FB","#FC8024")



# WORD FREQUENCY}
#  (CON DISCURSO COMPLETO)

word_freq_completo <- read_csv("data/corpus_discursos_completo.csv") %>% 
  mutate(word = ifelse(word == "nacion","nación",word),
         word = ifelse(word == "camara","cámara",word),
         word = ifelse(word == "pág","página",word)) %>% 
  group_by(discurso) %>% 
  mutate(total = n()) %>% 
  ungroup() %>% 
  group_by(discurso, word) %>% 
  mutate(n = n()) %>%
  print()



### ts Presidentes plot  ####

#este es conteo sin stopwords
name <- word_freq_completo %>% 
  group_by(presidente) %>% 
  slice(1) %>% 
  arrange(year)%>% 
  select(presidente) %>% 
  ungroup() %>% 
  mutate( presidente = case_when(
    presidente == "CS" ~ "CSM", 
    presidente == "CF" ~ "CFK", 
    T ~ presidente))

start_year <- word_freq_completo %>% group_by(presidente) %>% slice(1) %>% ungroup() %>% select(year) %>% arrange(year)

end_year <- start_year %>% mutate(year = ifelse(year == 1984, 2023,year)) %>% arrange(year)  

df <- data.frame(name = unique(name$presidente),
                 start = unique(start_year$year),
                 end = unique(end_year$year),
                 stringsAsFactors = FALSE) %>% 
  mutate(median_x = start + floor((end-start)/2)) %>%  
  print()


promedio_duracion <- word_freq_completo %>% 
  select(discurso, presidente, year, total)  %>% 
  ungroup() %>% 
  select(discurso, total) %>% 
  distinct() %>% 
  summarise(promedio_discurso = mean(total)) %>% 
  pull()


word_freq_completo %>% 
  select(discurso, presidente, year, total)  %>% 
  distinct() %>% 
  ggplot(aes(year,total)) +
  geom_rect(data=df, aes(NULL,NULL,xmin=start,xmax=end,fill=name),
            ymin=0,ymax=Inf, colour="white", size=0.5, alpha=0.4) +
  scale_fill_manual(values = menta) +
  scale_x_continuous(breaks = seq(from = 1984, to = 2023, by = 1),
                     labels = paste0("'", str_sub(seq(from = 1984, to = 2023, by = 1),3, 4)), 
                     guide = guide_axis(n.dodge = 2))+
  scale_y_continuous(limits = c(0, 30000)) +
  geom_point(size = 2, color = "blue") +
  geom_line(size = 1, alpha = .7, linetype="dotted", color = "blue") +
  geom_hline(yintercept = promedio_duracion, color = "red") +
  geom_vline(xintercept = c(1990,2000,2002, 2004,2008,2016,2020))+
  geom_text(data=df,aes(x=median_x,y=1000,label=name), size=4) +
  theme_minimal() + 
  theme(axis.text = element_text(face="bold", size=10),
        legend.position = 'none', 
        text = element_text(family = "AvenirNext LT Pro Bold")) +
  labs(y = "Cantidad de palabras", 
       x = "", 
       caption = "(la línea roja marca el largo promedio de los discursos)")


#dev.off()
#ggsave(plot = last_plot(), "plots/ts_freq_abs_discursos.png", width = 12, height = 7)


#word_freq plot ####

### WORD FREQ LIMPIO 


word_freq <- read_csv("data/corpus_discursos_limpio.csv")  %>% 
  mutate( presidente = case_when(
    presidente == "CS" ~ "CSM", 
    presidente == "CF" ~ "CFK", 
    T ~ presidente)) %>%
  mutate(total = n(), 
         discurso = paste0(str_sub(discurso, 1, 5), presidente))  %>% 
  ungroup() %>% 
  group_by(discurso, word) %>% 
  mutate(n = n()) %>% 
  distinct() %>% 
  ungroup() %>% 
  bind_tf_idf(word, discurso, n) %>% 
  mutate(n = n()) %>%
  print()



word_freq %>%
  select(discurso, presidente, word,tf) %>% 
  filter(!str_detect(word, "argentin") & !word == "ciento" & !word == "país") %>% 
  group_by(discurso) %>% 
  arrange(desc(tf)) %>%
  slice(1:5)  %>% 
  ungroup() %>% 
  mutate(discurso=as.factor(discurso),
         word = reorder_within(word, tf, discurso)) %>% 
  ggplot(aes(word,tf, fill = presidente)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = menta) +
  labs(x = NULL, y = "") +
  facet_wrap(~discurso, ncol = 4, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  theme(plot.subtitle = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.x = element_blank(), #text(size = 5, color = "white")
        axis.text.y = element_text(size = 9, color = "white"),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill = 'black'),
        strip.background = element_blank(),
        strip.text.x = element_text(size= 10, face = "bold", color = "white"), 
        text = element_text(family = "AvenirNext LT Pro Bold"))
  
dev.off()

ggsave(last_plot(), filename = "word_freqv2.png",
       device = "png", 
       path = "plots/", height = 40,width= 30,units = "cm")

# tf -idf ####

word_freq %>%
  filter(!word == "ciento" & !word=="hi" & !word=="cle" & !word=="ción" & !word=="versión" & !word=="taquigráfica" & !word=="catorce" & !word=="pág" & !word == "nacion" & !word == "camara") %>% 
  group_by(discurso) %>% 
  arrange(desc(tf_idf)) %>%
  slice(1:5)  %>% 
  ungroup() %>% 
  mutate(discurso=as.factor(discurso),
         word = reorder_within(word, tf_idf, discurso)) %>% 
  ggplot(aes(word,tf_idf, fill = presidente)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = menta) +
  labs(x = NULL, y = "") +
  facet_wrap(~discurso, ncol = 4, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  theme(plot.subtitle = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.x = element_blank(), #text(size = 5, color = "white")
        axis.text.y = element_text(size = 9, color = "white"),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill = 'black'),
        strip.background = element_blank(),
        strip.text.x = element_text(size= 10, face = "bold", color = "white"), 
        text = element_text(family = "AvenirNext LT Pro Bold"))


dev.off()

ggsave(last_plot(), filename = "word_tf_idfv3.png",
       device = "png", 
       path = "plots/", height = 40,width= 30,units = "cm")
  

# N-GRAM ####


bigram_freq <- read_csv("data/corpus_bigram_limpio.csv") %>% 
  filter(!str_detect(bigram,"señor") & !str_detect(bigram,"taquigráfica") &!str_detect(bigram,"pág") & !bigram =="asamblea legislativa" & !bigram == "honorable congreso"
         & !bigram == "honorable asamblea" & !bigram == "señores legisladores" & !bigram =="república argentina") %>%
  group_by(discurso) %>% 
  mutate(total = n()) %>% 
  ungroup() %>% 
  group_by(discurso, bigram) %>% 
  mutate(n = n()) %>% 
  distinct() %>% 
  ungroup() %>% 
  bind_tf_idf(bigram, discurso, n) %>% 
  print()

bigram_freq


#word_freq plot   #### TRAE PARES DE PALABRAS POCO INFORMATIVOS. MAJOR USAR TF_IDF

bigram_freq %>%
  select(discurso, presidente, bigram,tf) %>% 
  group_by(discurso) %>% 
  arrange(desc(tf)) %>%
  slice(1:5)  %>% 
  ungroup() %>% 
  mutate(discurso=as.factor(discurso),
         bigram = reorder_within(bigram, tf, discurso)) %>% 
  ggplot(aes(bigram,tf, fill = presidente)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = menta) +
  labs(x = NULL, y = "") +
  facet_wrap(~discurso, ncol = 4, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  theme(plot.subtitle = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.x = element_blank(), #text(size = 5, color = "white")
        axis.text.y = element_text(size = 9, color = "white"),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill = 'black'),
        strip.background = element_blank(),
        strip.text.x = element_text(size= 10, face = "bold", color = "white"), 
        text = element_text(family = "AvenirNext LT Pro Bold"))
#
dev.off()

ggsave(last_plot(), filename = "bigram_freqv2.png",
       device = "png", 
       path = "plots/", height = 40,width= 30,units = "cm")
#tf -idf

bigram_freq %>%
  group_by(discurso) %>% 
  arrange(desc(tf_idf)) %>%
  slice(1:5)  %>% 
  ungroup() %>% 
  mutate(discurso=as.factor(discurso),
         word = reorder_within(bigram, tf_idf, discurso)) %>% 
  ggplot(aes(word,tf_idf, fill = presidente)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = menta) +
  labs(x = NULL, y = "") +
  facet_wrap(~discurso, ncol = 4, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  theme(plot.subtitle = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.x = element_blank(), #text(size = 5, color = "white")
        axis.text.y = element_text(size = 9, color = "white"),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill = 'black'),
        strip.background = element_blank(),
        strip.text.x = element_text(size= 10, face = "bold", color = "white"), 
        text = element_text(family = "AvenirNext LT Pro Bold"))


dev.off()

ggsave(last_plot(), filename = "bigram_tf_idf.png",
       device = "png", 
       path = "plots/", height = 40,width= 30,units = "cm")





#######  STATE OF THE.....  #####
#######  SELECCIONAR PRIMER DISCURSO DE CADA PRESIDENTE
discursos <- word_freq %>% 
  select(discurso, presidente) %>% 
  group_by(presidente) %>% 
  slice(1) %>% 
  distinct() %>% 
  arrange(discurso) %>% 
  print(n = Inf)


# FILTRAMOS LOS PRIMEROS DISCURSOS DE CADA UNO 
primeros <- read_csv("data/corpus_discursos_limpio.csv") %>% 
  mutate( presidente = case_when(
    presidente == "CS" ~ "CSM", 
    presidente == "CF" ~ "CFK", 
    presidente == "FR" ~ "FDR", 
    T ~ presidente)) %>% 
  mutate(primeros = case_when(
      discurso == "1984-RA"  ~ 1,
      discurso == "1990-CSM"  ~ 1,
      discurso == "2000-FDR"  ~ 1,
      discurso == "2003-ED"  ~ 1,
      discurso == "2004-NK"  ~ 1,
      discurso == "2008-CFK"  ~ 1,
      discurso == "2016-MM" ~ 1,
      discurso == "2020-AF" ~ 1, T ~ 0)) %>% 
  filter(primeros == 1) %>%
  select(presidente, year, word) %>% 
  group_by(year) %>% 
  mutate(total = n()) %>% 
  print()

#### LARGO DE LOS PRIMEROS DISCURSOS DE CADA PRESIDNETE
primeros %>% 
  select(presidente, total) %>% 
  distinct()



### EXPLORAR PALABRAS MAS FRECUENTES DE PRIMERAS PRESIDENCIAS
### CLASIFICAR LAS 100 PALABRAS MAS FRECUENTES DE LOS PRIMEROS DISCURSOS PRESIDENCIALES SEGUN EJES 



primeros %>%
  ungroup() %>% 
  group_by(word) %>% 
  mutate(n = n()) %>% 
  arrange(desc(n)) %>% 
  select(word, n) %>% 
  distinct() %>% 
  print(n = 200)



#### Economía ####
economia_10 <- c(
  "empleo", 
  "desarrollo", 
  "crecimiento", 
  "dólares", 
  "producción",  
  "inversión", 
  "pesos", 
  "empresas",
  "deuda", 
  "obras", 
  "industria"
)



data <-   primeros   %>% 
  filter(word %in% economia_10) %>% 
  group_by(year, word) %>% 
  mutate(n = n(), 
         pct_n = n/total*100) %>% 
  ungroup() %>% 
  ungroup() %>% 
  distinct() %>% 
  group_by(year) %>% 
  mutate(economia = sum(n), 
         pct_economia = economia/total*100) %>% 
  arrange(desc(pct_n)) %>% 
  group_by(year, word) %>% 
  slice(1) %>% 
  print(n = Inf)


plot <- data %>% 
  mutate(discurso = paste0(year, "-", presidente)) %>% 
  select(discurso, word, everything()) %>% 
  gather_set_data( 1:2) %>% 
  print(n = Inf)

plot %>% filter(word == "industria")

  parallel_plot <- ggplot(data = plot,aes(x, id = id, split = y, value = pct_n)) +
   geom_parallel_sets(aes(fill = discurso), alpha = 0.3, axis.width = 0.2) +
   geom_parallel_sets_axes(axis.width = 0.3) +
   geom_parallel_sets_labels(colour = "white",angle = 360, size = 5.5,
                             family = "AvenirNext LT Pro Bold") +
    scale_fill_manual(values = menta) +
    theme(plot.subtitle = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.position = 'none',
          axis.text.x = element_blank(), #text(size = 5, color = "white")
          axis.text.y = element_blank(),
          plot.background = element_rect(fill="black", color = "black"),
          panel.background = element_rect(fill = 'black', color = "black"),
          strip.background = element_blank(),
          strip.text.x = element_text(size= 10, face = "bold", color = "white"), 
          text = element_text(family = "AvenirNext LT Pro Bold"), 
          axis.ticks = element_blank()) 

  
dot_plot <- plot %>% 
  ungroup() %>% 
  select(discurso, pct_economia) %>% 
  ggplot(aes(x = 1, y = discurso)) + 
  geom_point(aes(size = pct_economia, color = discurso))+
  geom_text(aes(label = discurso), color = "white", vjust = -3, size = 4.5 ) +
  geom_text(aes(label = paste0(round(pct_economia, 1), "%")),
            color = "black", size = 5,family = "AvenirNext LT Pro Bold") +
  scale_color_manual(values = menta) +
  scale_size_continuous(range = c(12,20))+
  theme(plot.subtitle = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = 'none',
        axis.text.x = element_blank(), #text(size = 5, color = "white")
        axis.text.y = element_blank(),
        plot.background = element_rect(fill="black", color = "black"),
        panel.background = element_rect(fill = 'black', color = "black"),
        strip.background = element_blank(),
        strip.text.x = element_text(size= 10, face = "bold", color = "white"), 
        text = element_text(family = "AvenirNext LT Pro Bold"), 
        axis.ticks = element_blank()) +
  coord_flip()



texto <- ggplot() +
  geom_text(aes(x = 3, y = 3, label = "La frecuencia agregada de las palabras
  más frecuentes referidas a ECONOMÍA  
                representan un ..."), color = "white", size = 9, family = "AvenirNext LT Pro Bold") +
  theme_void() +
  theme(plot.subtitle = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.x = element_blank(), #text(size = 5, color = "white")
        axis.text.y = element_blank(),
        plot.background = element_rect(fill="black", color = "black"),
        panel.background = element_rect(fill = 'black', color = "black"),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size= 10, face = "bold", color = "white"), 
        text = element_text(family = "AvenirNext LT Pro Bold"), 
        axis.ticks = element_blank()) 



texto2 <- ggplot() +
  geom_text(aes(x = 3, y = 3, label = "del total de palabras del primer discurso 
                ante la Asamblea Legislativa"), color = "white", size = 9, family = "AvenirNext LT Pro Bold") +
  theme_void() +
  theme(plot.subtitle = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.x = element_blank(), #text(size = 5, color = "white")
        axis.text.y = element_blank(),
        plot.background = element_rect(fill="black", color = "black"),
        panel.background = element_rect(fill = 'black', color = "black"),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size= 10, face = "bold", color = "white"), 
        text = element_text(family = "AvenirNext LT Pro Bold"), 
        axis.ticks = element_blank()) 



 parallel_plot + (texto / dot_plot / texto2 ) 
 dev.off()
 
 ggsave(plot = last_plot(), "plots/parallel_dot.png", width = 40, height = 20, units = "cm")
 
 
 
 
 
 
 
 
 #### Politica  ####
 politica_10 <- c(
   "pueblo", 
   "nación", 
   "democrácia", 
   "república", 
   "patria", 
   "justicia", 
   "comunidad", 
   "derecho", 
   "diálogo", 
   "libertad")
 
 
 
 data2 <-   primeros   %>% 
   filter(word %in% politica_10) %>% 
   group_by(year, word) %>% 
   mutate(n = n(), 
          pct_n = n/total*100) %>% 
   ungroup() %>% 
   ungroup() %>% 
   distinct() %>% 
   group_by(year) %>% 
   mutate(politica = sum(n), 
          pct_politica = politica/total*100) %>% 
   arrange(desc(pct_n)) %>% 
   group_by(year, word) %>% 
   slice(1) %>% 
   print(n = Inf)
 
 
 plot2 <- data2 %>% 
   mutate(discurso = paste0(year, "-", presidente)) %>% 
   select(discurso, word, everything()) %>% 
   gather_set_data( 1:2) %>% 
   print(n = Inf)
 
 
 parallel_plot <- ggplot(data = plot2,aes(x, id = id, split = y, value = pct_n)) +
   geom_parallel_sets(aes(fill = discurso), alpha = 0.3, axis.width = 0.2) +
   geom_parallel_sets_axes(axis.width = 0.3) +
   geom_parallel_sets_labels(colour = "white",angle = 360, size = 5.5, family = "AvenirNext LT Pro Bold") +
   scale_fill_manual(values = menta) +
   theme(plot.subtitle = element_text(size=12),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         legend.position = 'none',
         axis.text.x = element_blank(), #text(size = 5, color = "white")
         axis.text.y = element_blank(),
         plot.background = element_rect(fill="black", color = "black"),
         panel.background = element_rect(fill = 'black', color = "black"),
         strip.background = element_blank(),
         strip.text.x = element_text(size= 10, face = "bold", color = "white"), 
         text = element_text(family = "AvenirNext LT Pro Bold"), 
         axis.ticks = element_blank()) 
 
 
 dot_plot2 <- plot2 %>% 
   ungroup() %>% 
   select(discurso, pct_politica) %>% 
   ggplot(aes(x = 1, y = discurso)) + 
   geom_point(aes(size = pct_politica, color = discurso))+
   geom_text(aes(label = discurso), color = "white", vjust = -3, size = 4.5,
             family = "AvenirNext LT Pro Bold") +
   geom_text(aes(label = paste0(round(pct_politica, 1), "%")), color = "black", size = 5,
             family = "AvenirNext LT Pro Bold") +
   scale_color_manual(values = menta) +
   scale_size_continuous(range = c(12,20))+
   theme(plot.subtitle = element_text(size=12),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         legend.position = 'none',
         axis.text.x = element_blank(), #text(size = 5, color = "white")
         axis.text.y = element_blank(),
         plot.background = element_rect(fill="black", color = "black"),
         panel.background = element_rect(fill = 'black', color = "black"),
         strip.background = element_blank(),
         strip.text.x = element_text(size= 10, face = "bold", color = "white"), 
         text = element_text(family = "AvenirNext LT Pro Bold"), 
         axis.ticks = element_blank()) +
   coord_flip()
 
 
 
 texto <- ggplot() +
   geom_text(aes(x = 3, y = 3, label = "La frecuencia agregada de las palabras
  más frecuentes referidas a POLÍTICA  
                representan un ..."), color = "white", size = 9, family = "AvenirNext LT Pro Bold") +
   theme_void() +
   theme(plot.subtitle = element_text(size=12),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.position = 'none',
         axis.text.x = element_blank(), #text(size = 5, color = "white")
         axis.text.y = element_blank(),
         plot.background = element_rect(fill="black", color = "black"),
         panel.background = element_rect(fill = 'black', color = "black"),
         panel.border = element_blank(),
         strip.background = element_blank(),
         strip.text.x = element_text(size= 10, face = "bold", color = "white"), 
         text = element_text(family = "AvenirNext LT Pro Bold"), 
         axis.ticks = element_blank()) 
 
 
 
 texto2 <- ggplot() +
   geom_text(aes(x = 3, y = 3, label = "del total de palabras del primer discurso 
                ante la Asamblea Legislativa"), color = "white", size = 9, family = "AvenirNext LT Pro Bold") +
   theme_void() +
   theme(plot.subtitle = element_text(size=12),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.position = 'none',
         axis.text.x = element_blank(), #text(size = 5, color = "white")
         axis.text.y = element_blank(),
         plot.background = element_rect(fill="black", color = "black"),
         panel.background = element_rect(fill = 'black', color = "black"),
         panel.border = element_blank(),
         strip.background = element_blank(),
         strip.text.x = element_text(size= 10, face = "bold", color = "white"), 
         text = element_text(family = "AvenirNext LT Pro Bold"), 
         axis.ticks = element_blank()) 
 
 
 
 parallel_plot + (texto / dot_plot2 / texto2 ) 
 dev.off()
 
 ggsave(plot = last_plot(), "plots/parallel_dot2.png", width = 40, height = 20, units = "cm")
 
 
 
 ### TOPIC MODELLING ####
 
DTM_discursos <-  word_freq %>% 
   cast_dtm(document = discurso, term = word, value =  n)


topicos <- LDA(DTM_discursos, k = 2, control = list(seed = 1710))




discursos_topicos <- tidytext::tidy(topicos, matrix("beta"))


discursos_top_terms <- discursos_topicos %>%
  group_by(topic) %>%
  top_n(50, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

discursos_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()





# DOCUMENTOS
discursos_topicos <- tidytext::tidy(topicos, matrix("gamma"))


discursos_top_terms <- discursos_topicos %>%
  group_by(topic) %>%
  top_n(15, gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma)

discursos_top_terms %>%
  mutate(term = reorder_within(document, gamma, topic)) %>%
  ggplot(aes(document, gamma, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()






 
 
 
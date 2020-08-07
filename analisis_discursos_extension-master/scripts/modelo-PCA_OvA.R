#ESTE SCRIPT ESTÁ BASADO EN 

library(tidyverse)
library(ggplot2)
library(tidytext)
library(tm)
library(ggrepel)
library(extrafont)

font_import(pattern = "Avenir", prompt = F)
menta <- c("#179E8D","#CAFFFF","#A8FF00","#FFFD38",
           "#6CCEFF", "#E13586","#C226FB","#FC8024")

#OPCION 1 ####
# WORD FREQUENCY

discursos_texto <- read_csv("../data/discursos_texto.csv") %>% 
  filter(!str_detect(discursos,"ASU")) %>%
  mutate(discursos = str_sub(discursos,end =-5)) %>% 
  print()

discursos_corpus <- Corpus(VectorSource(discursos_texto$txt)) %>% #limpio la data con la librería tm
  #tm_map(x = ., FUN = PlainTextDocument) %>%
  tm_map(x = ., FUN = removePunctuation) %>%
  tm_map(x = ., FUN = removeNumbers) %>%
  tm_map(x = ., FUN = removeWords, stopwords(kind = 'es')) %>%
  tm_map(x = ., FUN = stripWhitespace)


doc_term <- DocumentTermMatrix(discursos_corpus) #Creo una matrix (rows = documents / columns = words / values = freq count)
doc_term$dimnames$Docs <- discursos_texto$discursos #le ponemos los nombres de los discursos 

# Creo la matriz de tf - idf
tf_idf <- weightTfIdf(m = doc_term, normalize = TRUE)
tf_idf_mat <- as.matrix(tf_idf) 

# K-Means Cluster

#Normalizamos la matrix  porque se basa en Euclidean Distance (y no en Cossine Dissimilarity)
tf_idf_norm <- tf_idf_mat / apply(tf_idf_mat, MARGIN = 1, FUN = function(x) sum(x^2)^0.5)
km_clust <- kmeans(x = tf_idf_norm, centers = 5, iter.max = 25)

tweet_token<-discursos_texto%>%
  unnest_tokens(word, txt)
tweet_token %>% group_by(presidente,clust_id,word) %>% summarise(
  presidente = mode(presidente),
  presidente = mode(clust_id),
  freq = n()
)

#PCA ANALISIS
pca_comp <- prcomp(tf_idf_norm)
pca_rep <- data_frame(txt = discursos_texto$txt,
                      discurso = discursos_texto$discursos,
                      pc1 = pca_comp$x[,1],
                      pc2 = pca_comp$x[,2],
                      clust_id = as.factor(km_clust$cluster))
list.files('./data/')
library(ggrepel)

ggplot(data = pca_rep, mapping = aes(x = pc1, y = pc2, color = clust_id)) +
  #scale_color_manual(values = menta) +
  geom_text_repel(mapping = aes(label = discurso), size = 2, fontface = 'bold') +
  labs(title = 'K-Means Cluster: 5 clusters on PCA Features',
       x = '',
       y = '') +
  theme_minimal()+
  theme(legend.position = 'none',
        axis.text.x = element_blank(),
        axis.text.y=element_blank()) 

dev.off()

ggsave(last_plot(), filename = "pca.png",
       device = "png", 
       path = "plots/")




###### OPCION 2 ####

a <- read_csv("../data/corpus_discursos_limpio.csv") %>% 
  group_by(discurso,word) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  distinct() %>% 
  print()


matrix <- spread(a,word,n)

matrix <- data.frame(sapply(matrix, function(x) ifelse(is.na(x), as.numeric(0), x)))

length.t<-length(matrix[1,])
T<- as.matrix(cbind(matrix[,2:length.t]))

#matrix transpuesta

Tt<-T%*%t(T)

#agregamos nombres de filas y columnas
colnames(Tt)<- matrix[,1]
rownames(Tt)<- matrix[,1]

Tt.pc<- log((Tt+.25)/diag(Tt+.25))

ideal<-prcomp(Tt.pc)

xvar<- ideal$rotation[,1]+.19
yvar<- ideal$rotation[,2]

xvar <- (2*(xvar-min(xvar))/(max(xvar)-min(xvar))-1)
yvar <- (2*(yvar-min(yvar))/(max(yvar)-min(yvar))-1)

x <- as.data.frame(xvar)
y <- as.data.frame(yvar)

names <- a %>% 
  select(discurso) %>% 
  distinct() %>%
  mutate(presidente = str_sub(discurso,start = 6 )) %>% 
  print()

df <- bind_cols(names,x,y)
df1<-df

df1 %>% ggplot(aes(x = xvar, y = yvar,  color = presidente))+geom_point()+
  geom_text_repel(mapping = aes(label = presidente, family = "AvenirNext LT Pro Bold"), size = 3)
  

df %>%
  ggplot(aes(x = xvar, y = yvar,  color = presidente)) +
  geom_text_repel(mapping = aes(label = discurso, family = "AvenirNext LT Pro Bold"), size = 3) +
  coord_fixed() +
  #scale_color_manual(values = menta) +
  labs(subtitle = "PCA - Apertura de Sesiones", x = "",y = "") +
  #  guides(colour=guide_legend(title="Espacio"), size = F) +
  theme(plot.subtitle = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text = element_blank(),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill = 'black')) 

ggsave(last_plot(), filename = "pca_v3.png",
       device = "png", 
       path = "plots/")




df1=pca_rep %>% mutate(
  anio = substr(discurso,1,4),
  presidente = substr(discurso,6,nchar(discurso))
)
colnames(df1)[colnames(df1)=='pc1']='xvar'
colnames(df1)[colnames(df1)=='pc2']='yvar'
df2=df1 %>% group_by(presidente) %>% summarise(
  xvarianza = sd(xvar,na.rm = T),
  yvarianza = sd(yvar,na.rm = T),
  varianza = xvarianza*10*yvarianza*10
) %>% filter(!is.na(varianza))
df3<-rbind(df2 %>% arrange(-varianza) %>% head(3) %>% mutate(cl='mayor_varianza'),df2 %>% arrange(-varianza) %>% tail(3) %>% mutate(cl='menor_varianza'))
df1<-df1 %>% filter(presidente%in%df3$presidente)


plot_presidente_one_vs_all<-function(pres_a_plot,df){
  df1<-df %>% group_by(presidente) %>% summarise(
    xvar = median(xvar),
    yvar = median(yvar)
  ) %>% ungroup()
    
  df_lim = df1[df1$presidente==pres_a_plot,]
  limx1=df_lim$xvar+0.5
  limx2=df_lim$xvar-0.5
  limy1=df_lim$yvar+0.5
  limy2=df_lim$yvar-0.5
  df['anio']=substr(df$discurso,1,4)
  
  
  df1[df1$xvar>limx2 & df1$xvar<limx1 & df1$yvar>limy2 & df1$yvar<limy1,] %>% dplyr::filter(presidente!=pres_a_plot)%>%
    ggplot(aes(x = xvar, y = yvar,  color = presidente))+
    ggtitle(paste("Discursos de \n",pres_a_plot)) +
    geom_text_repel(mapping = aes(label = presidente, family = "AvenirNext LT Pro Bold"), size = 3) +
    coord_fixed() +
    #scale_color_manual(values = menta) +
    #  guides(colour=guide_legend(title="Espacio"), size = F) +
    theme(plot.subtitle = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'none',
          axis.text = element_blank(),
          plot.title = element_text(color="white", size=14, face="bold.italic"),
          plot.background = element_rect(fill="black"),
          panel.background = element_rect(fill = 'black')) +
    geom_point(data=df[df$presidente==pres_a_plot,], aes(xvar,yvar))+  geom_text_repel(data=df[df$presidente==pres_a_plot,] ,mapping = aes(label = anio, family = "AvenirNext LT Pro Bold"), size = 3) 
}
list_plot_presidente_OvA<-list()

## Opcion 1, para tods los discursos
for(i in 1:length(df3$presidente)){
  plot_presidente_OvA<-plot_presidente_one_vs_all(df3$presidente[i],df)
  list_plot_presidente_OvA[[i]]<-plot_presidente_OvA
}
list_plot_presidente_OvA[[6]]

## Opcion 2, para tods con más de N discursos, N=2,en este caso

df2<-df %>% group_by(presidente) %>% summarise(
  xvar = median(xvar),
  yvar = median(yvar),
  n=n()
) %>% filter(n>2)
list_plot_presidente_OvA_2<-list()
for(i in 1:length(df2$presidente)){
  plot_presidente_OvA<-plot_presidente_one_vs_all(df2$presidente[i],df)
  list_plot_presidente_OvA_2[[i]]<-plot_presidente_OvA
}

list_plot_presidente_OvA_2[[2]]

plist<-list_plot_presidente_OvA_2
library(gridExtra)

n <- length(plist)

nCol <- floor(sqrt(n))


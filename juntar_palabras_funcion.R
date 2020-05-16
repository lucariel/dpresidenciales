library(tidyverse)

discursos <- list.files(path ="data/sesiones_ordinarias/",pattern=".*txt") %>% as_tibble()

f <- function(x, output) {
  
  file <- read_file(paste0("data/sesiones_ordinarias/",x))
  file_wo_space <- str_replace_all(file,"\r", " ")
  
  file_wo_space <- str_replace_all(file,"\n", " ")
  
  file_wo_dash <- str_replace_all(file_wo_space,"- ","")
  
  write_lines(file_wo_dash, x) 

  }

a<-apply(discursos, 1,f)
file<-a[[1]]
length(discursos$value)
for(i in 1:113){
  a[[i]] %>% writeLines(discursos$value[i])
}

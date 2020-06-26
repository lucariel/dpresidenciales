# CHEQUEO COLABORATIVO DE BASES DE DATOS ELECTORALES polAr


library(tidyverse)
library(polAr)

show_available_elections() %>% 
  transmute(consulta = paste0(NOMBRE, " ",
                              category, " ", 
                              round, " ", 
                              year)) %>% 
  write_csv("scripts/data_check_collab.csv")

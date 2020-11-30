library(tidyverse)
library(rvest)
library(httr)

# O primeiro passo é pegarmos os dados dos veradores que compõe a Câmara Municipal
# atualmente. São os nomes referentes à 10a Legislatura (2017-2020)

u_ver <- "http://www.camara.rj.gov.br/vereadores_atuais.php?m1=vereadores&m2=ver_atuais&m3=por_nome"

partidos <- read_html(u_ver) %>% 
  xml_find_all("//td[@class='td3']//img") %>% 
  xml_attr("title") %>% 
  str_squish()

foto_vereador <- read_html(u_ver) %>% 
  xml_find_all("//td[@class='td3']//img") %>% 
  xml_attr("src")
  

vereadores <- read_html(u_ver) %>% 
  xml_find_first('//table') %>% 
  html_table(header = TRUE) %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  mutate(partido = partidos,
         foto = foto_vereador)

# Atividade Parlamentar


# Projetos de Lei - 2017 a 2020
# 

# u_atv_par <- "http://www.camara.rj.gov.br/controle_atividade_parlamentar.php?"
# 
# q_prolei <- list(
#   "m1" = "materias_leg",
#   "m2" = "10a_Leg",
#   "m3" = "prolei",
#   "url" =  "http://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/LeiInt?OpenForm"
# )

prolei <- GET("https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/LeiEmentaInt?OpenForm=&Start=1&Count=100&Expand=1")

read_html(prolei) %>% 
  xml_find_all('//table') %>%
  magrittr::extract2(2) %>% 
  html_table(fill = TRUE) %>% 
  janitor::clean_names() %>% 
  select(c("x_2", "ementa", "data_publ", "autor_es")) %>% 
  set_names(c("prolei_num", "ementa", "data_publi", "autores")) %>% 
  as_tibble() %>% 
  slice(-c(1,2))
  
xml_f

?html_table
  
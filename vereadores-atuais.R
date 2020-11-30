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

ver_atuais <- read_html(u_ver) %>% 
  xml_find_first('//table') %>% 
  html_table(header = TRUE) %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  mutate(partido = partidos)

library(tidyverse)
library(rvest)
library(httr)
library(lubridate)

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


# Projeto Emenda à Lei Orgânica
u_proelo <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/EmendaEmentaInt"

# Projeto de Lei Complementar
u_prolec <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/LeiCompEmentaInt"

# Projeto de Lei
u_prolei <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/LeiEmentaInt"

# Projeto de Decreto Legislativo
u_prodele <-  "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/DecretoEmentaInt"

# Projeto de Resolução
# vai pedir uma função própria

# Indicações
u_ind <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/IndEmentaInt"

# Moções
u_moc <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/MocaoEmentaInt"

# Requerimentos de Informação
u_req_info <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/MocaoEmentaInt"

# Requerimentos (atentar que terá que ser parseado do 1:5, porque os resultados
# de 2020 estão divididos no ano 2020 e 1920)
u_req <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/ReqEmentaInt"

# Ofícios (iterar de 1:8)
u_oficio <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/OficioInt"

scrap_cm <- function(materia, index) {
  u_base <- materia
  q_base <- q_prolei <- list(
    "OpenForm" = "",
    "Start" = 1,
    "Count" = 1000,
    "Expand" = index
  )
  get_materia <- GET(u_base, query = q_base)
  read_html(get_materia) %>% 
    xml_find_all('//table') %>%
    magrittr::extract2(2) %>% 
    html_table(fill = TRUE) %>% 
    janitor::clean_names() %>% 
    select(c("x_2", "ementa", "data_publ", "autor_es")) %>% 
    set_names(c("prolei_num", "ementa", "data_publi", "autores")) %>% 
    as_tibble() %>%
    separate_rows(autores, sep = ",") %>% 
    filter(str_detect(ementa, "^[A-Z]")) %>%
    mutate(data_publi = mdy(data_publi))
}


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

# Função para scrappear a página separando por ano. Os índices representam os
# anos em ordem decrescente, ou seja índices de 1 a 4 indicam que estamos buscando
# os resultados de 2020 a 2017.

scrap_cm <- function(materia, index) {
  u_base <- materia
  q_base <- list(
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
    mutate(data_publi = mdy(data_publi),
           ementa = str_squish(ementa),
           autores = str_to_title(autores),
           autores = str_remove(autores, "(Vereador)(a*) "))
}

# Projeto Emenda à Lei Orgânica
u_proelo <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/EmendaEmentaInt"
proelo <- map_dfr(1:4, ~scrap_cm(u_proelo, .x)) %>% 
  mutate(materia = "Projeto Emenda à Lei Orgânica")

# Projeto de Lei Complementar
u_prolec <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/LeiCompEmentaInt"
prolec <- map_dfr(1:4, ~scrap_cm(u_prolec, .x)) %>% 
  mutate(materia = "Projeto de Lei Complementar")

# Projeto de Lei
u_prolei <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/LeiEmentaInt"
prolei <- map_dfr(1:4, ~scrap_cm(u_prolei, .x)) %>% 
  mutate(materia = "Projeto de Lei")
  
# Projeto de Decreto Legislativo
pleg <- function(p_num) {
  u_base <- "http://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/DecretoInt"
  q_base <- list(
    "OpenForm" = "",
    "Start" =  p_num)
  get_res <- GET(u_base, query = q_base)
  read_html(get_res) %>% 
    xml_find_all('//table') %>%
    magrittr::extract2(2) %>% 
    html_table(fill = TRUE) %>% 
    janitor::clean_names() %>% 
    select(c("x", "ementa", "data_publ", "autor_es")) %>% 
    set_names(c("prolei_num", "ementa", "data_publi", "autores")) %>% 
    as_tibble() %>%
    separate_rows(autores, sep = ",") %>% 
    filter(str_detect(ementa, "^[A-Z]")) %>%
    mutate(data_publi = mdy(data_publi),
           ementa = str_squish(ementa),
           autores = str_to_title(autores),
           autores = str_remove(autores, "(Vereador)(a*) "))
}
prodeleg <-  map_dfr(c(1,100,199), ~pleg(.x)) %>% 
  filter(data_publi > ymd(20161231))

# Projeto de Resolução
# vai pedir uma função própria pois ao buscar por ano retorna a página de Projeto de Lei

pres_fun <- function() {
  u_base <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/ResolucaoInt"
  q_base <- list(
    "OpenForm" =  "")
  get_res <- GET(u_base, query = q_base)
  read_html(get_res) %>% 
    xml_find_all('//table') %>%
    magrittr::extract2(2) %>% 
    html_table(fill = TRUE) %>% 
    janitor::clean_names() %>% 
    select(c("x", "x_3", "data_publ", "autor_es")) %>% 
    set_names(c("prolei_num", "ementa", "data_publi", "autores")) %>% 
    as_tibble() %>%
    separate_rows(autores, sep = ",") %>% 
    filter(str_detect(ementa, "^[A-Z]")) %>%
    mutate(data_publi = mdy(data_publi),
           ementa = str_squish(ementa),
           autores = str_to_title(autores),
           autores = str_remove(autores, "(Vereador)(a*) ")) %>% 
    filter(data_publi > ymd(20161231))
}

prores <- pres_fun()

# Indicações
u_ind <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/IndEmentaInt"
indica <- map_dfr(1:4, ~scrap_cm(u_ind, .x)) %>% 
  mutate(materia = "Indicações")

# Moções
u_moc <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/MocaoEmentaInt"
mocoes <- map_dfr(1:4, ~scrap_cm(u_moc, .x)) %>% 
  mutate(materia = "Moções")

# Requerimentos de Informação
u_req_info <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/MocaoEmentaInt"
req_info <- map_dfr(1:4, ~scrap_cm(u_req_info, .x)) %>% 
  mutate(materia = "Requerimentos de Informação")
  
# Requerimentos (atentar que terá que ser parseado do 1:5, porque os resultados
# de 2020 estão divididos no ano 2020 e 1920)
u_req <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/ReqEmentaInt"
requerimentos <- map_dfr(1:5, ~scrap_cm(u_req, .x)) %>% 
  mutate(materia = "Requerimentos")

# Ofícios (iterar de 1:8)
u_oficio <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/OficioInt"
oficios <- map_dfr(1:8, ~scrap_cm(u_oficio, .x)) %>% 
  mutate(materia = "Ofícios") %>% 
  filter(str_detect(prolei_num, "^[0-9]"))

atv_parlam <- bind_rows(indica, mocoes, oficios, prodeleg, proelo, prolec, prolei,
                        prores, req_info, requerimentos)

write_csv(atv_parlam, "atv_parlam.csv")

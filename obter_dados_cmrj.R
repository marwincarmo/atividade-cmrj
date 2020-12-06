# Carregando os pacotes necessários

library(tidyverse)
library(rvest)
library(httr)
library(lubridate)

# Crio uma pasta de saída onde os arquivos rds serão armazenados

#dir.create("out")


# Composição da Câmara ----------------------------------------------------


# O primeiro passo é pegarmos os dados dos veradores que compõe a Câmara Municipal
# atualmente. São os nomes referentes à 10a Legislatura (2017-2020)

u_ver <- "http://www.camara.rj.gov.br/vereadores_atuais.php?m1=vereadores&m2=ver_atuais&m3=por_nome"

# O nome dos partidos e a foto dos veradores não vem na tabela principal.
# Estou pegando separadamente para unir depois ao dataframe com os dados dos vereadores.

partidos <- read_html(u_ver) %>% 
    xml_find_all("//td[@class='td3']//img") %>% 
    xml_attr("title") %>% 
    str_squish()

foto_vereador <- read_html(u_ver) %>% 
    xml_find_all("//td[@class='td1']//img") %>% 
    xml_attr("src")

# Crio um dataframe com os dados da tabela e acrescento as informações faltantes

vereadores <- read_html(u_ver) %>% 
    xml_find_first('//table') %>% 
    html_table(header = TRUE) %>% 
    janitor::clean_names() %>% 
    as_tibble() %>% 
    mutate(partido = partidos,
           foto = foto_vereador,
           vereador = str_to_title(vereador),
           foto = str_replace_all(foto, " ", "%20")) # troca espaço vazio e conserta links

# Salvando a base
write_rds(vereadores, "out/vereadores.rds")                                



# Frequência dos palamentares ---------------------------------------------

# O modo de iteração pelos meses utiliza um parâmetro no formato:
# (número do mês)|(nome do mês)
# Deste modo, a função começa definindo o nome de cada mês de acordo com o número
# Em seguida, se obtém a tabela do mês e ano desejados.
# O título das colunas é coletado a parte e acrescentado por meio do set_names()

get_faltas <- possibly(
    function(ano, n_mes) {
        
        if (n_mes == 1) {
            mes <- "janeiro"
        } else if(n_mes == 2) {
            mes <- "fevereiro"
        } else if(n_mes == 3) {
            mes <- "março"
        } else if(n_mes == 4) {
            mes <- "abril"
        } else if(n_mes == 5) {
            mes <- "maio"
        } else if(n_mes == 6) {
            mes <- "junho"
        } else if(n_mes == 7) {
            mes <- "julho"
        } else if(n_mes == 8) {
            mes <- "agosto"
        } else if(n_mes == 9) {
            mes <- "setembro"
        } else if(n_mes == 10) {
            mes <- "outubro"
        } else if(n_mes == 11) {
            mes <- "novmebro"
        } else if (n_mes == 12) {
            mes <- "dezembro"
        }
        
        
        u <- "http://www.camara.rj.gov.br/vereadores_frequencia.php?m1=materias_leg&m2=freq"
        
        b <- list(
            "ano" = ano,
            "mes" = paste0(n_mes, "|", mes),
            "sessao" = 1,
            "submit" = "Consultar"
        )
        
        r <- POST(u, body = b)
        t_exists <- read_html(r) %>% 
            xml_find_first('//table') %>% 
            html_table(fill = TRUE)
        faltas <- t_exists %>% 
            as_tibble() %>% 
            set_names(xml_find_all(read_html(r), 
                                   "//td[contains(@class, 'azul-claro')]") %>% 
                          html_text() %>% 
                          str_squish()) %>% 
            janitor::clean_names() %>% 
            slice(-1) %>% 
            mutate(ano = ano,
                   nome_mes = str_to_lower(mes),
                   mes = n_mes,
                   faltas = as.double(faltas),
                   faltas_abonadas = as.double(faltas_abonadas),
                   total_de_faltas = as.double(total_de_faltas),
                   vereador = str_to_title(vereador))
        faltas
        
    }, NULL)

faltas_17 <- map_dfr(1:12, ~get_faltas(2017, .x))
faltas_18 <- map_dfr(1:12, ~get_faltas(2018, .x))
faltas_19 <- map_dfr(1:12, ~get_faltas(2019, .x))
faltas_20 <- map_dfr(1:12, ~get_faltas(2020, .x))
df_faltas <- bind_rows(faltas_17, faltas_18, faltas_19, faltas_20)

write_rds(df_faltas, "out/df_faltas.rds")

# Atividade parlamentar ---------------------------------------------------

# Crio uma função para raspar as páginas da atividade parlamentar, por ano. 
# Os índices representam os anos em ordem decrescente, ou seja índices de 1 a 4 
# indicam que estamos buscando os resultados de 2020 a 2017. A materia é a url
# da página da matéria legislativa que pretendemos obter.

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
write_rds(proelo, "out/proelo.rds")

# Projeto de Lei Complementar
u_prolec <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/LeiCompEmentaInt"
prolec <- map_dfr(1:4, ~scrap_cm(u_prolec, .x)) %>% 
    mutate(materia = "Projeto de Lei Complementar")
write_rds(prolec,"out/prolec.rds")

# Projeto de Lei
u_prolei <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/LeiEmentaInt"
prolei <- map_dfr(1:4, ~scrap_cm(u_prolei, .x)) %>% 
    mutate(materia = "Projeto de Lei")
write_rds(prolei,"out/prolei.rds")

# Indicações
u_ind <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/IndEmentaInt"
indica <- map_dfr(1:4, ~scrap_cm(u_ind, .x)) %>% 
    mutate(materia = "Indicações")
write_rds(indica,"out/indica.rds")

# Moções
u_moc <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/MocaoEmentaInt"
mocoes <- map_dfr(1:4, ~scrap_cm(u_moc, .x)) %>% 
    mutate(materia = "Moções")
write_rds(mocoes,"out/mocoes.rds")

# Requerimentos de Informação
u_req_info <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/ReqInfEmentaInt"
req_info <- map_dfr(1:4, ~scrap_cm(u_req_info, .x)) %>% 
    mutate(materia = "Requerimentos de Informação")
write_rds(req_info,"out/req_info.rds")

# Requerimentos (atentar que terá que ser parseado do 1:5, porque os resultados
# de 2020 estão divididos no ano 2020 e 1920)
u_req <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/ReqEmentaInt"
requerimentos <- map_dfr(1:5, ~scrap_cm(u_req, .x)) %>% 
    mutate(materia = "Requerimentos")
write_rds(requerimentos,"out/requerimentos.rds")

# Ofícios (iterar de 1:8)
u_oficio <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/OficioInt"
oficios <- map_dfr(1:8, ~scrap_cm(u_oficio, .x)) %>% 
    mutate(materia = "Ofícios") %>% 
    filter(str_detect(prolei_num, "^[0-9]")) %>% # elimina as linhas sem informações relevantes
    filter(data_publi > ymd(20161231)) # filtra somente a data de publicação a partir de 2017
write_rds(oficios,"out/oficios.rds")

# Projeto de Decreto Legislativo
# 
# Separando por ano não nos fornecia o nome dos autores, logo foi preciso criar uma
# nova função para puxar os dados separando por número.

pleg_fun <- function(p_num) {
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
               autores = str_remove(autores, "(Vereador)(a*) "),
               materia = "Projeto de Decreto Legislativo")
}
prodeleg <-  map_dfr(c(1,100,199), ~pleg_fun(.x)) %>% 
    filter(data_publi > ymd(20161231))  # filtra somente a data de publicação a partir de 2017
write_rds(prodeleg, "out/prodeleg.rds")

# Projeto de Resolução
# vai pedir uma função própria pois ao buscar por ano retorna a página de Projeto de Lei.
# Os dados estão dispostos na primeira página, sem a necessidade de iterar entre páginas
# Não é uma solução ideal, uma vez que conforme forem sendo inseidos mais projetos, a primeira
# página pode não contemplar mais todos os projetos do período desejado.
# É preciso tentar decifrar a lógica da troca de páginas

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
               autores = str_remove(autores, "(Vereador)(a*) "),
               materia = "Projeto de Resolução") %>% 
        filter(data_publi > ymd(20161231))
}

prores <- pres_fun()
write_rds(prores, "out/prores.rds")

## Unificando as páginas em um único dataframe

atv_parlam <- bind_rows(indica, mocoes, oficios, prodeleg, proelo, prolec, prolei,
                        prores, req_info, requerimentos)%>% 
    left_join(vereadores, c("autores" = "vereador"))

write_rds(atv_parlam, "out/atv_parlam.rds")


# Legislação Municipal ----------------------------------------------------

# O objetivo é coletar as Leis Ordinárias e Leis Complementares que entraram
# em vigor no período de 2017-2020
# 
# As funções para cada tópico são semelhantes, com pequenas alterações no tratamento
# da tabela, pois as colunas são dispostas de forma diferente.

get_lei_comp <- function(materia, index) {
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
        select(c("lei", "data", "status", "autoria", "ementa", "categoria", "subcategoria",
                 "assunto")) %>% 
        as_tibble() %>%
        separate_rows(autoria, sep = ",") %>% 
        filter(str_detect(ementa, "^[A-Z]")) %>%
        mutate(data = mdy(data),
               ementa = str_squish(ementa),
               autoria = str_to_title(autoria),
               autoria = str_remove(autoria, "(Vereador)(a*) "))
}

get_lei_ord <- function(materia, index) {
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
        janitor::clean_names() %>% #glimpse(.)
        select(c("lei", "data", "status", "autoria", "ementa")) %>% 
        as_tibble() %>%
        separate_rows(autoria, sep = ",") %>% 
        filter(str_detect(ementa, "^[A-Z]")) %>%
        mutate(data = mdy(data),
               ementa = str_squish(ementa),
               autoria = str_to_title(autoria),
               autoria = str_remove(autoria, "(Vereador)(a*) "))
}

u_ord <- "https://mail.camara.rj.gov.br/APL/Legislativos/contlei.nsf/LeiOrdAnoIntsup"
u_comp <- "https://mail.camara.rj.gov.br/APL/Legislativos/contlei.nsf/LeiCompAnoInt"

lei_ord <- map_dfr(1:4, ~get_lei_ord(u_ord, .x)) %>% 
    mutate(materia = "Lei Ordinária")

lei_comp <- map_dfr(1:4, ~get_lei_comp(u_comp, .x)) %>% 
    mutate(materia = "Lei Complementar")

l_muni <- bind_rows(lei_comp, lei_ord)%>% 
    mutate(id = group_indices(., lei, materia))

write_rds(l_muni, "out/l_muni.rds")

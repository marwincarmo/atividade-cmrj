u_base <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/LeiEmentaInt"
q_prolei <- list(
    "OpenForm" = "",
    "Start" = 1,
    "Count" = 1000,
    "Expand" = 2
)

get_prolei <- GET(u_base, query = q_prolei)
read_html(get_prolei) %>% 
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


# Projetos de Lei

# prolei <- function(index) {
#   u_base <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/LeiEmentaInt?OpenForm=&Start=1&Count=100&Expand="
#   get_prolei <- GET(paste0(u_base, index))
#   read_html(get_prolei) %>% 
#     xml_find_all('//table') %>%
#     magrittr::extract2(2) %>% 
#     html_table(fill = TRUE) %>% 
#     janitor::clean_names() %>% 
#     select(c("x_2", "ementa", "data_publ", "autor_es")) %>% 
#     set_names(c("prolei_num", "ementa", "data_publi", "autores")) %>% 
#     as_tibble() %>%
#     separate_rows(autores, sep = ",") %>% 
#     filter(str_detect(ementa, "^[A-Z]")) %>%
#     mutate(data_publi = mdy(data_publi))
# }
# 
# # Projeto Emenda à Lei Organica
# 
# proelo <- function(index) {
#   u_base <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/EmendaEmentaInt?OpenForm=&Start=1&Count=100&Expand="
#   get_proelo <- GET(paste0(u_base, index))
#   read_html(get_proelo) %>% 
#     xml_find_all('//table') %>%
#     magrittr::extract2(2) %>% 
#     html_table(fill = TRUE) %>% 
#     janitor::clean_names() %>% 
#     select(c("x_2", "ementa", "data_publ", "autor_es")) %>% 
#     set_names(c("prolei_num", "ementa", "data_publi", "autores")) %>% 
#     as_tibble() %>% 
#     filter(str_detect(ementa, "^[A-Z]")) %>% 
#     separate_rows(autores, sep = ",")
# }
# 
# procomp <- function(index) {
#   u_base <- "https://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/LeiCompEmentaInt?OpenForm=&Start=1&Count=100&Expand="
#   get_procomp <- GET(paste0(u_base, index))
#   read_html(get_procomp) %>% 
#     xml_find_all('//table') %>%
#     magrittr::extract2(2) %>% 
#     html_table(fill = TRUE) %>% 
#     janitor::clean_names() %>% 
#     select(c("x_2", "ementa", "data_publ", "autor_es")) %>% 
#     set_names(c("prolei_num", "ementa", "data_publi", "autores")) %>% 
#     as_tibble() %>% 
#     filter(str_detect(ementa, "^[A-Z]")) %>% 
#     mutate(data_publi = mdy(data_publi)) %>% 
#     separate_rows(autores, sep = ",") 
# }
# 
# Projeto de resolução (rascunho da função)
# u_base <- "http://www.camara.rj.gov.br/controle_atividade_parlamentar.php"
# q_base <- list(
#   "m1" =  "materias_leg",
#   "m2" =  "10a_Leg",
#   "m3" =  "prores",
#   "url" = "http://mail.camara.rj.gov.br/APL/Legislativos/scpro1720.nsf/Internet/ResolucaoInt?OpenForm"
# )
# get_res <- GET(u_base, query = q_base)
# read_html(get_res) %>% 
#   xml_find_all('//table') %>%
#   magrittr::extract2(2) %>% 



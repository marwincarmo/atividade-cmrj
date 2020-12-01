library(tidyverse)
library(rvest)
library(httr)
library(lubridate)

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
        janitor::clean_names() %>% #glimpse(.)
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

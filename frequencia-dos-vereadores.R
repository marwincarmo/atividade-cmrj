# Frequencia dos veradores


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
                   total_de_faltas = as.double(total_de_faltas))
        faltas
    
}, NULL)

faltas_17 <- map_dfr(1:12, ~get_faltas(2017, .x))
faltas_18 <- map_dfr(1:12, ~get_faltas(2018, .x))
faltas_19 <- map_dfr(1:12, ~get_faltas(2019, .x))
faltas_20 <- map_dfr(1:12, ~get_faltas(2020, .x))
df_faltas <- bind_rows(faltas_17, faltas_18, faltas_19, faltas_20)

write_csv(df_faltas, "df_faltas.csv")

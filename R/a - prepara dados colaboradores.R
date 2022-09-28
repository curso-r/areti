library(lubridate)
library(tidyverse)

hora_do_envio_do_questionario <- as_datetime("2019-06-14 13:00:00")
categs_likert_ordenados <- c(
  "Discordo totalmente"
  ,"Discordo parcialmente"
  ,"Não concordo nem discordo"
  ,"Concordo parcialmente"
  ,"Concordo totalmente"
)
eh_likert <- function(x) {
  any(x %in% categs_likert_ordenados)
}


# 
colaboradores <- read_csv("data-raw/(colaboradores) Pesquisa de Cultura da InMetrics.csv") %>%
  janitor::clean_names() %>%
  select(-x79) %>%
  mutate(carimbo_de_data_hora = as_datetime(carimbo_de_data_hora, format = "%Y/%m/%d %I:%M:%S %p") %>% ymd_hms()) %>%
  filter(carimbo_de_data_hora > hora_do_envio_do_questionario) %>%
  mutate_if(eh_likert, ~factor(.x, ordered = TRUE, levels = categs_likert_ordenados)) %>%
  mutate(
    x2_area = case_when(
      x2_area %in% c("QTS", "PMS", "MS", "Consulting", "GO e Controle de Qualidade") ~ "Operações",
      x2_area %in% c("BUs", "Novos Negócios e Parcerias") ~ "B.U.'s",
      x2_area %in% c("Marketing", "Financeiro/Administrativo/TI/CIG", "Pessoas") ~ "Suportes",
      x2_area %in% c("Kernel", "Segurança & Cloud") ~ "Kernel/Segurança e Cloud",
      TRUE ~ "(rever)"
    ),
    x5_faixa_etaria = fct_relevel(x5_faixa_etaria, c("18 a 20 anos", "36 a 45 anos", "21 a 25 anos", "31 a 35 anos", "26 a 30 anos", "Mais de 45 anos")),
    x4_tempo_de_empresa_ha_quanto_tempo_trabalha_na_inmetrics = fct_relevel(x4_tempo_de_empresa_ha_quanto_tempo_trabalha_na_inmetrics, c("Menos de 6 meses", "6 meses a 1 ano", "1 a 2 anos", "2 a 4 anos", "4 a 7 anos", "Mais de 7 anos"))
  )

saveRDS(colaboradores, file = "data/colaboradores.rds")

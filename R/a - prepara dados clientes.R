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
clientes <- read_csv("data-raw/(clientes) Pesquisa de Cultura da InMetrics.csv") %>%
  janitor::clean_names() %>%
  rename(
    x4_quais_as_tres_palavras_que_melhor_descrevem_o_time_inmetrics_que_atende_sua_empresa_palavra1 = x4_quais_as_tres_palavras_que_melhor_descrevem_o_time_inmetrics_que_atende_sua_empresa,
    x4_quais_as_tres_palavras_que_melhor_descrevem_o_time_inmetrics_que_atende_sua_empresa_palavra2 = x23,
    x4_quais_as_tres_palavras_que_melhor_descrevem_o_time_inmetrics_que_atende_sua_empresa_palavra3 = x24
  ) %>%
  slice(c(6, 8, 10, 11, 12, 13)) %>%
  mutate(carimbo_de_data_hora = as_datetime(carimbo_de_data_hora, format = "%Y/%m/%d %I:%M:%S %p") %>% ymd_hms()) %>%
  mutate_if(eh_likert, ~factor(.x, ordered = TRUE, levels = categs_likert_ordenados))

saveRDS(clientes, file = "data/clientes.rds")
write.csv(clientes, file = "data/clientes.csv", fileEncoding = "latin1", row.names = FALSE)

## polychoric


## factor analysis
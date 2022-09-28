library(knitr)
library(JLutils)
library(kableExtra)
library(ggplot2)
library(ggrepel)
library(sjPlot)
library(tidyverse)


colaboradores <- readRDS("C:/Users/ap_da/OneDrive/Documents/areti/data/colaboradores.rds")
construtos <- readxl::read_xlsx("C:/Users/ap_da/OneDrive/Documents/areti/data/Questoes_e_Construtos.xlsx", sheet = "construtos")
variaveis <- readxl::read_xlsx("C:/Users/ap_da/OneDrive/Documents/areti/data/Questoes_e_Construtos.xlsx", sheet = "variaveis")
papel_das_variaveis <- construtos %>% 
  select(variavel_codigo, construto) %>%
  distinct(variavel_codigo, .keep_all = TRUE) %>%
  left_join(variaveis) %>%
  dplyr::mutate(
    variavel_nome = variavel_nome %>% stringr::str_replace_all('\"', '')
  )


tabelar <- function(likert, corte) {
  colaboradores %>%
    mutate(
      questao = fct_explicit_na(!!sym(likert)),
      corte = fct_explicit_na(!!sym(corte))
    ) %>%
    dplyr::count(questao, corte)
}

teste_chisq <- function(likert, corte) {
  suppressWarnings(
    colaboradores %>%
    chisq_test(as.formula(glue::glue("{likert} ~ {corte}")))
  )
}

fisher.test()
library(infer)

colaboradores %>% chisq_test()

tidyr::crossing(
  papel_das_variaveis %>% filter(!positiva_ou_negativa %in% "não likert") %>% select(variavel_nome_likert = variavel_nome),
  papel_das_variaveis %>% filter(construto %in% 'corte') %>% select(variavel_nome_corte = variavel_nome)  
) %>%
  mutate(
    teste_qui_quadrado = map2(variavel_nome_likert, variavel_nome_corte, teste_chisq)
  ) %>%
  unnest %>%
  mutate(
    tabela = map2(variavel_nome_likert, variavel_nome_corte, tabelar)
  )

library(tidyverse)

# Colaboradores

enviar_email_para_colaborador <- function(email) {
  Sys.sleep(.5)
  confirmada <- gmailr::mime() %>% 
    gmailr::to(email) %>% 
    gmailr::from("pesquisa@rseis.com.br") %>%
    # gmailr::bcc("contato@rseis.com.br") %>%
    gmailr::subject("[lembrete] Pesquisa de Cultura Inmetrics") %>% 
    gmailr::html_body(
      read_file("email_colaborador.html")
    )
  gmailr::send_message(confirmada)
  
  cat("Email enviado para", email, "\n")
}


lista_de_emails_de_colaboradores <- c(readr::read_csv2("InMetrics_Base_Colaboradores.csv")$email, 
                                      "athos.damiani@gmail.com",
                                      # "adamiani@curso-r.com",
                                      "moema@areti.com.br",
                                      "lucas@estatikos.com.br",
                                      "patricia.testa@areti.com.br",
                                      "gustavo.oda@inmetrics.com.br")
  
# ultimo_email <- "BRUNA.SANTOS@inmetrics.com.br"
# 
# lista_de_emails_de_colaboradores <- lista_de_emails_de_colaboradores[1:length(lista_de_emails_de_colaboradores) > which(lista_de_emails_de_colaboradores == ultimo_email)]

  #c(
  # "gsanches@inmetrics.com.br",
  # "debora.silva@inmetrics.com.br",
  # "maria.ribeiro@inmetrics.com.br",
  # "olinda.alvino@inmetrics.com.br",
  # "jucelia.silva@inmetrics.com.br",
  # "folha@inmetrics.com.br",
  # "erikasou@inmetrics.com.br",
  # "gabriela.camargo@inmetrics.com.br",
  # "bianca.liberatore@inmetrics.com.br",
  # "pliveira@inmetrics.com.br",
  # "edbertan@inmetrics.com.br",
  # "athos.damiani@gmail.com",
  # "adamiani@curso-r.com",
  # "moema@areti.com.br",
  # "lucas@estatikos.com.br",
  # "patricia.testa@areti.com.br",
  # "danielleastrid@gmail.com"
#)

purrr::walk(lista_de_emails_de_colaboradores, enviar_email_para_colaborador)

# enviar_email_para_colaborador("gustavo.oda@inmetrics.com.br")



# Clientes

enviar_email_para_cliente <- function(email) {
  Sys.sleep(.5)
  confirmada <- gmailr::mime() %>% 
    gmailr::to(email) %>% 
    gmailr::from("pesquisa@rseis.com.br") %>%
    gmailr::bcc("contato@rseis.com.br") %>%
    gmailr::subject("[Cultura Inmetrics] Responda à pesquisa - Hoje é o último dia!") %>% 
    gmailr::html_body(
      read_file("email_cliente.html")
    )
  gmailr::send_message(confirmada)
  
  cat("Email enviado para", email, "\n")
}


lista_de_emails_de_clientes <- c(
  # "gsanches@inmetrics.com.br",
  # "debora.silva@inmetrics.com.br",
  # "maria.ribeiro@inmetrics.com.br",
  # "olinda.alvino@inmetrics.com.br",
  # "jucelia.silva@inmetrics.com.br",
  # "folha@inmetrics.com.br",
  # "erikasou@inmetrics.com.br",
  # "gabriela.camargo@inmetrics.com.br",
  # "bianca.liberatore@inmetrics.com.br",
  # "pliveira@inmetrics.com.br",
  # "edbertan@inmetrics.com.br",
  "athos.damiani@gmail.com",
  # "adamiani@curso-r.com",
  "moema@areti.com.br",
  "lucas@estatikos.com.br",
  "patricia.testa@areti.com.br",
  # "danielleastrid@gmail.com",
  # "brunamazzaron@gmail.com"
  "andreacabeca@gmail.com",
  "ccaselli@bancocbss.com.br",
  "fabio.correa@safra.com.br",
  "luiz.masukawa@livelo.com.br",
  "fabio.brito@santander.com.br",
  "ricardo.geraldini@cielo.com.br",
  "lauro.vargas@original.com.br",
  "cecrodrigues@timbrasil.com.br"
  
)

purrr::walk(lista_de_emails_de_clientes, enviar_email_para_cliente)











emails_nao_entregues <- tibble::tribble(
  ~email, ~motivo_da_nao_entrega,
  "CLEBER.MIRANDA@inmetrics.com.br", "acesso negado pelo outlook",
  "SIMONE.SZYSZKO@inmetrics.com.br", "email não existe",
  "GUSTAVO.GONCALVES@inmetrics.com.br", "acesso negado pelo outlook",
  "brunaesp@inmetrics.com.br", "férias/ausente/licença",
  "damendes@inmetrics.com.br", "férias/ausente/licença",
  "vinicius.gasparini@inmetrics.com.br", "férias/ausente/licença",
  "andrieli@inmetrics.com.br", "férias/ausente/licença",
  "carolcar@inmetrics.com.br", "férias/ausente/licença"
)

emails_nao_entregues










library(polycor)  
library(psych)
library(FactoMineR)

colaboradores <- readRDS("C:/Users/ap_da/OneDrive/Documents/areti/data/colaboradores.rds")
construtos <- readxl::read_xlsx("C:/Users/ap_da/OneDrive/Documents/areti/data/Questoes_e_Construtos.xlsx", sheet = "construtos")
variaveis <- readxl::read_xlsx("C:/Users/ap_da/OneDrive/Documents/areti/data/Questoes_e_Construtos.xlsx", sheet = "variaveis")
papel_das_variaveis <- construtos %>% 
  left_join(variaveis)
variaveis_selecionadas <- papel_das_variaveis %>% 
  filter(
    construto %in% c("Maior Valorização da Rapidez do que de Qualidade")
  ) %>%
  mutate(
    variavel_nome = variavel_nome %>% stringr::str_replace_all('\"', '')
  )

## polychoric             
rapidez_vs_qualidade_pc <- colaboradores %>% 
  select_at(vars(!!variaveis_selecionadas$variavel_nome)) %>%
  as.data.frame %>%
  hetcor(ML = TRUE) 

saveRDS(rapidez_vs_qualidade_pc, file = "data/rapidez_vs_qualidade_pc.rds")

rapidez_vs_qualidade_pc <- readRDS(file = "data/rapidez_vs_qualidade_pc.rds")


library("pheatmap")
pheatmap(abs(rapidez_vs_qualidade_pc$correlations) %>% `colnames<-`(1:5), cutree_rows = 3, clustering_method = "ward.D2")

## factor analysis
fa.parallel(rapidez_vs_qualidade_pc$correlations, n.obs = nrow(colaboradores))

rapidez_vs_qualidade_fa <- fa(
  rapidez_vs_qualidade_pc$correlations, 
  nfactors = 2, 
  n.obs = nrow(colaboradores), 
  rotate="varimax"
)

saveRDS(rapidez_vs_qualidade_fa, file = "data/rapidez_vs_qualidade_fa.rds")

rapidez_vs_qualidade_fa <- readRDS(file = "data/rapidez_vs_qualidade_fa.rds")



# http://www.anpad.org.br/admin/pdf/GPR2349.pdf
# https://mail.google.com/mail/u/0/#inbox/FMfcgxwChcgKpmSqFCLbrFlsdjKJxLSC?projector=1&messagePartId=0.1
# http://www.revispsi.uerj.br/v10n3/artigos/html/v10n3a10.html
# https://en.wikipedia.org/wiki/Cronbach%27s_alpha
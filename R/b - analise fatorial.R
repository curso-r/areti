library(polycor)  
library(psych)
library(FactoMineR)

colaboradores <- readRDS(file = "data/colaboradores.rds")

## polychoric             
colaboradores_pc <- colaboradores %>% 
  select_if(eh_likert) %>%
  as.data.frame %>%
  hetcor(ML = TRUE, use = c("pairwise.complete.obs")) 

saveRDS(colaboradores_pc, file = "data/colaboradores_pc.rds")

colaboradores_pc <- readRDS(file = "data/colaboradores_pc.rds")


library("pheatmap")
pheatmap(abs(colaboradores_pc$correlations) %>% `colnames<-`(1:68), cutree_rows = 10, clustering_method = "ward.D2")

## factor analysis
fa.parallel(colaboradores_pc$correlations, n.obs = nrow(colaboradores))

colaboradores_fa <- fa(
  colaboradores_pc$correlations, 
  nfactors = 12, 
  n.obs = nrow(colaboradores), 
  rotate="varimax"
)

colaboradores_fa$loadings


saveRDS(colaboradores_fa, file = "data/colaboradores_fa.rds")

colaboradores_fa <- readRDS(file = "data/colaboradores_fa.rds")

  
# http://www.anpad.org.br/admin/pdf/GPR2349.pdf
# https://mail.google.com/mail/u/0/#inbox/FMfcgxwChcgKpmSqFCLbrFlsdjKJxLSC?projector=1&messagePartId=0.1
# http://www.revispsi.uerj.br/v10n3/artigos/html/v10n3a10.html
# https://en.wikipedia.org/wiki/Cronbach%27s_alpha
# Importando tabelas auxiliares de https://balanca.economia.gov.br/balanca/bd/tabelas

site <- 'https://balanca.economia.gov.br/balanca/bd'

nome_tabelas  <- c('NCM', 'NCM_SH', 'PAIS', 'UF')

carrega_tabelas <- function(nome_tabela, caminho) {
  readr::read_csv2(paste0(site, caminho, nome_tabela,'.csv'))
}

e <- try(is.list(tabelas_dimensao), silent = TRUE)

if (e != TRUE) {
  tabelas_dimensao <- purrr::map2(nome_tabelas, rep('/tabelas/', 4), carrega_tabelas)
}

saveRDS(tabelas_dimensao, "data/tabelas_dimensao.RDS")

# Importando e consolidando dados de exportações de 2011 a 2021 disponíveis em
# https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_20XX.csv

e <- try(is.object(exportacoes), silent = TRUE)

if (e != TRUE) {
  exportacoes <- purrr::map2_dfr(c(2012:2021), rep('/comexstat-bd/ncm/EXP_', 10), carrega_tabelas)
}

# Aproximadamente 7 minutos para gerar uma tabela com 11 variáveis e 11.780.761 observações

# Importando e consolidando dados de importações de 2011 a 2021 disponíveis em
# https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/IMP_20XX.csv

e <- try(is.object(importacoes), silent = TRUE)

if (e != TRUE) {
  importacoes <- purrr::map2_dfr(c(2012:2021), rep('/comexstat-bd/ncm/IMP_', 10), carrega_tabelas)
}

# Aproximadamente 20 minutos para gerar uma tabela com 13 variáveis e 18.533.347 observações

# Carregando tabela 'agrupamentos.csv'

agrupamentos <- readr::read_csv2("data-raw/agrupamentos.csv")

# Aplicadas as seguintes transformações

exportacoes <-
  exportacoes |>
  rename(V_EXP = VL_FOB) |>
  group_by(CO_ANO, SG_UF_NCM, CO_PAIS, CO_NCM) |>
  summarise(V_EXP = sum(V_EXP, na.rm = TRUE))

importacoes <-
  importacoes |>
  rename(V_IMP = VL_FOB) |>
  group_by(CO_ANO, SG_UF_NCM, CO_PAIS, CO_NCM) |>
  summarise(V_IMP = sum(V_IMP, na.rm = TRUE))

# Unindo as duas tabelas

operacoes <- full_join(exportacoes, importacoes)

# Acrescentando dados de setor

operacoes <- left_join(operacoes, agrupamentos)

## Saldo a tabela operacoes

saveRDS(operacoes, "data/operacoes.RDS")

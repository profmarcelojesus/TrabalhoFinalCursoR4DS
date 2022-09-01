## Carregando tabelas

e <- try(is.object(operacoes), silent = TRUE)

if (e != TRUE) {
  operacoes <- readRDS("data/operacoes.RDS")
}

e <- try(is.object(tabelas_dimensao), silent = TRUE)

if (e != TRUE) {
  tabelas_dimensao <- readRDS("data/tabelas_dimensao.RDS")
}


## Criando tabelas agrupadas por ano, uf e setor e por ano, uf e destino

operacoes_por_setor <- operacoes |>
  filter(SG_UF_NCM != "CB" & SG_UF_NCM != "EX" & SG_UF_NCM != "MN" &
                  SG_UF_NCM != "RE" & SG_UF_NCM != "ED" & SG_UF_NCM != "ND" &
                  SG_UF_NCM != "ZN" & !is.na(SETOR)) |>
  group_by(CO_ANO, SG_UF_NCM, SETOR) |>
  summarise(V_EXP = sum(V_EXP, na.rm = TRUE), V_IMP = sum(V_IMP, na.rm = TRUE))

operacoes_por_destino <- operacoes |>
  filter(SG_UF_NCM != "CB" & SG_UF_NCM != "EX" & SG_UF_NCM != "MN" &
                  SG_UF_NCM != "RE" & SG_UF_NCM != "ED" & SG_UF_NCM != "ND" &
                  SG_UF_NCM != "ZN" & !is.na(SETOR)) |>
  group_by(CO_ANO, SG_UF_NCM, CO_PAIS) |>
  summarise(V_EXP = sum(V_EXP, na.rm = TRUE), V_IMP = sum(V_IMP, na.rm = TRUE))

operacoes_por_destino <- left_join(operacoes_por_destino, data.frame(tabelas_dimensao[[3]]))

## Exportações por UF/SETOR/ANO

export_uf_setor_ano <-
  operacoes_por_setor[,-5] |>
  group_by(SETOR, SG_UF_NCM, CO_ANO) |>
  pivot_wider(names_from = CO_ANO, values_from = V_EXP, values_fn = sum, values_fill = 0) |>
  arrange(SG_UF_NCM) |>
  mutate(TOTAL = sum(c_across(where(is.numeric))))

## Importações por UF/SETOR/ANO

import_uf_setor_ano <-
  operacoes_por_setor[,-4] |>
  group_by(SETOR, SG_UF_NCM, CO_ANO) |>
  pivot_wider(names_from = CO_ANO, values_from = V_IMP, values_fn = sum, values_fill = 0) |>
  arrange(SG_UF_NCM)|>
  mutate(TOTAL = sum(c_across(where(is.numeric))))

## Identificando a UF com maior volume de exportações para cada setor

principal_uf_por_setor <-
  export_uf_setor_ano |>
  group_by(SETOR) |>
  filter(TOTAL == max(TOTAL))

## Identificando o setor com maior volume de exportações para cada UF

principal_setor_por_uf <-
  export_uf_setor_ano |>
  group_by(SG_UF_NCM) |>
  filter(TOTAL == max(TOTAL)) |>
  arrange(SETOR)

## Preaparação de indicadores por setor

indicadores_por_setor <-
  operacoes_por_setor |>
  mutate(
    Xij = V_EXP,
    Mij = V_IMP,
    Xi = sum(V_EXP),
    Mi = sum(V_IMP)
  ) |>
  group_by(CO_ANO, SETOR) |>
  mutate(
    Xwj = sum(V_EXP),
    Mwj = sum(V_IMP),
    Xjr = Xi - Xij
  ) |>
  group_by(CO_ANO) |>
  mutate(
    Xw = sum(V_EXP),
    Mw = sum(V_IMP),
    IVCR = (Xij / Xi) / (Xwj / Xw)
  )|>
  mutate(
    Xjr = Xwj - Xij,
    Xir = Xi - Xij,
    Xwr = Xw - Xwj,
    Mjr = Mwj - Mij,
    Mir = Mi - Mij,
    Mwr = Mw - Mwj,
    VCRS = (IVCR - 1) / (IVCR + 1),
    VRE = log((Xij / Xjr) / (Xir / Xwr)),
    CR = log(((Xij / Xjr) / (Xir / Xwr)) / ((Mij / Mjr) / (Mir / Mwr))),
    GLn = ((Xij + Mij) - abs(Xij - Mij)),
    GLd = (Xij + Mij),
    ICP = (Xij / Xi)^2
  )

## Preparação de indicadores por destino

indicadores_por_destino <-
  operacoes_por_destino |>
  mutate(
    Xik = V_EXP,
    Xi = sum(V_EXP)
  ) |>
  mutate(
    ICD = (Xik / Xi)^2
  )

## Resumo de indicadores por UF/ANO

indicadores_por_uf_ano <-
  indicadores_por_setor[,-c(4:14,16:20)] |>
  mutate(
    across(
      .cols = c(IVCR:ICP),
      .fns = ~ ifelse(is.infinite(.), NA, .)
    )
  ) |>
  group_by(SG_UF_NCM, CO_ANO) |>
  summarise(
    across(
      .cols = c(IVCR, VCRS:CR),
      .fns = mean,
      na.rm = T
    ),
    across(
      .cols = c(GLn, GLd, ICP),
      .fns = sum,
      na.rm = T
    )
  ) |>
  mutate(
    GL = GLn / GLd,
    ICP = sqrt(ICP)
  )

indicadores_por_uf_ano |>
  select(-c(7:8))

indicadores_por_destino_ano <-
  indicadores_por_destino |>
  select(CO_ANO, SG_UF_NCM, ICD) |>
  group_by(SG_UF_NCM, CO_ANO) |>
  summarise(ICD = sum(ICD, na.rm =T)) |>
  mutate(ICD = sqrt(ICD))

indicadores_por_uf_ano <- full_join(indicadores_por_uf_ano, indicadores_por_destino_ano)


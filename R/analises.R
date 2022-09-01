## Teste normalidade multivariada

res <- mvn(data = indicadores_por_uf_ano[,-c(2:3)], subset = 'UF', mvnTest = 'hz', tol = 1e-35)

res.multivariateNormality <- map_dfr(res$multivariateNormality, ~ bind_rows(.))

res.multivariateNormality$UF <- names(res$multivariateNormality)

## Violação da normalidade multivariada por UF

res.multivariateNormality <-
  res.multivariateNormality |>
  relocate(UF) |>
  filter(MVN == 'NO')

## Teste normalidade univariada

unir_resultados <- function(tabela, uf){
   tabela |> mutate(UF = uf)
}

res.univariateNormality <- map2_dfr(res$univariateNormality, names(res$univariateNormality), unir_resultados)

## Violação da normalidade univariada por variável/UF

res.univariateNormality <-
  res.univariateNormality |>
  relocate(UF) |>
  mutate(Normality = trimws(Normality)) |>
  filter(Normality == 'NO')

table(res.univariateNormality$Variable)

## Verificando outliers multivariados

indicadores_por_uf_ano[,-2] |>
  group_by(UF) |>
  doo(~mahalanobis_distance(.)) |>
  filter(is.outlier == TRUE)

## Verificando homogeneidade das matrizes de covariância e variâncias

res <- box_m(indicadores_por_uf_ano[,-c(1:2)], indicadores_por_uf_ano$UF)

## Verificando a homogeneidade de variâncias

teste_levene <- function(coluna) {
  leveneTest(coluna ~ as_factor(UF),
             indicadores_por_uf_ano,
             center = mean)
}

res <- map_dfr(indicadores_por_uf_ano[,-c(1:2)], teste_levene)

res <- res |> filter(!is.na(res$`Pr(>F)`))

res$Variavel <- names(indicadores_por_uf_ano[,-c(1:2)])

res <- res |> relocate(Variavel)

## Verificando a presença de multicolinearidade

cor(indicadores_por_uf_ano[,-c(1:3)])

## Verificando a relação linear entre as variáveis dependentes

plots <- indicadores_por_uf_ano[,-c(2:3)] |>
  group_by(UF) |>
  doo(~ggpairs(.), result = 'plots')

ms <- plots |> filter(UF  == 'MS')

ms$plots

## Modelo MANOVA

variaveis <- cbind(indicadores_por_uf_ano$VCRS,
                   indicadores_por_uf_ano$VRE,
                   indicadores_por_uf_ano$CR,
                   indicadores_por_uf_ano$ICP,
                   indicadores_por_uf_ano$GL,
                   indicadores_por_uf_ano$ICD)

modelo <- manova(variaveis   ~ UF, data = indicadores_por_uf_ano)

summary(modelo, test = "Pillai")

## Teste ANOVA

summary.aov(modelo)

## Testes post-hocs

indicadores_por_uf_ano$UF <- as_factor(indicadores_por_uf_ano$UF)

res <- TukeyHSD(x = aov(VCRS ~ UF, data = indicadores_por_uf_ano), 'UF', conf.level = 0.95)

res <- data.frame(res$UF)

res[res$p.adj > 0.05, ]

## Medindo o tamanho do efeito da variável independente

effectsize::eta_squared(modelo)

#### Carregando conjuntos de variáveis X e Y
# x - variáveis independentes (Indicadores de Concentração)
# y - variáveis dependentes (Vantagens Comparativas)

X <- as.matrix(indicadores_por_uf_ano[,7:9])
Y <- as.matrix(indicadores_por_uf_ano[,4:6])

#### Analisando a matriz de correlação entre X e Y quanto à correlação dentro e entre os grupos.

#### Calculando a matriz e salvando em "correl"

#### Correl, cache=TRUE}
correl <- matcor(X, Y)

#### Obtermos a correlação canonica

# X - matriz numérica (n * p), contendo as coodenadas X
# Y - matriz numérica (n * q), contendo as coodenadas Y

#### ccyx, cache=TRUE}

ccyx <- cc(X,Y)

#### Obtendo a correlação canônica e salvando no objeto v.ca
#### ca, cache=TRUE}

v.ca <- ccyx$cor

#### Testes multivariados de significância para ambas funções canônicas

#### rho, cache=TRUE}
rho <- ccyx$cor

#### Definindo o número de observações "n", o número de variáveis no primeiro
# conjunto de dados "p" e o número de variáveis no segundo conjunto "q".
#### npq, cache=TRUE}

n <- dim(indicadores_por_uf_ano[,c(4:9)])[1]
p <- dim(X)[2]
q <- dim(Y)[2]

#### Lambda de Wilks
#### Wilks, cache=TRUE}
p.asym(rho, n, p, q, tstat = "Wilks")

#### Traço de Pillai
#### Pillai, cache=TRUE}
p.asym(rho, n, p, q, tstat = "Pillai")

#### Traço de Hoteling
#### Hoteling, cache=TRUE}
p.asym(rho, n, p, q, tstat = "Hotelling")

#### Traço de gcr de Roy
#### Roy, cache=TRUE}
p.asym(rho, n, p, q, tstat = "Roy")

#### Proporção da variância total explicada
#### Comput loadings, cache=TRUE}
loadings <- comput(X,Y,ccyx)

#### PVTE Uk: Variância das variáveis independentes explicada pelas variáveis canônicas
#### pvte_u}
pvte.u <-(colSums((loadings$corr.X.xscores)^2))/(dim(X)[2])*100

#### PVTE Vk: Variância das variáveis dependentes explicada pelas variáveis canônicas
#### pvte_v}
pvte.v <-(colSums((loadings$corr.Y.yscores)^2))/(dim(Y)[2])*100

#### Índice de redundância (IR)

# Sintetiza a PVTE e o R² canônico em um único indicador
# R² canônico: indica o quanto da variancia da variável canônica
# dependente é explicada pela variável canônica independente.
# É o quadrado da correlação canônica.
#### IR}
r2.c <-ccyx$cor^2

#### Calculando IR para as variáveis canônicas Uk
#### IRx}
ir.x <-(colSums((loadings$corr.X.xscores)^2))/(dim(X)[2])*(ccyx$cor^2)*100

#### Calculando IR para as variáveis canônicas Vk
#### IRy}
ir.y <-(colSums((loadings$corr.Y.yscores)^2))/(dim(Y)[2])*(ccyx$cor^2)*100

#### Coeficientes padronizados (pesos canônicos)

# A magnitude dos pesos canônicos (coeficientes padronizados) representa
# a contribuição relativa de cada variável para com a respectiva variável
# estatística latente (variável canônica).

#### Variáveis dependentes;
#### sy}
sy <- diag(sqrt(diag(cov(Y))))

#### Variáveis independentes;
#### sx}
sx <- diag(sqrt(diag(cov(X))))

tabela8 <- data.frame("u1v1" = v.ca[1], "u2v2" = v.ca[2], "u3v3" = v.ca[3])

tabela9 <- p.asym(rho, n, p, q, tstat = "Pillai")

tabela9 <- data.frame(tabela9$stat, tabela9$approx, tabela9$df1, tabela9$df2, tabela9$p.value)

colnames(tabela9) <- c("stat", "approx", "df1", "df2", "p.value")

row.names(tabela9) <- c("1 a 3", "2 a 3", "3 a 3")

tabela9[,5] <- cell_spec(round(tabela9[,5], 3))

tabela9[1,5]  <- paste0(tabela9[1,5], footnote_marker_symbol(1))

tabela10 <- rbind(loadings$corr.Y.xscores, loadings$corr.X.xscores)
colnames(tabela10) <- c("v1", "v2", "v3")
row.names(tabela10) <- str_replace(row.names(tabela10), "z", "")

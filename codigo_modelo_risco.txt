# =============================================================================
# [Case] Modelagem de Riscos – Modelo preditivo do TARGET (classificação)
# -----------------------------------------------------------------------------
# Autora: Rayssa Lorrane
#
# Abordagem:
#   1. Análise descritiva das variáveis v1–v8 e da taxa de TARGET.
#   2. Divisão treino/teste estratificada (70/30).
#   3. Binning monotônico + WoE (Weight of Evidence) e IV (Information Value)
#      de cada variável, calculados só no treino.
#   4. Modelo principal: regressão logística sobre as variáveis em WoE
#      (scorecard), com seleção por IV, correlação, sinal e significância.
#   5. Modelo de comparação: Random Forest (ranger) com as variáveis originais.
#   6. Métricas: AUC, Gini, KS, Brier, matriz de confusão, calibração,
#      ordenação por decil, PSI e validação cruzada (5 folds).
#   7. Score (0–1000) e faixas de risco para uso no negócio.
#
# Como rodar (a partir da raiz do projeto, abrindo o .Rproj):
#   source("scripts/modelo_risco.R")
#
# Entrada: data/raw/DADOS_CASE_ANALISTA.xlsx
# Saídas:  output/resultados_modelo.xlsx  (todas as tabelas)
#          output/figuras/*.png           (gráficos)
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ranger)     # Random Forest (modelo de comparação)
  library(openxlsx)
})

ARQ_DADOS <- file.path("data", "raw", "DADOS_CASE_ANALISTA.xlsx")
DIR_SAIDA <- "output"
DIR_FIG   <- file.path(DIR_SAIDA, "figuras")
dir.create(DIR_FIG, showWarnings = FALSE, recursive = TRUE)

SEMENTE   <- 2026
VARS      <- paste0("v", 1:8)
PCT_TREINO <- 0.70

set.seed(SEMENTE)


# =============================================================================
# FUNÇÕES
# =============================================================================

# ---- Métricas de discriminação ------------------------------------------------

# AUC pela estatística de Mann-Whitney (probabilidade de um "mau" ter
# probabilidade prevista maior que um "bom")
calc_auc <- function(y, p) {
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(rank(p)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# KS: maior distância entre as distribuições acumuladas de bons e maus
calc_ks <- function(y, p) {
  cortes <- sort(unique(p))                      # avalia só nos valores distintos (trata empates)
  max(abs(ecdf(p[y == 1])(cortes) - ecdf(p[y == 0])(cortes)))
}

resumo_metricas <- function(y, p, amostra, modelo) {
  auc <- calc_auc(y, p)
  tibble(modelo = modelo, amostra = amostra, n = length(y), taxa_target = mean(y),
         auc = auc, gini = 2 * auc - 1, ks = calc_ks(y, p),
         brier = mean((p - y)^2))
}

# Ponto de corte que maximiza o KS (equivale à estatística J de Youden)
corte_ks <- function(y, p) {
  cortes <- sort(unique(quantile(p, probs = seq(0.01, 0.99, by = 0.01))))
  j <- sapply(cortes, function(k) mean(p[y == 1] >= k) - mean(p[y == 0] >= k))
  cortes[which.max(j)]
}

matriz_confusao <- function(y, p, corte, amostra) {
  prev <- as.integer(p >= corte)
  vp <- sum(prev == 1 & y == 1); fp <- sum(prev == 1 & y == 0)
  fn <- sum(prev == 0 & y == 1); vn <- sum(prev == 0 & y == 0)
  tibble(amostra = amostra, corte = corte,
         verdadeiro_positivo = vp, falso_positivo = fp,
         falso_negativo = fn, verdadeiro_negativo = vn,
         acuracia = (vp + vn) / length(y),
         sensibilidade = vp / (vp + fn),        # % dos maus que o modelo pega
         especificidade = vn / (vn + fp),       # % dos bons corretamente aprovados
         precisao = vp / (vp + fp),             # dos sinalizados, % que são maus
         f1 = 2 * vp / (2 * vp + fp + fn),
         pct_sinalizados = mean(prev))
}

# Tabela por decil de probabilidade: ordenação, captura acumulada e calibração
tabela_decis <- function(y, p) {
  tibble(y = y, p = p) |>
    mutate(decil = ntile(-p, 10)) |>               # decil 1 = maior risco
    group_by(decil) |>
    summarise(n = n(), maus = sum(y), taxa_observada = mean(y),
              prob_media_prevista = mean(p), p_min = min(p), p_max = max(p),
              .groups = "drop") |>
    mutate(captura_acum_maus = cumsum(maus) / sum(maus),
           lift = taxa_observada / mean(y))
}

# PSI (Population Stability Index) entre duas distribuições de score
calc_psi <- function(ref, novo, n_faixas = 10) {
  cortes <- unique(quantile(ref, probs = seq(0, 1, length.out = n_faixas + 1)))
  cortes[1] <- -Inf; cortes[length(cortes)] <- Inf
  a <- table(cut(ref, cortes)) / length(ref)
  b <- table(cut(novo, cortes)) / length(novo)
  a <- pmax(a, 1e-6); b <- pmax(b, 1e-6)
  sum((b - a) * log(b / a))
}


# ---- Binning monotônico e WoE ---------------------------------------------------
# WoE de uma faixa = ln( %bons da faixa / %maus da faixa ).
#   WoE > 0 -> faixa com risco menor que a média; WoE < 0 -> risco maior.
# IV = soma( (%bons - %maus) x WoE ): poder de separação da variável.
#   < 0,02 sem poder | 0,02–0,1 fraco | 0,1–0,3 médio | 0,3–0,5 forte | > 0,5 muito forte

tabela_faixas <- function(x, y, cortes) {
  faixa <- cut(x, c(-Inf, cortes, Inf), right = TRUE)
  tibble(faixa = faixa, y = y) |>
    group_by(faixa, .drop = FALSE) |>
    summarise(n = n(), maus = sum(y), .groups = "drop") |>
    mutate(bons = n - maus, taxa = maus / pmax(n, 1))
}

# Faixas iniciais por quantis (ou um valor por faixa, se a variável tiver
# poucos valores). Depois, faixas vizinhas são unidas até que:
#   - toda faixa tenha ao menos min_pct da amostra (2%: com 5%, o grupo de
#     4,6% dos clientes com v5/v6 > 0, que tem 73% de TARGET, seria unido
#     aos zeros e a variável perderia todo o seu poder); e
#   - a taxa de TARGET seja monotônica (sempre sobe ou sempre desce),
#     o que deixa o modelo estável e fácil de explicar.
binning_monotonico <- function(x, y, max_faixas = 10, min_pct = 0.02) {
  valores <- sort(unique(x))
  cortes <- if (length(valores) <= max_faixas) {
    head(valores, -1)
  } else {
    unique(quantile(x, probs = seq_len(max_faixas - 1) / max_faixas, type = 1, names = FALSE))
  }
  cortes <- cortes[cortes < max(x)]
  sentido <- sign(cor(x, y, method = "spearman"))
  if (sentido == 0) sentido <- 1

  repeat {
    if (length(cortes) == 0) break
    tab <- tabela_faixas(x, y, cortes)
    pct <- tab$n / length(x)

    if (any(pct < min_pct)) {
      # une a menor faixa à vizinha de taxa mais parecida
      i <- which.min(pct); k <- nrow(tab)
      viz <- if (i == 1) 2 else if (i == k) k - 1 else
        c(i - 1, i + 1)[which.min(abs(tab$taxa[c(i - 1, i + 1)] - tab$taxa[i]))]
      cortes <- cortes[-min(i, viz)]
      next
    }
    dif <- diff(tab$taxa) * sentido
    if (any(dif < 0)) {
      cortes <- cortes[-which(dif < 0)[1]]       # une o primeiro par fora do sentido
      next
    }
    break
  }
  cortes
}

calcular_woe <- function(x, y, cortes, variavel) {
  tabela_faixas(x, y, cortes) |>
    mutate(
      variavel = variavel,
      pct_pop  = n / sum(n),
      dist_bons = (bons + 0.5) / sum(bons + 0.5),   # +0,5 evita log(0)
      dist_maus = (maus + 0.5) / sum(maus + 0.5),
      woe = log(dist_bons / dist_maus),
      iv_faixa = (dist_bons - dist_maus) * woe
    ) |>
    select(variavel, faixa, n, pct_pop, maus, bons, taxa, woe, iv_faixa)
}

aplicar_woe <- function(dados, bins) {
  for (v in names(bins)) {
    b <- bins[[v]]
    faixa <- cut(dados[[v]], c(-Inf, b$cortes, Inf), right = TRUE)
    dados[[paste0("woe_", v)]] <- b$tabela$woe[as.integer(faixa)]
  }
  dados
}


# =============================================================================
# 1. LEITURA E ANÁLISE DESCRITIVA
# =============================================================================

dados <- read_excel(ARQ_DADOS) |>
  mutate(target = as.integer(target))

stopifnot(all(c(VARS, "target") %in% names(dados)))
cat("Linhas:", nrow(dados), "| Taxa de TARGET:", round(100 * mean(dados$target), 2), "%\n")
cat("Valores ausentes:", sum(is.na(dados)), "| Linhas duplicadas:", sum(duplicated(dados)), "\n\n")

descritiva <- dados |>
  pivot_longer(all_of(VARS), names_to = "variavel", values_to = "valor") |>
  group_by(variavel) |>
  summarise(
    n_distintos = n_distinct(valor),
    pct_zero = mean(valor == 0),
    media = mean(valor), desvio_padrao = sd(valor),
    minimo = min(valor), p01 = quantile(valor, 0.01), p25 = quantile(valor, 0.25),
    mediana = median(valor), p75 = quantile(valor, 0.75), p99 = quantile(valor, 0.99),
    maximo = max(valor),
    media_target_0 = mean(valor[target == 0]),
    media_target_1 = mean(valor[target == 1]),
    .groups = "drop"
  )

correlacoes <- cor(dados[c(VARS, "target")]) |>
  as.data.frame() |> tibble::rownames_to_column("variavel")

# Estrutura percebida nas variáveis (os nomes são anônimos):
#   v1 = ln(1 + contagem 0..8);  v5 = 230,82 x log2(1 + contagem 0..12);
#   v3 = categórica (1, 2, 3);  v5, v6 e v8 com muitos zeros.
cat("v1 = ln(1 + k)? ", all(abs(exp(dados$v1) - 1 - round(exp(dados$v1) - 1)) < 1e-6), "\n")
k_v5 <- round(2^(dados$v5 / 230.82) - 1)
cat("v5 = 230,82 x log2(1 + k)? ", all(abs(230.82 * log2(1 + k_v5) - dados$v5) < 0.02), "\n\n")


# =============================================================================
# 2. TREINO / TESTE (estratificado pelo TARGET)
# =============================================================================

dados <- dados |> mutate(id = row_number())
ids_treino <- dados |>
  group_by(target) |>
  slice_sample(prop = PCT_TREINO) |>
  pull(id)

treino <- dados |> filter(id %in% ids_treino)
teste  <- dados |> filter(!id %in% ids_treino)
cat("Treino:", nrow(treino), "(", round(100 * mean(treino$target), 2), "% TARGET ) | ",
    "Teste:", nrow(teste), "(", round(100 * mean(teste$target), 2), "% TARGET )\n\n")


# =============================================================================
# 3. BINNING, WoE E IV (calculados só no treino)
# =============================================================================

bins <- setNames(lapply(VARS, function(v) {
  cortes <- binning_monotonico(treino[[v]], treino$target)
  list(cortes = cortes, tabela = calcular_woe(treino[[v]], treino$target, cortes, v))
}), VARS)

tab_woe <- bind_rows(lapply(bins, `[[`, "tabela"))
tab_iv <- tab_woe |>
  group_by(variavel) |>
  summarise(iv = sum(iv_faixa), n_faixas = n(), .groups = "drop") |>
  mutate(poder = cut(iv, c(-Inf, 0.02, 0.1, 0.3, 0.5, Inf),
                     labels = c("Sem poder", "Fraco", "Médio", "Forte", "Muito forte"))) |>
  arrange(desc(iv))
print(tab_iv)

treino_w <- aplicar_woe(treino, bins)
teste_w  <- aplicar_woe(teste, bins)


# =============================================================================
# 4. SELEÇÃO DE VARIÁVEIS E REGRESSÃO LOGÍSTICA (scorecard)
# =============================================================================
# Critérios, em ordem:
#   a) IV >= 0,02 (variável com algum poder de separação);
#   b) entre variáveis muito correlacionadas (|r| > 0,7 em WoE), fica a de maior IV;
#   c) no modelo, coeficiente com o sinal esperado (negativo: WoE maior = risco
#      menor) e p-valor < 0,05; a pior variável é retirada e o modelo, reajustado.

candidatas <- tab_iv |> filter(iv >= 0.02) |> pull(variavel)

cor_woe <- cor(treino_w[paste0("woe_", candidatas)])
removidas_cor <- character(0)
for (v in candidatas) {                                   # candidatas em ordem de IV
  if (v %in% removidas_cor) next
  outras <- setdiff(candidatas, c(v, removidas_cor))
  fortes <- outras[abs(cor_woe[paste0("woe_", v), paste0("woe_", outras)]) > 0.7]
  removidas_cor <- c(removidas_cor, fortes)
}
selecionadas <- setdiff(candidatas, removidas_cor)

repeat {
  formula <- as.formula(paste("target ~", paste0("woe_", selecionadas, collapse = " + ")))
  modelo_log <- glm(formula, data = treino_w, family = binomial)
  coefs <- summary(modelo_log)$coefficients[-1, , drop = FALSE]
  problema <- coefs[, "Estimate"] > 0 | coefs[, "Pr(>|z|)"] > 0.05
  if (!any(problema)) break
  pior <- rownames(coefs)[problema][which.max(coefs[problema, "Pr(>|z|)"])]
  selecionadas <- setdiff(selecionadas, sub("^woe_", "", pior))
}

# VIF (multicolinearidade) das variáveis finais
calc_vif <- function(dados, vars) {
  sapply(vars, function(v) {
    if (length(vars) == 1) return(1)
    r2 <- summary(lm(as.formula(paste(v, "~", paste(setdiff(vars, v), collapse = "+"))),
                     data = dados))$r.squared
    1 / (1 - r2)
  })
}

tab_coef <- summary(modelo_log)$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column("termo") |>
  rename(coeficiente = Estimate, erro_padrao = `Std. Error`, z = `z value`, p_valor = `Pr(>|z|)`) |>
  mutate(variavel = sub("^woe_", "", termo),
         razao_chance_1dp = ifelse(termo == "(Intercept)", NA,
                                   exp(coeficiente * sapply(termo, function(t)
                                     if (t == "(Intercept)") NA else sd(treino_w[[t]]))))) |>
  left_join(tab_iv |> select(variavel, iv), by = "variavel") |>
  left_join(tibble(termo = paste0("woe_", selecionadas),
                   vif = calc_vif(treino_w, paste0("woe_", selecionadas))), by = "termo")

cat("\nVariáveis selecionadas:", paste(selecionadas, collapse = ", "), "\n")
cat("Removidas por correlação:", paste(removidas_cor, collapse = ", "), "\n")
print(tab_coef)

p_log_treino <- predict(modelo_log, treino_w, type = "response")
p_log_teste  <- predict(modelo_log, teste_w,  type = "response")


# =============================================================================
# 5. MODELO DE COMPARAÇÃO: RANDOM FOREST
# =============================================================================
# Captura não linearidades e interações sem binning. Serve de referência:
# se ganhar pouco da logística, o modelo interpretável é preferível.

modelo_rf <- ranger(
  x = as.data.frame(treino[VARS]), y = factor(treino$target),
  probability = TRUE, num.trees = 500, mtry = 3, min.node.size = 50,
  importance = "permutation", seed = SEMENTE
)
p_rf_treino <- predict(modelo_rf, as.data.frame(treino[VARS]))$predictions[, "1"]
p_rf_teste  <- predict(modelo_rf, as.data.frame(teste[VARS]))$predictions[, "1"]
p_rf_oob    <- modelo_rf$predictions[, "1"]          # out-of-bag: honesto no treino

tab_importancia <- tibble(variavel = names(modelo_rf$variable.importance),
                          importancia_permutacao = modelo_rf$variable.importance) |>
  arrange(desc(importancia_permutacao))


# =============================================================================
# 6. MÉTRICAS
# =============================================================================

tab_metricas <- bind_rows(
  resumo_metricas(treino$target, p_log_treino, "Treino", "Logística (WoE)"),
  resumo_metricas(teste$target,  p_log_teste,  "Teste",  "Logística (WoE)"),
  resumo_metricas(treino$target, p_rf_oob,     "Treino (OOB)", "Random Forest"),
  resumo_metricas(teste$target,  p_rf_teste,   "Teste",  "Random Forest")
)
print(tab_metricas)

# Validação cruzada (5 folds) no treino: estabilidade das métricas.
# O binning é refeito dentro de cada fold, para não vazar informação.
folds <- sample(rep(1:5, length.out = nrow(treino)))
tab_cv <- bind_rows(lapply(1:5, function(k) {
  tr <- treino[folds != k, ]; va <- treino[folds == k, ]
  b <- setNames(lapply(selecionadas, function(v) {
    ct <- binning_monotonico(tr[[v]], tr$target)
    list(cortes = ct, tabela = calcular_woe(tr[[v]], tr$target, ct, v))
  }), selecionadas)
  trw <- aplicar_woe(tr, b); vaw <- aplicar_woe(va, b)
  m <- glm(formula, data = trw, family = binomial)
  rf <- ranger(x = as.data.frame(tr[VARS]), y = factor(tr$target), probability = TRUE,
               num.trees = 300, mtry = 3, min.node.size = 50, seed = SEMENTE)
  bind_rows(
    resumo_metricas(va$target, predict(m, vaw, type = "response"), paste("Fold", k), "Logística (WoE)"),
    resumo_metricas(va$target, predict(rf, as.data.frame(va[VARS]))$predictions[, "1"],
                    paste("Fold", k), "Random Forest")
  )
}))
tab_cv_resumo <- tab_cv |>
  group_by(modelo) |>
  summarise(auc_media = mean(auc), auc_dp = sd(auc), ks_medio = mean(ks), ks_dp = sd(ks),
            .groups = "drop")
print(tab_cv_resumo)

# Ponto de corte (escolhido no treino, aplicado no teste)
corte <- corte_ks(treino$target, p_log_treino)
tab_confusao <- bind_rows(
  matriz_confusao(treino$target, p_log_treino, corte, "Treino"),
  matriz_confusao(teste$target,  p_log_teste,  corte, "Teste")
)
print(tab_confusao)

tab_decis_log <- tabela_decis(teste$target, p_log_teste) |> mutate(modelo = "Logística (WoE)", .before = 1)
tab_decis_rf  <- tabela_decis(teste$target, p_rf_teste)  |> mutate(modelo = "Random Forest", .before = 1)


# =============================================================================
# 7. SCORE E FAIXAS DE RISCO
# =============================================================================
# Score de 0 a 1000: quanto maior, menor o risco (score = 1000 x (1 - p)).
# Faixas definidas pelos quintis do score no treino e aplicadas ao teste.

score <- function(p) round(1000 * (1 - p))
score_treino <- score(p_log_treino)
score_teste  <- score(p_log_teste)

cortes_faixa <- quantile(score_treino, probs = c(0.2, 0.4, 0.6, 0.8), names = FALSE)
faixa_risco <- function(s) cut(s, c(-Inf, cortes_faixa, Inf), labels = c("E", "D", "C", "B", "A"), right = FALSE)

tab_faixas <- bind_rows(
  tibble(amostra = "Treino", faixa = faixa_risco(score_treino), y = treino$target, s = score_treino),
  tibble(amostra = "Teste",  faixa = faixa_risco(score_teste),  y = teste$target,  s = score_teste)
) |>
  group_by(amostra, faixa) |>
  summarise(n = n(), maus = sum(y),
            taxa_target = mean(y), score_min = min(s), score_max = max(s), .groups = "drop_last") |>
  mutate(pct_clientes = n / sum(n)) |>
  ungroup() |>
  arrange(amostra, desc(faixa))

psi_score <- calc_psi(score_treino, score_teste)
cat("\nPSI do score (treino x teste):", round(psi_score, 4), "\n")
print(tab_faixas)


# =============================================================================
# 8. GRÁFICOS
# =============================================================================

curva_roc <- function(y, p, modelo) {
  cortes <- sort(unique(p), decreasing = TRUE)
  tibble(modelo = modelo,
         fpr = c(0, sapply(cortes, function(k) mean(p[y == 0] >= k))),
         tpr = c(0, sapply(cortes, function(k) mean(p[y == 1] >= k))))
}
roc <- bind_rows(curva_roc(teste$target, p_log_teste, "Logística (WoE)"),
                 curva_roc(teste$target, p_rf_teste, "Random Forest"))
auc_rot <- tab_metricas |> filter(amostra == "Teste") |>
  mutate(rotulo = sprintf("%s (AUC %.3f)", modelo, auc))
roc <- roc |> left_join(select(auc_rot, modelo, rotulo), by = "modelo")

g_roc <- ggplot(roc, aes(fpr, tpr, colour = rotulo)) +
  geom_abline(linetype = "dashed", colour = "grey60") +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("#1F6FB2", "#9AA5B1"), name = NULL) +
  labs(title = "Curva ROC na amostra de teste", x = "Taxa de falsos positivos", y = "Taxa de verdadeiros positivos") +
  coord_equal() + theme_minimal(base_size = 12) + theme(legend.position = "bottom")
ggsave(file.path(DIR_FIG, "roc_teste.png"), g_roc, width = 6.5, height = 6.5, dpi = 150, bg = "white")

g_iv <- ggplot(tab_iv, aes(x = iv, y = reorder(variavel, iv), fill = variavel %in% selecionadas)) +
  geom_col() +
  geom_vline(xintercept = 0.02, linetype = "dashed", colour = "grey50") +
  scale_fill_manual(values = c(`TRUE` = "#1F6FB2", `FALSE` = "#B8C2CC"),
                    labels = c(`TRUE` = "No modelo", `FALSE` = "Fora do modelo"), name = NULL) +
  labs(title = "Information Value (IV) por variável", x = "IV (treino)", y = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
ggsave(file.path(DIR_FIG, "iv_variaveis.png"), g_iv, width = 7, height = 4.5, dpi = 150, bg = "white")

g_faixas <- tab_faixas |> filter(amostra == "Teste") |>
  ggplot(aes(x = faixa, y = taxa_target)) +
  geom_col(fill = "#1F6FB2") +
  geom_hline(yintercept = mean(teste$target), linetype = "dashed", colour = "grey40") +
  geom_text(aes(label = scales::percent(taxa_target, accuracy = 0.1)), vjust = -0.4) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Taxa de TARGET por faixa de risco (teste)", x = "Faixa (A = menor risco)", y = "Taxa de TARGET") +
  theme_minimal(base_size = 12)
ggsave(file.path(DIR_FIG, "faixas_risco.png"), g_faixas, width = 7, height = 4.5, dpi = 150, bg = "white")


# =============================================================================
# 9. EXPORTAÇÃO
# =============================================================================

tab_selecao <- tibble(
  etapa = c("Candidatas (IV >= 0,02)", "Removidas por correlação", "Variáveis finais"),
  variaveis = c(paste(candidatas, collapse = ", "), paste(removidas_cor, collapse = ", "),
                paste(selecionadas, collapse = ", "))
)

abas <- list(
  "Descritiva"        = descritiva,
  "Correlacoes"       = correlacoes,
  "IV"                = tab_iv,
  "WoE por faixa"     = tab_woe |> mutate(faixa = as.character(faixa)),
  "Selecao"           = tab_selecao,
  "Coeficientes"      = tab_coef,
  "Metricas"          = tab_metricas,
  "Validacao cruzada" = tab_cv,
  "Matriz confusao"   = tab_confusao,
  "Decis teste"       = bind_rows(tab_decis_log, tab_decis_rf),
  "Faixas de risco"   = tab_faixas |> mutate(faixa = as.character(faixa)),
  "Importancia RF"    = tab_importancia,
  "Estabilidade"      = tibble(indicador = "PSI do score (treino x teste)", valor = psi_score)
)

wb <- createWorkbook()
estilo_cab <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2", border = "Bottom")
for (nome in names(abas)) {
  tabela <- abas[[nome]] |> mutate(across(where(is.numeric), \(x) round(x, 4)))
  addWorksheet(wb, nome)
  writeData(wb, nome, tabela, headerStyle = estilo_cab)
  setColWidths(wb, nome, cols = seq_along(tabela), widths = "auto")
  freezePane(wb, nome, firstRow = TRUE)
}
saveWorkbook(wb, file.path(DIR_SAIDA, "resultados_modelo.xlsx"), overwrite = TRUE)

cat("\nArquivos gerados em", DIR_SAIDA, "\n")

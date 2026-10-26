# [Case] Modelagem de Riscos – Modelo preditivo do TARGET

Este repositório traz um modelo de classificação em R para prever o `TARGET` (11,5% da base de 10.000 clientes) a partir de 8 variáveis anônimas (v1–v8).

**Resultado:** regressão logística com variáveis em WoE (scorecard), com 6 variáveis. Na amostra de teste ela tem **AUC 0,807, Gini 0,613 e KS 47,2**, desempenho igual ou melhor que o de um Random Forest. A validação cruzada (5 folds) dá AUC 0,823 ± 0,019.

## Estrutura

```
├── case-modelagem-riscos.Rproj
├── scripts/
│   └── modelo_risco.R          todo o processo, de ponta a ponta
├── data/
│   └── raw/                    base original do case (não editar)
├── output/
│   ├── resultados_modelo.xlsx  tabelas: descritiva, IV, WoE, coeficientes, métricas, faixas…
│   └── figuras/                curva ROC, IV por variável, taxa por faixa de risco
└── codigo_modelo_risco.txt     cópia do script em .txt (formato pedido no case)
```

## Como rodar

1. Abra o `case-modelagem-riscos.Rproj` no RStudio.
2. Instale os pacotes:
   ```r
   install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "ranger", "openxlsx"))
   ```
   Se o projeto tiver um `renv.lock`, use `renv::restore()` no lugar.
3. Rode o script, que leva menos de 1 minuto:
   ```r
   source("scripts/modelo_risco.R")
   ```

## Metodologia

1. **Análise descritiva:** distribuição de cada variável, % de zeros, correlações e taxa de TARGET por faixa.
2. **Treino e teste:** divisão 70/30, estratificada pelo TARGET (semente 2026).
3. **Binning monotônico + WoE/IV:**
   - cada variável é dividida em até 10 faixas;
   - faixas vizinhas são unidas até a taxa de TARGET ser monotônica e toda faixa ter pelo menos 2% dos clientes;
   - tudo é calculado só no treino.
4. **Seleção de variáveis:**
   - IV ≥ 0,02;
   - entre variáveis com correlação acima de 0,7, fica a de maior IV (v6 saiu, por ser redundante com v5);
   - sinal esperado e p-valor < 0,05 (v3 saiu);
   - VIF abaixo de 1,07.
5. **Modelos:** regressão logística sobre o WoE (o escolhido) e Random Forest (`ranger`, 500 árvores) para comparação.
6. **Métricas:**
   - AUC, Gini, KS e Brier;
   - matriz de confusão no corte que maximiza o KS;
   - calibração e captura por decil;
   - PSI entre treino e teste;
   - validação cruzada em 5 folds, com o binning refeito em cada fold.
7. **Score e faixas:** o score vai de 0 a 1000 e é calculado como 1000 × (1 − P). As faixas A–E vêm dos quintis do treino. No teste, a taxa de TARGET vai de 1,8% na faixa A a 32,1% na faixa E.

## Principais resultados (teste)

| Modelo | AUC | Gini | KS |
|---|---|---|---|
| **Logística (WoE)** | **0,807** | **0,613** | **47,2** |
| Random Forest | 0,803 | 0,607 | 46,3 |

As variáveis mais fortes são:
- **v5/v6 > 0:** 4,8% dos clientes, com 73% de TARGET;
- **v8:** valores maiores indicam menos risco;
- **v7:** valores maiores indicam mais risco.

## Documentos

- [Documento técnico (PDF)](docs/Case_Modelagem_de_Riscos.pdf)
- [Apresentação (PDF)](docs/Apresentacao_Case_Modelagem_de_Riscos.pdf)

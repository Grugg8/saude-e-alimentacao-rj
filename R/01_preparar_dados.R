# ===========================================================================
# 01_preparar_dados.R — das tabelas do relatório para os dados do site
# ===========================================================================
# Lê as saídas existentes em 03_Saídas/02_Tabelas, converte para formato tidy
# (decimal com ponto, uma linha por estimativa) e grava em 04_Página/data.
# Reproduz os gradientes socioeconômicos com o MESMO desenho do relatório e
# confere as âncoras numéricas antes de gravar qualquer coisa.
#
# Rodar na raiz do projeto: Rscript 04_Página/R/01_preparar_dados.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(tibble)
})

# Carrega os helpers a partir do próprio arquivo, não do diretório atual.
.args <- commandArgs(trailingOnly = FALSE)
.arquivo <- sub("^--file=", "", .args[grepl("^--file=", .args)])
if (length(.arquivo) == 0) .arquivo <- file.path(getwd(), "04_Página", "R", "01_preparar_dados.R")
source(file.path(dirname(normalizePath(.arquivo)), "00_comum.R"))

cat("Raiz do projeto:", raiz, "\n")

# ---------------------------------------------------------------------------
# 1. Perfil sociodemográfico
# ---------------------------------------------------------------------------
perfil <- ler_tabela("analise_perfil_socio.csv") |>
  tidy_grupos(unidade = "pct") |>
  mutate(
    unidade = case_when(
      bloco == "Idade (anos)" ~ "anos",
      grepl("Renda domiciliar", bloco) ~ "reais",
      TRUE ~ "pct"
    ),
    tipo_rotulo = case_when(
      categoria == "Média (IC 95%)" ~ "Média",
      categoria == "Mediana [P25–P75]" ~ "Mediana",
      TRUE ~ categoria
    )
  ) |>
  select(bloco, categoria = tipo_rotulo, grupo, n, est, li, ls, unidade, tipo)

# ---------------------------------------------------------------------------
# 2. Exposições alimentares
# ---------------------------------------------------------------------------
exposicoes <- ler_tabela("analise_exposicoes.csv") |>
  tidy_grupos(unidade = "pct") |>
  mutate(unidade = if_else(grepl("Média de grupos", categoria), "grupos", "pct")) |>
  select(bloco, categoria, grupo, n, est, li, ls, unidade, tipo)

# ---------------------------------------------------------------------------
# 3. Desfechos
# ---------------------------------------------------------------------------
desfechos <- ler_tabela("analise_desfechos.csv") |>
  tidy_grupos(unidade = "pct") |>
  select(bloco, categoria, grupo, n, est, li, ls, unidade, tipo)

# ---------------------------------------------------------------------------
# 4. Contagens e zeros
# ---------------------------------------------------------------------------
contagens <- ler_tabela("analise_contagens.csv") |>
  tidy_grupos(unidade = "contagem") |>
  mutate(
    unidade = case_when(
      grepl("Zero", categoria) ~ "pct",
      grepl("Percentil", categoria) ~ "contagem",
      grepl("Mediana", categoria) ~ "contagem",
      TRUE ~ "contagem"
    )
  ) |>
  select(bloco, categoria, grupo, n, est, li, ls, unidade, tipo)

zeros <- ler_tabela("analise_zeros_resposta.csv") |>
  tidy_grupos(unidade = "pct") |>
  select(bloco, categoria, grupo, n, est, li, ls, unidade, tipo)

# ---------------------------------------------------------------------------
# 5. Acordo entre percepção e diagnóstico
# ---------------------------------------------------------------------------
acordo_bruto <- ler_tabela("analise_percepcao_diagnostico.csv") |>
  mutate(
    grupo = str_extract(bloco, "^(Total|Homens|Mulheres)"),
    kappa = num_br(str_match(bloco, "kappa = ([0-9,]+)")[, 2]),
    kappa_ep = num_br(str_match(bloco, "EP ([0-9,]+)")[, 2]),
    n = num_br(n)
  )

parte_nao <- separa_celula(acordo_bruto$nao) |>
  transmute(nao_est = est, nao_li = li, nao_ls = ls)
parte_sim <- separa_celula(acordo_bruto$sim) |>
  transmute(sim_est = est, sim_li = li, sim_ls = ls)

acordo <- bind_cols(
  acordo_bruto |> select(grupo, percepcao, n, kappa, kappa_ep),
  parte_nao,
  parte_sim
)

# ---------------------------------------------------------------------------
# 6. Testes bivariados
# ---------------------------------------------------------------------------
testes <- ler_tabela("analise_testes.csv") |>
  mutate(
    n = num_br(n),
    ref = num_br(ref),
    comp = num_br(comp),
    p_num = num_br(p_num),
    q_num = num_br(q_num)
  ) |>
  select(familia, vista, desfecho, comparacao, teste, estatistica,
         n, ref, comp, p, q, p_num, q_num)

# ---------------------------------------------------------------------------
# 7. Modelos
# ---------------------------------------------------------------------------
modelos <- ler_tabela("analise_modelos.csv") |>
  mutate(across(c(estim, ep, li, ls, p_num, q_num, gl, dispersao), num_br)) |>
  select(eixo, medida, ajuste, exposicao, efeito, estim, ep, li, ls,
         p, q, p_num, q_num, gl, dispersao)

vif <- ler_tabela("analise_modelos_vif.csv") |>
  mutate(gvif_aj = num_br(gvif_aj))

# ---------------------------------------------------------------------------
# 8. Fluxo amostral
# ---------------------------------------------------------------------------
fluxo <- ler_tabela("fluxo_amostra.csv") |>
  mutate(
    etapa = str_trim(etapa),
    n = num_br(n),
    percentual_do_anterior = num_br(percentual_do_anterior)
  )

# ---------------------------------------------------------------------------
# 9. Gradientes socioeconômicos
# ---------------------------------------------------------------------------
# Replica a seção de gradientes do script do relatório: mesmo objeto de desenho
# (pós-estratificado), mesmo subconjunto, mesmo svyby e mesmo t crítico. Não é
# uma análise nova: é a mesma estimativa, exportada em formato tidy.

suppressPackageStartupMessages(library(survey))
options(survey.lonely.psu = "adjust")
options(survey.adjust.domain.lonely = TRUE)

desenho_br <- readRDS(file.path(raiz, "01_Dados", "pns2019_morador_selec.rds"))
desenho <- suppressWarnings(subset(
  desenho_br, V0001 == "Rio de Janeiro" & C008 >= 18 & C008 <= 59
))
selecionados <- is.finite(desenho$prob)

base <- readRDS(file.path(raiz, "01_Dados", "pns2019_rj_18a59_analise.rds"))
stopifnot(
  sum(selecionados) == nrow(base),
  identical(as.character(desenho$variables$UPA_PNS[selecionados]), as.character(base$UPA_PNS)),
  identical(as.integer(desenho$variables$C008[selecionados]), as.integer(base$C008))
)

# Anexa as derivadas apenas às linhas com peso finito.
derivadas <- c("doenca_cronica", "consultas_12m", "teve_internacao")
for (v in derivadas) desenho$variables[[v]] <- NA_real_
desenho$variables$doenca_cronica[selecionados]  <- as.numeric(base$doenca_cronica)
desenho$variables$consultas_12m[selecionados]   <- as.numeric(base$consultas_12m)
desenho$variables$teve_internacao[selecionados] <- as.numeric(base$teve_internacao)

desenho$variables$escolaridade <- factor(
  rep(NA_character_, nrow(desenho$variables)),
  levels = c("sem instrução", "alfabetização", "fundamental", "médio", "superior")
)
desenho$variables$escolaridade[selecionados] <- as.character(base$escolaridade)

desenho$variables$faixa_etaria <- factor(
  rep(NA_character_, nrow(desenho$variables)),
  levels = c("18 a 29", "30 a 44", "45 a 59")
)
desenho$variables$faixa_etaria[selecionados] <- as.character(base$faixa_etaria)

rotulos_estrato <- list(
  faixa_etaria = c("18 a 29", "30 a 44", "45 a 59"),
  escolaridade = c("Sem instrução", "Alfabetização", "Fundamental", "Médio", "Superior")
)
mapa_faixa <- c(
  "18 a 29" = "18 a 29 anos",
  "30 a 44" = "30 a 44 anos",
  "45 a 59" = "45 a 59 anos"
)
mapa_escolaridade <- c(
  "sem instrução" = "Sem instrução",
  "alfabetização" = "Alfabetização",
  "fundamental" = "Fundamental",
  "médio" = "Médio",
  "superior" = "Superior"
)

rotulos_desfecho <- c(
  doenca_cronica = "Doença crônica composta",
  consultas_12m = "Consultas em 12 meses",
  teve_internacao = "Internação em 12 meses"
)
unidade_desfecho <- c(
  doenca_cronica = "pct", consultas_12m = "contagem", teve_internacao = "pct"
)

t_critico <- qt(0.975, degf(desenho))
cat("Desenho:", sum(selecionados), "registros com peso;", degf(desenho), "graus de liberdade\n")

gradientes <- bind_rows(lapply(names(rotulos_desfecho), function(out) {
  bind_rows(lapply(c("faixa_etaria", "escolaridade"), function(strato) {
    bind_rows(lapply(c("Homens", "Mulheres"), function(sx) {
      dd <- suppressWarnings(subset(desenho, C006 == ifelse(sx == "Homens", "Homem", "Mulher")))
      by <- suppressWarnings(svyby(
        as.formula(paste0("~", out)), as.formula(paste0("~", strato)),
        dd, svymean, na.rm = TRUE
      ))
      categoria <- as.character(by[[strato]])
      if (strato == "escolaridade") {
        categoria <- unname(mapa_escolaridade[categoria])
      } else {
        categoria <- unname(mapa_faixa[categoria])
      }
      est <- as.numeric(by[[out]])
      se  <- as.numeric(by$se)
      ok <- selecionados &
        !is.na(desenho$variables[[out]]) &
        desenho$variables$C006 == ifelse(sx == "Homens", "Homem", "Mulher")
      n_cat <- as.integer(table(factor(
        as.character(desenho$variables[[strato]])[ok],
        levels = if (strato == "escolaridade") names(mapa_escolaridade) else rotulos_estrato[[strato]]
      )))
      tibble(
        eixo = out,
        desfecho = unname(rotulos_desfecho[[out]]),
        sexo = sx,
        estrato = if (strato == "faixa_etaria") "Faixa etária" else "Escolaridade",
        categoria = categoria,
        n = n_cat,
        est = est,
        li = est - t_critico * se,
        ls = est + t_critico * se,
        unidade = unname(unidade_desfecho[[out]])
      )
    }))
  }))
})) |>
  filter(!is.na(est)) |>
  # Percentuais saem em 0-100; contagens ficam na escala original.
  mutate(across(c("est", "li", "ls"), ~ if_else(.data$unidade == "pct", 100 * .x, .x))) |>
  # Células com menos de 30 pessoas não são exibidas: o IC fica degenerado e a
  # estimativa não é informativa (ex.: n = 5, 100% com IC 100-100).
  mutate(exibir = n >= 30)

# ---------------------------------------------------------------------------
# 10. Conferência das âncoras
# ---------------------------------------------------------------------------
totais <- tibble(
  indicador = c("Doença crônica composta (%)", "Consultas em 12 meses (média)",
                "Internação em 12 meses (%)", "Idade média (anos)"),
  estimado = c(
    100 * as.numeric(coef(svymean(~doenca_cronica, desenho, na.rm = TRUE))),
    as.numeric(coef(svymean(~consultas_12m, desenho, na.rm = TRUE))),
    100 * as.numeric(coef(svymean(~teve_internacao, desenho, na.rm = TRUE))),
    as.numeric(coef(svymean(~C008, desenho, na.rm = TRUE)))
  ),
  esperado = c(29.5, 3.11, 5.8, 38.8),
  tolerancia = c(0.15, 0.02, 0.15, 0.15)
) |>
  mutate(ok = abs(estimado - esperado) <= tolerancia)

print(totais, n = Inf)
if (!all(totais$ok)) {
  stop("As âncoras não conferem com o relatório. Nada foi gravado.")
}

# ---------------------------------------------------------------------------
# 11. Gravação dos dados tidy
# ---------------------------------------------------------------------------
grava <- function(df, nome) {
  readr::write_csv(df, file.path(dir_dados, nome), na = "")
  cat("  data/", nome, " (", nrow(df), " linhas)\n", sep = "")
}

perfil |> grava("perfil.csv")
exposicoes |> grava("exposicoes.csv")
desfechos |> grava("desfechos.csv")
contagens |> grava("contagens.csv")
zeros |> grava("zeros.csv")
acordo |> grava("acordo.csv")
testes |> grava("testes.csv")
modelos |> grava("modelos.csv")
vif |> grava("vif.csv")
fluxo |> grava("fluxo.csv")
gradientes |> grava("gradientes.csv")
totais |> grava("totais.csv")

# ---------------------------------------------------------------------------
# 12. Tabelas markdown para o site
# ---------------------------------------------------------------------------
# Matriz dos testes: uma linha por desfecho, uma coluna por exposição, célula
# com o q de Benjamini-Hochberg. O asterisco marca q < 0,05.

ordem_exposicoes <- c("Feijão", "Frutas", "Verduras e legumes", "Doces",
                      "Ultraprocessados (10 itens)")

matriz <- function(familia, vista) {
  d <- testes |> filter(.data$familia == .env$familia, .data$vista == .env$vista)
  if (nrow(d) == 0) return(NULL)
  d <- d |>
    mutate(
      celula = if_else(!is.na(q_num) & q_num < 0.05, paste0(q, " *"), q)
    ) |>
    select(desfecho, comparacao, celula) |>
    pivot_wider(names_from = comparacao, values_from = celula)
  cols <- c("desfecho", intersect(ordem_exposicoes, names(d)))
  d |> select(all_of(cols)) |> rename(Desfecho = desfecho)
}

for (vista in c("direta", "mulheres", "homens")) {
  vista_csv <- c(direta = "Direta", mulheres = "Mulheres", homens = "Homens")[[vista]]
  m <- matriz("Núcleo", vista_csv)
  escreve_md(m, paste0("matriz_nucleo_", vista, ".md"), align = c("l", "r", "r", "r", "r", "r"))
  m <- matriz("Exploratória", vista_csv)
  escreve_md(m, paste0("matriz_exploratoria_", vista, ".md"), align = c("l", "r", "r", "r", "r", "r"))
}

# Família sexo: nove comparações. O indicador está em `desfecho` (na família
# sexo, `comparacao` é sempre "Sexo"). Percentuais com uma decimal; médias com
# duas.
escreve_md(
  testes |>
    filter(familia == "Sexo") |>
    mutate(
      unidade = case_when(
        desfecho %in% c("Consultas em 12 meses", "Internações em 12 meses") ~ "media",
        desfecho == "Idade (anos)" ~ "anos",
        TRUE ~ "pct"
      ),
      Homens = if_else(unidade %in% c("pct", "anos"), fmt_num(ref, 1), fmt_num(ref, 2)),
      Mulheres = if_else(unidade %in% c("pct", "anos"), fmt_num(comp, 1), fmt_num(comp, 2)),
      `q` = if_else(!is.na(q_num) & q_num < 0.05, paste0(q, " *"), q)
    ) |>
    select(Indicador = desfecho, Homens, Mulheres, `q (BH)` = `q`),
  "sexo.md", align = c("l", "r", "r", "r")
)

# Modelos: efeito entre homens e entre mulheres, por exposição.
rotulos_exposicao <- c(
  feijao_regular = "Feijão",
  frutas_regular = "Frutas",
  hortalicas_regular = "Verduras e legumes",
  doces_regular = "Doces",
  ultraprocessados_5mais = "Ultraprocessados (10 itens)"
)
ordem_exposicao <- names(rotulos_exposicao)

tabela_modelo <- function(eixo, medida) {
  d <- modelos |> filter(.data$eixo == .env$eixo, .data$ajuste == "Mútuo")
  h <- d |> filter(efeito == "Exposição")
  m <- d |> filter(efeito == "Efeito entre mulheres")
  tibble(
    Exposição = unname(rotulos_exposicao[h$exposicao]),
    Homens = fmt_ic(h$estim, h$li, h$ls, 2, ""),
    Mulheres = fmt_ic(m$estim, m$li, m$ls, 2, "")
  ) |>
    arrange(match(Exposição, unname(rotulos_exposicao[ordem_exposicao])))
}

escreve_md(tabela_modelo("composta", "OR"), "modelos_composta.md", c("l", "r", "r"))
escreve_md(tabela_modelo("consultas", "RC"), "modelos_consultas.md", c("l", "r", "r"))
escreve_md(tabela_modelo("internacao", "OR"), "modelos_internacao.md", c("l", "r", "r"))

# Fluxo amostral.
escreve_md(
  fluxo |>
    mutate(
      Etapa = if_else(etapa %in% c("homens", "mulheres"), str_to_title(etapa), etapa),
      Pessoas = format(n, big.mark = ".", decimal.mark = ",", scientific = FALSE),
      `% da etapa anterior` = if_else(
        is.na(percentual_do_anterior), "—",
        paste0(fmt_num(percentual_do_anterior, 1), "%")
      )
    ) |>
    select(Etapa, Pessoas, `% da etapa anterior`),
  "fluxo.md", align = c("l", "r", "r")
)

# Gradientes em tabela, para o bloco recolhível.
escreve_md(
  gradientes |>
    filter(estrato == "Escolaridade", exibir) |>
    mutate(
      `Estimativa (IC 95%)` = if_else(
        unidade == "pct",
        fmt_ic(est, li, ls, 1),
        fmt_ic(est, li, ls, 2, "")
      ),
      `n` = format(n, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    ) |>
    select(Desfecho = desfecho, Sexo = sexo, Escolaridade = categoria,
           `Estimativa (IC 95%)`, n),
  "gradientes.md", align = c("l", "l", "l", "r", "r")
)

# ---------------------------------------------------------------------------
# 13. Cartões de indicadores (markdown com spans do Quarto)
# ---------------------------------------------------------------------------
cartao <- function(valor, rotulo, fonte = NULL) {
  c(
    "::: {.cartao}",
    paste0("[", valor, "]{.valor}"),
    paste0("[", rotulo, "]{.rotulo}"),
    if (!is.null(fonte)) paste0("[", fonte, "]{.fonte}"),
    ":::"
  )
}
cartoes <- function(...) c("::: {.cartoes}", unlist(list(...)), ":::")

filtra_celula <- function(df, categoria, grupo, bloco) {
  d <- df |> filter(.data$categoria == .env$categoria, .data$grupo == .env$grupo)
  if (!is.null(bloco)) d <- d |> filter(.data$bloco == .env$bloco)
  stopifnot(nrow(d) == 1)
  d
}
valor_grupo <- function(df, categoria, grupo, digitos = 1, pct = TRUE, bloco = NULL) {
  d <- filtra_celula(df, categoria, grupo, bloco)
  if (pct) fmt_pct(d$est, digitos) else fmt_num(d$est, digitos)
}
fonte_grupo <- function(df, categoria, grupo, digitos = 1, bloco = NULL) {
  d <- filtra_celula(df, categoria, grupo, bloco)
  sprintf("IC 95%% %s–%s", fmt_num(d$li, digitos), fmt_num(d$ls, digitos))
}

writeLines(
  cartoes(
    cartao("3.242", "pessoas entrevistadas no recorte", "18 a 59 anos, estado do RJ"),
    cartao(valor_grupo(desfechos, "Doença crônica composta (ao menos uma)", "Total"),
           "têm ao menos uma doença crônica",
           fonte_grupo(desfechos, "Doença crônica composta (ao menos uma)", "Total")),
    cartao(valor_grupo(desfechos, "Consultou ao menos uma vez", "Total"),
           "consultaram médico nos últimos 12 meses",
           fonte_grupo(desfechos, "Consultou ao menos uma vez", "Total")),
    cartao(valor_grupo(desfechos, "Internou ao menos uma vez", "Total"),
           "internaram nos últimos 12 meses",
           fonte_grupo(desfechos, "Internou ao menos uma vez", "Total"))
  ),
  file.path(dir_tabelas, "cartoes_index.md"), useBytes = TRUE
)

writeLines(
  cartoes(
    cartao(valor_grupo(exposicoes, "Feijão", "Total"), "comem feijão 5 ou mais dias por semana",
           fonte_grupo(exposicoes, "Feijão", "Total")),
    cartao(valor_grupo(exposicoes, "Frutas", "Total"), "comem frutas 5 ou mais dias por semana",
           fonte_grupo(exposicoes, "Frutas", "Total")),
    cartao(valor_grupo(exposicoes, "Verduras e legumes", "Total"), "comem verduras ou legumes 5 ou mais dias por semana",
           fonte_grupo(exposicoes, "Verduras e legumes", "Total")),
    cartao(valor_grupo(exposicoes, "Ultraprocessados (10 itens)", "Total"), "comeram 5 ou mais grupos de ultraprocessados no dia anterior",
           fonte_grupo(exposicoes, "Ultraprocessados (10 itens)", "Total"))
  ),
  file.path(dir_tabelas, "cartoes_alimentacao.md"), useBytes = TRUE
)

writeLines(
  cartoes(
    cartao(valor_grupo(desfechos, "Hipertensão", "Total"), "têm hipertensão diagnosticada",
           fonte_grupo(desfechos, "Hipertensão", "Total")),
    cartao(valor_grupo(desfechos, "Diabetes", "Total"), "têm diabetes diagnosticada",
           fonte_grupo(desfechos, "Diabetes", "Total")),
    cartao(valor_grupo(desfechos, "Depressão (diagnóstico)", "Total"), "têm diagnóstico de depressão",
           fonte_grupo(desfechos, "Depressão (diagnóstico)", "Total")),
    cartao(valor_grupo(contagens, "Média (IC 95%)", "Total", 2, pct = FALSE,
                       bloco = "Consultas médicas em 12 meses"),
           "consultas médicas por pessoa em 12 meses",
           fonte_grupo(contagens, "Média (IC 95%)", "Total", 2,
                       bloco = "Consultas médicas em 12 meses"))
  ),
  file.path(dir_tabelas, "cartoes_saude.md"), useBytes = TRUE
)

writeLines(
  cartoes(
    cartao(valor_grupo(desfechos, "Doença crônica composta (ao menos uma)", "Mulheres"),
           "das mulheres têm ao menos uma doença crônica",
           fonte_grupo(desfechos, "Doença crônica composta (ao menos uma)", "Mulheres")),
    cartao(valor_grupo(desfechos, "Doença crônica composta (ao menos uma)", "Homens"),
           "dos homens têm ao menos uma doença crônica",
           fonte_grupo(desfechos, "Doença crônica composta (ao menos uma)", "Homens")),
    cartao(valor_grupo(contagens, "Média (IC 95%)", "Mulheres", 2, pct = FALSE,
                       bloco = "Consultas médicas em 12 meses"),
           "consultas por mulher em 12 meses",
           fonte_grupo(contagens, "Média (IC 95%)", "Mulheres", 2,
                       bloco = "Consultas médicas em 12 meses")),
    cartao(valor_grupo(contagens, "Média (IC 95%)", "Homens", 2, pct = FALSE,
                       bloco = "Consultas médicas em 12 meses"),
           "consultas por homem em 12 meses",
           fonte_grupo(contagens, "Média (IC 95%)", "Homens", 2,
                       bloco = "Consultas médicas em 12 meses"))
  ),
  file.path(dir_tabelas, "cartoes_homens.md"), useBytes = TRUE
)

cat("Dados, tabelas e cartões gravados em 04_Página.\n")

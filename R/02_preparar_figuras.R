# ===========================================================================
# 02_preparar_figuras.R — figuras do site
# ===========================================================================
# Todas as figuras são geradas a partir de 04_Página/data (que vem das saídas
# do relatório) e gravadas em SVG. Nenhuma estimativa é recalculada aqui.
#
# Rodar na raiz do projeto: Rscript 04_Página/R/02_preparar_figuras.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(ggplot2)
  library(scales)
})

.args <- commandArgs(trailingOnly = FALSE)
.arquivo <- sub("^--file=", "", .args[grepl("^--file=", .args)])
if (length(.arquivo) == 0) .arquivo <- file.path(getwd(), "04_Página", "R", "02_preparar_figuras.R")
source(file.path(dirname(normalizePath(.arquivo)), "00_comum.R"))

ler_dado <- function(nome) {
  readr::read_csv(file.path(dir_dados, nome), show_col_types = FALSE)
}

perfil     <- ler_dado("perfil.csv")
exposicoes <- ler_dado("exposicoes.csv")
desfechos  <- ler_dado("desfechos.csv")
contagens  <- ler_dado("contagens.csv")
zeros      <- ler_dado("zeros.csv")
acordo     <- ler_dado("acordo.csv")
modelos    <- ler_dado("modelos.csv")
gradientes <- ler_dado("gradientes.csv")

# ---------------------------------------------------------------------------
# Helpers de desenho
# ---------------------------------------------------------------------------
quebra <- function(x, largura = 26) {
  vapply(x, function(um) paste(strwrap(um, largura), collapse = "\n"), character(1))
}

# Barras horizontais com IC e rótulo direto. Uma faceta por sexo, mesma escala
# nos dois painéis (comparação visual honesta).
barras_sexo <- function(df, ordem = NULL, sufixo = "%", digitos = 1,
                        largura_rotulo = 26, extra = 0.28) {
  df <- df |>
    filter(grupo %in% c("Homens", "Mulheres"), !is.na(est)) |>
    mutate(
      grupo = factor(grupo, levels = c("Homens", "Mulheres")),
      rotulo = quebra(categoria, largura_rotulo)
    )

  if (!is.null(ordem)) {
    df$rotulo <- factor(df$rotulo, levels = rev(quebra(ordem, largura_rotulo)))
  } else {
    df$rotulo <- factor(df$rotulo, levels = rev(unique(df$rotulo)))
  }

  df$texto <- paste0(fmt_num(df$est, digitos), sufixo)

  ggplot(df, aes(x = rotulo, y = est)) +
    geom_col(fill = pal[["teal"]], width = 0.6) +
    geom_errorbar(
      aes(ymin = pmax(li, 0, na.rm = TRUE), ymax = ls),
      width = 0.16, colour = pal[["ink"]], linewidth = 0.4, na.rm = TRUE
    ) +
    geom_text(
      aes(y = pmax(ls, est, na.rm = TRUE), label = texto),
      hjust = -0.18, size = 4.1, family = "Helvetica", colour = pal[["ink"]]
    ) +
    coord_flip(clip = "off") +
    facet_wrap(~grupo, ncol = 1) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, extra)),
      labels = function(x) fmt_num(x, 0)
    ) +
    labs(x = NULL, y = NULL) +
    tema_site() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = pal[["rule"]], linewidth = 0.3),
      axis.text.x = element_blank(),
      panel.spacing.y = grid::unit(1.1, "lines"),
      strip.text = element_text(margin = margin(b = 4))
    )
}

# ---------------------------------------------------------------------------
# 1. Perfil: faixa etária, escolaridade, renda e cor ou raça
# ---------------------------------------------------------------------------
f_idade <- perfil |>
  filter(bloco == "Faixa etária") |>
  barras_sexo(ordem = c("18 a 29 anos", "30 a 44 anos", "45 a 59 anos"))
salva_svg(f_idade, "fig_perfil_idade", 6.3, 4.6)

f_escol <- perfil |>
  filter(bloco == "Escolaridade") |>
  barras_sexo(ordem = c("Sem instrução", "Alfabetização", "Fundamental", "Médio", "Superior"))
salva_svg(f_escol, "fig_perfil_escolaridade", 6.3, 5.4)

f_renda <- perfil |>
  filter(bloco == "Faixas de renda (salários mínimos)") |>
  barras_sexo(ordem = c(
    "Até 1/4 SM", "Mais de 1/4 a 1/2 SM", "Mais de 1/2 a 1 SM",
    "Mais de 1 a 2 SM", "Mais de 2 a 3 SM", "Mais de 3 a 5 SM", "Mais de 5 SM"
  ))
salva_svg(f_renda, "fig_perfil_renda", 6.3, 6.6)

f_raca <- perfil |>
  filter(bloco == "Cor ou raça") |>
  barras_sexo(ordem = c("Branca", "Parda", "Preta", "Amarela", "Indígena"))
salva_svg(f_raca, "fig_perfil_raca", 6.3, 5.0)

# ---------------------------------------------------------------------------
# 2. Exposições alimentares
# ---------------------------------------------------------------------------
f_expo <- exposicoes |>
  filter(!grepl("Média de grupos", categoria)) |>
  barras_sexo(ordem = c(
    "Feijão", "Frutas", "Verduras e legumes", "Doces",
    "Ultraprocessados (10 itens)", "Ultraprocessados (8 itens, sensibilidade)"
  ))
salva_svg(f_expo, "fig_exposicoes", 6.3, 6.4)

# ---------------------------------------------------------------------------
# 3. Desfechos: doenças, percepção e uso de serviços
# ---------------------------------------------------------------------------
f_doencas <- desfechos |>
  filter(bloco == "Doença crônica") |>
  barras_sexo(ordem = c(
    "Doença crônica composta (ao menos uma)", "Hipertensão", "Depressão (diagnóstico)",
    "Diabetes", "Doença do coração", "Câncer", "AVC ou derrame"
  ))
salva_svg(f_doencas, "fig_doencas", 6.3, 7.0)

f_percepcao <- desfechos |>
  filter(bloco %in% c("Percepção de saúde", "Uso de serviços")) |>
  barras_sexo(ordem = c(
    "Consultou ao menos uma vez", "Indisposição frequente",
    "Sentir-se deprimido frequente", "Internou ao menos uma vez"
  ))
salva_svg(f_percepcao, "fig_percepcao_uso", 6.3, 5.4)

# Zeros: quem não consultou e quem não internou.
f_zeros <- zeros |>
  filter(bloco == "Zeros", !grepl("alternativa", categoria)) |>
  mutate(categoria = recode(
    categoria,
    "Consultas em 12 meses (contagem)" = "Não consultou nenhuma vez",
    "Internações em 12 meses (contagem)" = "Não internou nenhuma vez"
  )) |>
  barras_sexo(ordem = c("Não consultou nenhuma vez", "Não internou nenhuma vez"))
salva_svg(f_zeros, "fig_zeros", 6.3, 4.2)

# ---------------------------------------------------------------------------
# 4. Percepção x diagnóstico
# ---------------------------------------------------------------------------
f_acordo <- acordo |>
  filter(grupo == "Total") |>
  mutate(
    percepcao = factor(
      percepcao, levels = c("Não frequente", "Frequente"),
      labels = c("Não se sente deprimido\ncom frequência", "Sente-se deprimido\nna maioria dos dias")
    ),
    texto = paste0(fmt_num(sim_est, 1), "%")
  ) |>
  ggplot(aes(x = percepcao, y = sim_est)) +
  geom_col(fill = pal[["terra"]], width = 0.55) +
  geom_errorbar(
    aes(ymin = pmax(sim_li, 0), ymax = sim_ls), width = 0.14,
    colour = pal[["ink"]], linewidth = 0.4, na.rm = TRUE
  ) +
  geom_text(
    aes(y = sim_ls, label = texto), vjust = -0.7, size = 4.6,
    family = "Helvetica", colour = pal[["ink"]]
  ) +
  scale_y_continuous(
    limits = c(0, NA), expand = expansion(mult = c(0, 0.18)),
    labels = function(x) paste0(fmt_num(x, 0), "%")
  ) +
  labs(x = NULL, y = "Com diagnóstico de depressão") +
  tema_site() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = rel(0.95))
  )
salva_svg(f_acordo, "fig_acordo", 5.8, 4.4)

# ---------------------------------------------------------------------------
# 5. Gradientes socioeconômicos
# ---------------------------------------------------------------------------
barras_gradiente <- function(df, sufixo, digitos, ordem, extra = 0.3) {
  df |>
    mutate(
      grupo = factor(sexo, levels = c("Homens", "Mulheres")),
      rotulo = factor(categoria, levels = rev(ordem))
    ) |>
    mutate(texto = paste0(fmt_num(est, digitos), sufixo)) |>
    ggplot(aes(x = rotulo, y = est)) +
    geom_col(fill = pal[["teal"]], width = 0.55) +
    geom_errorbar(
      aes(ymin = pmax(li, 0, na.rm = TRUE), ymax = ls),
      width = 0.15, colour = pal[["ink"]], linewidth = 0.4, na.rm = TRUE
    ) +
    geom_text(
      aes(y = pmax(ls, est, na.rm = TRUE), label = texto),
      hjust = -0.18, size = 4, family = "Helvetica", colour = pal[["ink"]]
    ) +
    coord_flip(clip = "off") +
    facet_wrap(~grupo, ncol = 1) +
    scale_y_continuous(
      limits = c(0, NA), expand = expansion(mult = c(0, extra)),
      labels = function(x) fmt_num(x, 0)
    ) +
    labs(x = NULL, y = NULL) +
    tema_site() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.x = element_blank(),
      panel.spacing.y = grid::unit(1.1, "lines"),
      strip.text = element_text(margin = margin(b = 4))
    )
}

f_grad_idade <- gradientes |>
  filter(eixo == "doenca_cronica", estrato == "Faixa etária", exibir) |>
  barras_gradiente("%", 1, c("18 a 29 anos", "30 a 44 anos", "45 a 59 anos"))
salva_svg(f_grad_idade, "fig_grad_idade", 6.3, 4.6)

f_grad_escol <- gradientes |>
  filter(eixo == "doenca_cronica", estrato == "Escolaridade", exibir) |>
  barras_gradiente("%", 1, c("Fundamental", "Médio", "Superior"))
salva_svg(f_grad_escol, "fig_grad_escolaridade", 6.3, 4.4)

f_grad_cons <- gradientes |>
  filter(eixo == "consultas_12m", estrato == "Escolaridade", exibir) |>
  barras_gradiente("", 2, c("Fundamental", "Médio", "Superior"))
salva_svg(f_grad_cons, "fig_grad_consultas", 6.3, 4.4)

# ---------------------------------------------------------------------------
# 6. Modelos: razões e OR por sexo, nos dois ajustes
# ---------------------------------------------------------------------------
rotulos_exposicao <- c(
  feijao_regular = "Feijão",
  frutas_regular = "Frutas",
  hortalicas_regular = "Verduras e legumes",
  doces_regular = "Doces",
  ultraprocessados_5mais = "Ultraprocessados"
)

floresta <- function(eixo, medida) {
  d <- modelos |>
    filter(.data$eixo == .env$eixo, efeito %in% c("Exposição", "Efeito entre mulheres")) |>
    mutate(
      sexo = if_else(efeito == "Exposição", "Homens", "Mulheres"),
      sexo = factor(sexo, levels = c("Homens", "Mulheres")),
      ajuste = factor(ajuste, levels = c("Marginal", "Mútuo")),
      exposicao = factor(unname(rotulos_exposicao[exposicao]), levels = rev(unname(rotulos_exposicao)))
    )

  ggplot(d, aes(x = estim, y = exposicao, colour = sexo, shape = sexo)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = pal[["muted"]], linewidth = 0.4) +
    geom_pointrange(
      aes(xmin = li, xmax = ls),
      position = position_dodge(width = 0.45), size = 0.35, linewidth = 0.6
    ) +
    facet_wrap(~ajuste, ncol = 1) +
    scale_x_log10(breaks = c(0.5, 0.7, 1, 1.5, 2, 3), labels = function(x) fmt_num(x, 1)) +
    scale_colour_manual(values = c(Homens = pal[["teal"]], Mulheres = pal[["terra"]])) +
    scale_shape_manual(values = c(Homens = 16, Mulheres = 17)) +
    labs(x = paste0(medida, " (escala logarítmica)"), y = NULL, colour = NULL, shape = NULL) +
    tema_site() +
    theme(
      legend.position = "top",
      legend.text = element_text(size = rel(0.85)),
      panel.grid.major.y = element_blank(),
      panel.spacing.y = grid::unit(1.2, "lines"),
      strip.text = element_text(margin = margin(b = 5)),
      axis.text.y = element_text(size = rel(0.95))
    )
}

salva_svg(floresta("composta", "OR"), "fig_modelo_composta", 6.4, 5.2)
salva_svg(floresta("consultas", "Razão de médias"), "fig_modelo_consultas", 6.4, 5.2)
salva_svg(floresta("internacao", "OR"), "fig_modelo_internacao", 6.4, 5.2)

cat("Figuras geradas em", dir_figuras, ":\n")
print(sort(basename(list.files(dir_figuras, pattern = "\\.png$"))))

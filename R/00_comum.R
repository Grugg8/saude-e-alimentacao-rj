# ===========================================================================
# 00_comum.R — caminhos, leitura e formatação
# ===========================================================================
# Este arquivo é carregado pelos outros scripts. Ele localiza a raiz do projeto
# pelo arquivo .Rproj (e não pelo diretório atual), para que uma execução a
# partir de uma cópia de pasta não leia a base errada.

raiz_projeto <- function() {
  d <- normalizePath(getwd(), winslash = "/")
  repeat {
    if (file.exists(file.path(d, "Projeto III.Rproj"))) return(d)
    pai <- dirname(d)
    if (identical(pai, d)) {
      stop("Raiz do projeto não encontrada: esperava 'Projeto III.Rproj' em algum diretório acima.")
    }
    d <- pai
  }
}

raiz <- raiz_projeto()
dir_saidas   <- file.path(raiz, "03_Saídas", "02_Tabelas")
dir_site     <- file.path(raiz, "04_Página")
dir_dados    <- file.path(dir_site, "data")
dir_figuras  <- file.path(dir_site, "figuras")
dir_tabelas  <- file.path(dir_site, "tabelas")
for (d in c(dir_dados, dir_figuras, dir_tabelas)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ---------------------------------------------------------------------------
# Leitura
# ---------------------------------------------------------------------------
# As tabelas do relatório são de exibição: separador ';', decimal com vírgula e
# células que juntam n, estimativa e intervalo. Leio tudo como texto e converto
# campo a campo, para não deixar o readr adivinhar (3.085 viraria 3,085).

ler_tabela <- function(nome) {
  readr::read_csv2(
    file.path(dir_saidas, nome),
    col_types = readr::cols(.default = "c"),
    locale = readr::locale(encoding = "UTF-8"),
    progress = FALSE
  )
}

# "3.085" -> 3085 ; "29,5" -> 29.5 ; "—" e "<0,001" -> NA
num_br <- function(x) {
  x <- trimws(gsub("\u00A0", " ", x))
  x <- gsub("\u2013", "-", x, fixed = TRUE)
  x <- gsub(".", "", x, fixed = TRUE)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

# ---------------------------------------------------------------------------
# Células do tipo "n; estimativa (li-ls)"
# ---------------------------------------------------------------------------
# Formatos presentes nas saídas:
#   "3.085; 29,5 (27,7-31,4)"   n + estimativa + IC
#   "3.085; 29,5"               n + estimativa
#   "29,5 (27,7-31,4)"          estimativa + IC
#   "38,8 (—)"                  estimativa sem IC
#   "1033 [600-1900]"           mediana e quartis
#   "0 (0,0)"                   n + estimativa entre parênteses
#   "18,7"                      estimativa isolada

padroes_celula <- list(
  n_est_ic   = "^([0-9.]+); ([0-9,]+) \\(([0-9,]+)-([0-9,]+)\\)$",
  n_est_sem_ic = "^([0-9.]+); ([0-9,]+) \\(—\\)$",
  n_est      = "^([0-9.]+); ([0-9,]+)$",
  est_ic     = "^([0-9,]+) \\(([0-9,]+)-([0-9,]+)\\)$",
  est_sem_ic = "^([0-9,]+) \\(—\\)$",
  mediana_iq = "^([0-9,]+) \\[([0-9,]+)-([0-9,]+)\\]$",
  n_est_paren = "^([0-9.]+) \\(([0-9,]+)\\)$",
  est        = "^([0-9,]+)$"
)

uma_celula <- function(s) {
  s <- trimws(gsub("\u00A0", " ", s))
  s <- gsub("\u2013", "-", s, fixed = TRUE)
  for (tipo in names(padroes_celula)) {
    m <- stringr::str_match(s, padroes_celula[[tipo]])
    if (!is.na(m[1, 1])) {
      g <- function(i) if (ncol(m) >= i) num_br(m[1, i]) else NA_real_
      v <- switch(
        tipo,
        n_est_ic    = list(n = g(2), est = g(3), li = g(4), ls = g(5)),
        n_est_sem_ic = list(n = g(2), est = g(3), li = NA_real_, ls = NA_real_),
        n_est       = list(n = g(2), est = g(3), li = NA_real_, ls = NA_real_),
        est_ic      = list(n = NA_real_, est = g(2), li = g(3), ls = g(4)),
        est_sem_ic  = list(n = NA_real_, est = g(2), li = NA_real_, ls = NA_real_),
        mediana_iq  = list(n = NA_real_, est = g(2), li = g(3), ls = g(4)),
        n_est_paren = list(n = g(2), est = g(3), li = NA_real_, ls = NA_real_),
        est         = list(n = NA_real_, est = g(2), li = NA_real_, ls = NA_real_)
      )
      return(tibble::tibble(
        n = v$n, est = v$est, li = v$li, ls = v$ls, tipo = tipo
      ))
    }
  }
  stop(sprintf("Formato de célula não reconhecido: '%s'", s))
}

separa_celula <- function(x) {
  dplyr::bind_rows(lapply(x, uma_celula))
}

# ---------------------------------------------------------------------------
# Tabelas de três grupos (Total, Homens, Mulheres)
# ---------------------------------------------------------------------------
tidy_grupos <- function(df, unidade = "pct") {
  df |>
    tidyr::pivot_longer(
      cols = c("Total", "Homens", "Mulheres"),
      names_to = "grupo", values_to = "celula"
    ) |>
    dplyr::mutate(parsed = lapply(.data$celula, uma_celula)) |>
    tidyr::unnest(.data$parsed) |>
    dplyr::mutate(unidade = unidade) |>
    dplyr::select(-"celula")
}

# ---------------------------------------------------------------------------
# Formatação para o site (padrão brasileiro)
# ---------------------------------------------------------------------------
fmt_num <- function(x, digitos = 1) {
  ifelse(
    is.na(x), "—",
    formatC(x, format = "f", digits = digitos, decimal.mark = ",", big.mark = ".")
  )
}

fmt_pct <- function(x, digitos = 1) {
  paste0(fmt_num(x, digitos), "%")
}

fmt_ic <- function(est, li, ls, digitos = 1, sufixo = "%") {
  ifelse(
    is.na(li) | is.na(ls),
    paste0(fmt_num(est, digitos), sufixo),
    sprintf("%s%s (%s–%s)", fmt_num(est, digitos), sufixo,
            fmt_num(li, digitos), fmt_num(ls, digitos))
  )
}

# Markdown de tabela, com alinhamento opcional por coluna.
escreve_md <- function(df, arquivo, align = NULL) {
  txt <- knitr::kable(
    df, format = "pipe", align = align, row.names = FALSE,
    format.args = list(decimal.mark = ",", big.mark = ".")
  )
  writeLines(as.character(txt), file.path(dir_tabelas, arquivo), useBytes = TRUE)
  invisible(arquivo)
}

# ---------------------------------------------------------------------------
# Paleta e tema das figuras
# ---------------------------------------------------------------------------
pal <- c(
  ink    = "#23272B",
  teal   = "#1F6F68",
  terra  = "#A85A3F",
  muted  = "#5F6B70",
  rule   = "#E4E1DB",
  soft   = "#F8F6F2",
  sand   = "#C9A227"
)

tema_site <- function(base = 15) {
  ggplot2::theme_minimal(base_size = base, base_family = "Helvetica") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = pal[["rule"]], linewidth = 0.3),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = ggplot2::rel(0.85), colour = pal[["muted"]]),
      axis.text = ggplot2::element_text(size = ggplot2::rel(0.9), colour = pal[["ink"]]),
      plot.title = ggplot2::element_text(
        family = "Helvetica", face = "bold", size = ggplot2::rel(1.05),
        colour = pal[["ink"]], margin = ggplot2::margin(b = 8)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(0.9), colour = pal[["muted"]],
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.75), colour = pal[["muted"]], hjust = 0
      ),
      strip.text = ggplot2::element_text(
        family = "Helvetica", face = "bold", size = ggplot2::rel(0.9),
        colour = pal[["ink"]], hjust = 0
      ),
      legend.position = "none",
      plot.margin = ggplot2::margin(6, 10, 6, 6)
    )
}

salva_svg <- function(plot, nome, largura, altura) {
  # Neste ambiente o svglite está incompatível com a API gráfica do R e o
  # cairo não tem as bibliotecas do X11. O ragg gera PNG sem depender de
  # nenhum dos dois; a 260 dpi o arquivo fica nítido em telas retina.
  ggplot2::ggsave(
    filename = file.path(dir_figuras, paste0(nome, ".png")),
    plot = plot, width = largura, height = altura,
    dpi = 260, bg = "white", device = ragg::agg_png
  )
  invisible(nome)
}

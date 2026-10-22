# Saúde e alimentação no Rio de Janeiro — PNS 2019

Site estático em Quarto com resultados da Pesquisa Nacional de Saúde (PNS) 2019
para adultos de 18 a 59 anos no estado do Rio de Janeiro.

Publicado em: **https://grugg8.github.io/saude-e-alimentacao-rj/**

## Conteúdo

Dez páginas, 15 figuras e 12 tabelas sobre o perfil da população, o consumo
alimentar, as doenças crônicas, o uso de serviços de saúde, as diferenças entre
homens e mulheres, a percepção de saúde, a escolaridade e a análise ajustada.

As estimativas usam os pesos, estratos e unidades primárias de amostragem da PNS,
com intervalo de confiança de 95%.

## Estrutura

```text
.
├── _quarto.yml
├── index.qmd                    início e resposta curta
├── quem-foi-estudado.qmd        recorte, fluxo e perfil
├── alimentacao.qmd              exposições alimentares
├── saude.qmd                    doenças, percepção e uso de serviços
├── homens-mulheres.qmd          comparações por sexo
├── percepcao.qmd                percepção x diagnóstico de depressão
├── desigualdade.qmd             gradientes por idade e escolaridade
├── alimentacao-e-saude.qmd      testes, matriz e modelos
├── limites.qmd                  limites, fontes e citação
├── metodos.qmd                  desenho, estimação e glossário
├── styles.css
├── assets/tabelas.js            rolagem horizontal das tabelas no celular
├── R/                           preparação dos dados, tabelas e figuras
├── data/                        estimativas em formato tidy
├── tabelas/                     tabelas e cartões incluídos nas páginas
└── figuras/                     figuras do site
```

## Como renderizar

```sh
quarto render
quarto preview
```

O site é estático: nenhum modelo ou teste é executado no navegador. Os arquivos
em `data/`, `tabelas/` e `figuras/` já estão prontos, e a renderização apenas
monta o HTML.

Os scripts em `R/` documentam como os dados, as tabelas e as figuras foram
preparados. Eles leem as saídas do projeto de análise, que não fazem parte deste
repositório.

## Publicação

O repositório mantém os fontes na branch `main` e o site na `gh-pages`.

```sh
quarto publish gh-pages --no-prompt
```

Se quiser manter a branch publicada sem arquivos do sistema:

```sh
find . -name .DS_Store -delete
```

## Dados

Fonte: Pesquisa Nacional de Saúde 2019, Instituto Brasileiro de Geografia e
Estatística (IBGE). O repositório publica apenas estimativas agregadas e as
figuras do site. Os microdados não são redistribuídos e podem ser obtidos no
site do IBGE.

## Citação

A referência para citar o site está na página [limites](limites.qmd), junto com
as limitações da análise.

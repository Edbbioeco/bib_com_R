# Pacotes

``` r
library(RefManageR)

library(tidyverse)
```

# Dados

## Importando

``` r
bib <- RefManageR::ReadBib("library.bib", check = FALSE)
```

## Visualisando

``` r
bib_df <- bib |>
  tibble::as.tibble()

bib_df
```

    ## # A tibble: 726 × 23
    ##    bibtype   author issn  issue journal pages publisher title volume year  abstract
    ##    <chr>     <chr>  <chr> <chr> <chr>   <chr> <chr>     <chr> <chr>  <chr> <chr>   
    ##  1 Article   Mario… 1932… 3     PloS o… e015… Public L… Dise… 11     2016   <NA>   
    ##  2 TechRepo… Lee R… <NA>  3     <NA>    297-… <NA>      Meas… 26     1945   <NA>   
    ##  3 TechRepo… Devon… <NA>  1     J. Par… 216-… <NA>      Func… 93     2007  "Most e…
    ##  4 TechRepo… Benja… <NA>  1     Source… 70-82 <NA>      A Co… 76     1996   <NA>   
    ##  5 TechRepo… Habin… <NA>  3     Landsc… 155-… SPB Acad… A ne… 8      1993  "A cont…
    ##  6 TechRepo… G Eve… <NA>  <NA>  <NA>    1-12  <NA>      The … 105    1953   <NA>   
    ##  7 TechRepo… Rober… <NA>  <NA>  <NA>    <NA>  <NA>      ON T… <NA>   2024  "One ap…
    ##  8 Book      Penny… <NA>  <NA>  <NA>    81    LEARN (L… A pl… <NA>   2008  "Cover …
    ##  9 TechRepo… T M D… <NA>  2     Source… 222-… <NA>      A Co… 26     1975   <NA>   
    ## 10 TechRepo… R H W… <NA>  3     <NA>    279-… <NA>      Vege… 30     1960   <NA>   
    ## # ℹ 716 more rows
    ## # ℹ 12 more variables: isbn <chr>, url <chr>, keywords <chr>, doi <chr>,
    ## #   month <chr>, pmid <chr>, city <chr>, editor <chr>, booktitle <chr>,
    ## #   edition <chr>, note <chr>, school <chr>

``` r
bib_df |> dplyr::glimpse()
```

    ## Rows: 726
    ## Columns: 23
    ## $ bibtype   <chr> "Article", "TechReport", "TechReport", "TechReport", "TechRepor…
    ## $ author    <chr> "Mario R Moura and Fabricio Villalobos and Gabriel C Costa and …
    ## $ issn      <chr> "1932-6203", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "00301299"…
    ## $ issue     <chr> "3", "3", "1", "1", "3", NA, NA, NA, "2", "3", NA, "2", "3655",…
    ## $ journal   <chr> "PloS one", NA, "J. Parasitol", "Source: Oikos", "Landscape Eco…
    ## $ pages     <chr> "e0152468", "297-302", "216-219", "70-82", "155-162", "1-12", N…
    ## $ publisher <chr> "Public Library of Science San Francisco, CA USA", NA, NA, NA, …
    ## $ title     <chr> "Disentangling the role of climate, topography and vegetation i…
    ## $ volume    <chr> "11", "26", "93", "76", "8", "105", NA, NA, "26", "30", NA, "11…
    ## $ year      <chr> "2016", "1945", "2007", "1996", "1993", "1953", "2024", "2008",…
    ## $ abstract  <chr> NA, NA, "Most efforts aimed at elucidating the factors responsi…
    ## $ isbn      <chr> NA, NA, NA, "202402:17:35", NA, NA, NA, "0901637106", NA, NA, N…
    ## $ url       <chr> NA, NA, NA, "https://about.jstor.org/terms", NA, NA, "https://w…
    ## $ keywords  <chr> NA, NA, NA, NA, "Contagion index,information index,landscape ec…
    ## $ doi       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "10.1111/j.2006.003…
    ## $ month     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "5", NA, NA, NA, NA…
    ## $ pmid      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ city      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ editor    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ booktitle <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ edition   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ note      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ school    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…

# Setando temas

``` r
theme_set(theme_bw() +
            theme(axis.text = element_text(color = "black", size = 15),
                  axis.title = element_text(color = "black", size = 15),
                  legend.text = element_text(color = "black", size = 15),
                  legend.title = element_text(color = "black", size = 15),
                  panel.border = element_rect(color = "black", linewidth = 1)))
```

# Análises

## Tipo de referência por ano

``` r
bib_df |>
  dplyr::summarise(quantidade = dplyr::n(),
                   .by = c(bibtype, year)) |>
  dplyr::mutate(year = year |>
                  as.numeric()) |>
  ggplot(aes(year, quantidade, color = bibtype)) +
  geom_line(linewidth = 1) +
  labs(x = "Ano",
       y = "Quantidade de trabalhos",
       color = "Tipo de bibliografia")
```

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

## Tipo de jornal por ano

``` r
bib_df |>
  dplyr::summarise(quantidade = dplyr::n(),
                   .by = c(journal)) |>
  dplyr::arrange(quantidade|> dplyr::desc()) |>
  tidyr::drop_na()
```

    ## # A tibble: 211 × 2
    ##    journal                       quantidade
    ##    <chr>                              <int>
    ##  1 Biological Conservation               20
    ##  2 Biotropica                            19
    ##  3 Biodiversity and Conservation         15
    ##  4 Source: Oikos                         11
    ##  5 PLoS ONE                              11
    ##  6 Ecology                               11
    ##  7 Journal of Biogeography               11
    ##  8 Landscape Ecology                     10
    ##  9 Journal of Herpetology                10
    ## 10 Ecography                              9
    ## # ℹ 201 more rows

``` r
bib_df |>
  dplyr::summarise(quantidade = dplyr::n(),
                   .by = c(journal, year)) |>
  dplyr::mutate(year = year |> as.numeric()) |>
  tidyr::drop_na() |>
  dplyr::filter(journal %in% c("Biotropica",
                               "Biodiversity and Conservation",
                               "Biological Conservation",
                               "Ecology",
                               "PLoS ONE"))  |>
  ggplot(aes(year, quantidade, color = journal)) +
  geom_line(linewidth = 1) +
  labs(x = "Ano",
       y = "Quantidade de trabalhos",
       color = "Revista científica")
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## Tamanho do título por ano

``` r
bib_df |>
  dplyr::summarise(quantidade = title |>
                     stringr::str_count(stringr::boundary("word")),
                   .by = year) |>
  dplyr::slice_max(quantidade,
                   by = year,
                   n = 1) |>
  dplyr::mutate(year = year |> as.numeric()) |>
  ggplot(aes(year, quantidade)) +
  geom_line(linewidth = 1) +
  labs(x = "Ano",
       y = "Quantidade de palavras") +
  scale_x_continuous(breaks = seq(1910, 2025, 15))
```

    ## Warning: Returning more (or less) than 1 row per `summarise()` group was deprecated in
    ## dplyr 1.1.0.
    ## ℹ Please use `reframe()` instead.
    ## ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()`
    ##   always returns an ungrouped data frame and adjust accordingly.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## Histogrma da quantidade de palavras

``` r
bib_df |>
  dplyr::mutate(observado = title |>
                  stringr::str_count(stringr::boundary("word"))) |>
  ggplot(aes(observado)) +
  geom_histogram(color = "black", binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 30, 2)) +
  labs(x = "Quantidade de palavras",
       y = "Contagem")
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## Tamanho do resumo por ano

``` r
bib_df |>
  dplyr::summarise(quantidade = abstract |>
                     stringr::str_count(stringr::boundary("word")),
                   .by = year) |>
  dplyr::slice_max(quantidade,
                   by = year,
                   n = 1) |>
  dplyr::mutate(year = year |> as.numeric()) |>
  ggplot(aes(year, quantidade)) +
  geom_line(linewidth = 1) +
  labs(x = "Ano",
       y = "Quantidade de palavras") +
  scale_x_continuous(breaks = seq(1910, 2025, 15))
```

    ## Warning: Returning more (or less) than 1 row per `summarise()` group was deprecated in
    ## dplyr 1.1.0.
    ## ℹ Please use `reframe()` instead.
    ## ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()`
    ##   always returns an ungrouped data frame and adjust accordingly.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: Removed 18 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

## Histogrma da quantidade de palavras nos abstracts

``` r
bib_df |>
  dplyr::mutate(observado = abstract |>
                  stringr::str_count(stringr::boundary("word"))) |>
  ggplot(aes(observado)) +
  geom_histogram(color = "black", binwidth = 10) +
  labs(x = "Quantidade de palavras",
       y = "Contagem")
```

    ## Warning: Removed 201 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

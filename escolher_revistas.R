# Pacotes ----

library(RefManageR)

library(tidyverse)

# Dados ----

## Importando ----

bib <- RefManageR::ReadBib("library.bib", check = FALSE) |>
  tibble::as_tibble()

## Visualizando ----

bib

# Selecionar revistas para o capítulo 1 da dissertação ----

## Total ----

bib |>
  dplyr::filter(title |> stringr::str_detect("abundance|Abundance") &
                  title |> stringr::str_detect("anura|Anura") |
                  title |> stringr::str_detect("herp|Herp")) |>
  dplyr::select(journal, title, year) |>
  dplyr::filter(year >= 2010) |>
  dplyr::summarise(quantidade = dplyr::n(),
                   .by = journal) |>
  dplyr::arrange(quantidade |> dplyr::desc()) |>
  as.data.frame()

## Nomes dos artigos ----

bib |>
  dplyr::filter(title |> stringr::str_detect("abundance|Abundance") &
                  title |> stringr::str_detect("anura|Anura") |
                  title |> stringr::str_detect("herp|Herp")) |>
  dplyr::select(journal, title, year) |>
  dplyr::filter(year >= 2010) |>
  dplyr::select(journal, title) |>
  as.data.frame()

# Selecionar revistas para o capítulo 2 da dissertação ----

## Total ----

bib |>
  dplyr::filter(title |> stringr::str_detect("diversity|Diversity") &
                  title |> stringr::str_detect("anura|Anura") |
                  title |> stringr::str_detect("herp|Herp")) |>
  dplyr::select(journal, title, year) |>
  dplyr::filter(year >= 2010) |>
  dplyr::summarise(quantidade = dplyr::n(),
                   .by = journal) |>
  dplyr::arrange(quantidade |> dplyr::desc()) |>
  as.data.frame()

## Nomes dos artigos ----

bib |>
  dplyr::filter(title |> stringr::str_detect("diversity|Diversity") &
                  title |> stringr::str_detect("anura|Anura") |
                  title |> stringr::str_detect("herp|Herp")) |>
  dplyr::select(journal, title, year) |>
  dplyr::filter(year >= 2010) |>
  dplyr::select(journal, title) |>
  as.data.frame()

(cos(-8.7275 * pi/180) * 2 * pi * 6378137) / (256 * 2^15)

## code to prepare `nea_cod_2025` dataset goes here

usethis::use_data(nea_cod_2025, overwrite = TRUE)

nea_cod_2025 <- rvest::read_html(x = "nea_cod_2025.html") |>
  rvest::html_table(header = TRUE)# |>
  rvest::html_text()

## code to prepare `nea_cod_2025` dataset goes here

usethis::use_data(nea_cod_2025, overwrite = TRUE)


import_species_data <- function(file_path, species_name) {
  df_list <- read_html(file_path) |>
    html_table()

  df <- df_list[[1]]


  colnames(df) <- df[3,]
  colnames(df)[2] <- "Recruitment"
  colnames(df)[3] <- "Low_R"
  colnames(df)[4] <- "High_R"
  colnames(df)[6] <- "Low_SSB"
  colnames(df)[7] <- "High_SSB"


  df <- df[-c(1:4),] |>
    select(1:7) |>
    filter(Year != "")


  df$species <- species_name

  return(df)
}

all_files <- list(
  "data/nea_cod_2025.html",
  "data/nea_haddock_2025.html"
)

all_species <- c("nea_cod","nea_haddock")



merge_species_df <- function(file_path_list, species_names_list) {

  all_data <- map2(all_files, all_species, ~ import_species_data(.x, .y))

  combined_df <- bind_rows(all_data)

}

combined_dataset <- merge_species_df(all_files, all_species)


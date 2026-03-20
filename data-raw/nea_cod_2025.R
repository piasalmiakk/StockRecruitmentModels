## code to prepare `nea_cod_2025` dataset goes here

usethis::use_data(nea_cod_2025, overwrite = TRUE)

## FUNCTION ##
# imports individual stock assessment html files and cleans them

import_species_data <- function(file_path, species_name) {
  df_list <- read_html(file_path) |>
    html_table() # reads the html file and turns it into a list of dataset tables

  df <- df_list[[1]] # chooses the dataset table number 1


  colnames(df) <- df[3,] # sets the column row as row 3

  # changing column names, some were duplicates so using indices
  colnames(df)[2] <- "Recruitment"
  colnames(df)[3] <- "Low_R"
  colnames(df)[4] <- "High_R"
  colnames(df)[6] <- "Low_SSB"
  colnames(df)[7] <- "High_SSB"


  df <- df[-c(1:4),] |> # removing the unnecessary top rows
    select(1:7) # choosing only relevant columns

  recruitment_age <- 3  #FIX THIS #extracting recruitment age for the species


  df$species <- species_name # adding column for species name

  # lagging recruitment by 3 years, since the spawns from SSB year 1 will be recruits by year of recruitment
  df <- df |>
    mutate(Recruitment = lag(Recruitment, recruitment_age),
           Low_R = lag(Low_R, recruitment_age),
           High_R = lag(High_R, recruitment_age),
           across(c(Recruitment, Low_R, High_R, SSB, Low_SSB, High_SSB)
                  ,as.numeric))  |> # making all cols numeric
    na.exclude() # removing NAs

  return(df)
}

# list of all the datafiles
all_files <- list(
  "data/nea_cod_2025.html",
  "data/nea_haddock_2025.html"
)

# list of all the species
all_species <- c("nea_cod","nea_haddock")

# test
test1 <- import_species_data("data/nea_cod_2025.html","cod_nea")

## FUNCTION ##
# merges the datasets imported from import_species_data function
merge_species_df <- function(file_path_list, species_names_list) {

  all_data <- map2(all_files, all_species, ~ import_species_data(.x, .y))

  combined_df <- bind_rows(all_data)

}

combined_dataset <- merge_species_df(all_files, all_species)


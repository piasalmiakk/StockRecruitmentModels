#' @importFrom dplyr select mutate bind_rows across
#' @import rvest
#' @importFrom purrr map2
#' @importFrom utils globalVariables

## import_species_data ##
### FUNCTION ###

#' @title Import stock assessment html file
#' @description Imports and cleans a stock assessment html file
#' @param file_path Takes in file path of an html file
#' @param species_name Takes a species name chosen by user (string)
#' @return returns a cleaned html file
#' @export


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
    mutate(Recruitment = dplyr::lag(.data$Recruitment, recruitment_age),
           Low_R = dplyr::lag(.data$Low_R, recruitment_age),
           High_R = dplyr::lag(.data$High_R, recruitment_age),
           across(c(.data$Recruitment, .data$Low_R, .data$High_R,
                    .data$SSB, .data$Low_SSB, .data$High_SSB)
                  ,as.numeric))  |> # making all cols numeric
    na.exclude() # removing NAs

  return(df)
}

utils::globalVariables(c("all_files", "all_species"))

## merge_species_df ##
### FUNCTION ###

#' Import and merge stock assessment datasets
#' @description Takes files and runs them through import_species_data and then merges them
#' @param file_path_list Takes in a list of file paths
#' @param species_names_list Takes in a list of species names in order of file list
#' @return returns a merged dataset
#' @export
merge_species_df <- function(file_path_list, species_names_list) {

  # using map to iterate all the files and species in the lists into the function
  all_data <- map2(all_files, all_species, ~ import_species_data(.x, .y))

  # binding together all to make a complete dataframe
  combined_df <- bind_rows(all_data)

}


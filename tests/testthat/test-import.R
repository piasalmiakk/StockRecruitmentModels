test_that("import works", {

  file_path <- system.file(
    "extdata",
    "nea_cod_2025.html",
    package = "StockRecruitmentModels"
  )

  imported_data <- import_species_data(file_path,"nea_cod")

  expect_true(!is.na(imported_data)) #testing if removal of NAs worked
  expect_true("Year" %in% imported_data[1,]) #testing if header is the correct row
  expect_true("Low_SSB" %in% imported_data[1,]) #testing if renaming of cols worked
})

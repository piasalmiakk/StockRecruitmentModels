test_that("import works", {
  imported_data <- import_species_data()

  expect_true(!is.na(imported_data)) #testing if removal of NAs worked
  expect_true("Year" %in% imported_data[1,]) #testing if header is the correct row
  expect_true("Low_SSB" %in% imported_data[1,]) #testing if renaming of cols worked
})

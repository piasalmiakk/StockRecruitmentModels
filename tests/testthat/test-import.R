test_that("import works", {
  imported_data <- import_species_data()

  expect_true(!is.na(imported_data))
  expect_true("Year" %in% imported_data[1,])
  expect_true("Low_SSB" %in% imported_data[1,])
  expect_true() #length of header
})

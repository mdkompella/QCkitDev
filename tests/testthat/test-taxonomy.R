df <- tibble::tibble(scientificName=c("Lynx canadensis", "Mimulus guttatus", "guttata", "Mimulus", "Erythranthe", "Phrymaceae"))

test_that("get_taxon_rank adds column called taxonRank", {
  df_new <- get_taxon_rank(df, sciName_col = "scientificName")
  expect_equal(names(df_new), c("scientificName", "taxonRank"))
})

test_that("get_taxon_rank taxonRank column has correct contents", {
  df_new <- get_taxon_rank(df, sciName_col = "scientificName")
  expect_equal(df_new$taxonRank, c("species", "species", "genus",
                                   "genus", "genus", "family"))
})

test_that("te_check returns correct list of NPS units", {
  nps_unit_list <- "ROMO"
  x<-check_te(df, species_col = "scientificName",
              park_code = nps_unit_list,
              expansion = FALSE)
  expect_equal(x$Park_code[-1], nps_unit_list)
})

test_that("te_check returns no hits for common taxa", {
  df2 <- df[-1,] #remove threatened (in ROMO) species
  x <- check_te(df2, "scientificName", "ROMO", expansion = FALSE)
  expect_equal(nrow(x)*ncol(x), 0)
})

test_that("te_check expansion = TRUE returns correct columns", {
  x <- check_te(df, "scientificName", "ROMO", expansion = TRUE)
  expect_equal(names(x), c("Park_code", "Species", "In_data", "status_code", "status_explanation"))
})

##### Tests for deprecated function:

test_that("te_check is deprecated", {
  rlang::local_options(lifecycle_verbosity = "error")
  expect_error(te_check(), class = "defunctError")
})

test_that("te_check returns correct list of NPS units", {
  nps_unit_list <- "ROMO"
  x <- suppressWarnings(te_check(df,
                              species_col = "scientificName",
                              park_code = nps_unit_list,
                              expansion = FALSE))
  expect_equal(x$Park_code[-1], nps_unit_list)
})

test_that("te_check returns no hits for common taxa", {
  df2 <- df[-1,] #remove threatened (in ROMO) species
  x <- suppressWarnings(te_check(df2,
                                 "scientificName",
                                 "ROMO",
                                 expansion = FALSE))
  expect_equal(nrow(x)*ncol(x), 0)
})

test_that("te_check expansion = TRUE returns correct columns", {
  x <- suppressWarnings(te_check(df,
                                 "scientificName",
                                 "ROMO",
                                 expansion = TRUE))
  expect_equal(names(x), c("Park_code",
                           "Species",
                           "In_data",
                           "status_code",
                           "status_explanation"))
})

test_that("check_te_species exits with error for non-dataframe input", {
  expect_error(suppressMessages(
    check_te_species(x = c("Lynx canadensis", "guttata"))),
    regexp = "Input must be a data frame.")
})

test_that("check_te_species exits with error for bad column input", {
  expect_error(suppressMessages(
    check_te_species(x = df,
                     sciname_col = "sciName")),
    regexp = "Scientific name column specified must exist in input data frame.")
})

test_that("check_te_species exits with error for listing_status input", {
  expect_error(suppressMessages(
    check_te_species(x = df,
                     sciname_col = "scientificName",
                     listing_status = "de-listed")),
    regexp = "Listing status must be one of: 'all', 'listed', and 'listed or proposed'.")
})

test_that("check_te_species exits with error for listing_status returns a data frame", {
  x <- suppressMessages(check_te_species(x = df,
                                         sciname_col = "scientificName"))
  expect_s3_class(x, "data.frame")
})

test_that("check_te_species with good inputs prints messages", {
  expect_message(check_te_species(x = df,
                                  sciname_col = "scientificName"))
})

test_that("check_te_species on test df catches Lynx canadensis", {
  x <- suppressMessages(check_te_species(x = df,
                                         sciname_col = "scientificName"))
  expect_match(x$INPUT_scientificName, "Lynx canadensis")
})

test_that("check_te_species returns correct columns", {
  x <- suppressMessages(check_te_species(x = df,
                                         sciname_col = "scientificName"))
  expect_equal(names(x), c("INPUT_scientificName",
                           "INPUT_currentName",
                           "ECOS_scientificName",
                           "ECOS_commonName",
                           "ECOS_ID",
                           "ECOS_taxonomicGroup",
                           "ECOS_listingStatus",
                           "ECOS_statusCategory",
                           "ECOS_whereListed",
                           "currentTaxonID",
                           "taxonSource"))
})

test_that("check_te_species(resolve_input_taxonomy = FALSE) returns correct columns", {
  x <- suppressMessages(check_te_species(x = df,
                                         sciname_col = "scientificName",
                                         resolve_input_taxonomy = FALSE))
  expect_equal(names(x), c("INPUT_scientificName",
                           "ECOS_scientificName",
                           "ECOS_commonName",
                           "ECOS_ID",
                           "ECOS_taxonomicGroup",
                           "ECOS_listingStatus",
                           "ECOS_statusCategory",
                           "ECOS_whereListed",
                           "currentName",
                           "currentTaxonID",
                           "taxonSource"))
})

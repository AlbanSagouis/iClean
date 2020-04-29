context("Species functions")
source('data/species_names_examples.r', encoding = 'UTF-8')

test_that("Result characteristics are correct", {
   expect_is(species_names_correction(species_vector, grouping_vector), "character")
   expect_is(species_names_correction(species_vector, grouping_vector), "character")
   expect_equal(length(species_names_correction(species_vector)), length(species_vector))
   expect_error(species_names_correction(species_vector, genus_name_extension = TRUE))
})

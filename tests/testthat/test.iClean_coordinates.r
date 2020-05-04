context("Species functions")
source('C:/Users/as80fywe/idiv/my r packages/iClean/data/coordinates_examples.r', encoding = 'UTF-8')

test_that("Result characteristics are correct", {
   expect_is(coordinate_cleaning(coords, id), "data.frame")
   expect_equal(nrow(coordinate_cleaning(coords, id)), length(coords))
   expect_error(coordinate_cleaning(coords=rep(NA, length(coords)), id))
})

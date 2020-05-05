context("Species functions")
source('C:/Users/as80fywe/idiv/my r packages/iClean/data/coordinates_examples.r', encoding = 'UTF-8')

test_that("Result characteristics are correct", {
   expect_is(coordinate_cleaning(coords, id), "data.frame")
   expect_equal(nrow(coordinate_cleaning(coords, id)), length(coords))
   expect_error(coordinate_cleaning(coords=rep(NA, length(coords)), id))
})

# test_that("corrections work", {
#    expect_identical(coordinate_cleaning(coords, id, result_format = 'simple', assume_good_order = TRUE))
# })

tst <- coordinate_cleaning(dat$coord, dat$study_ID, assume_good_order = F)

#
# tdat <- rbind(dat,dat,dat,dat,dat,dat,dat,dat)
# profvis::profvis({
#    coordinate_cleaning(tdat$coord, tdat$study_ID, assume_good_order = T)
# })


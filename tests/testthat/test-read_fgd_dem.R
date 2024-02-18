test_that("FGD DEM", {
  dem_644320 <- system.file("extdata", "FG-GML-6443-20-DEM5A.zip", package = "fgd2r")
  trr_test <- read_fgd_dem(dem_644320)
  expect_s4_class(trr_test, "SpatRaster")
})

library(testthat)
library(PhotoMapper)

context("integration")
#' Integration tests, meaning that it is only ensured that changes leading to bugs will be detected
print(getwd())
imagePath <- normalizePath("../images/")

test_that("valid_exif_no_error", {
  photos <- paste0(imagePath, "valid.jpg")
  comparison <- readRDS("valid_f_f_30_300.RDS")
  #rds file was created with exact same parameters
  expect_equal(
    mapPhotos(
      photos,
      imputeExif = F,
      ignoreMissingTimestamp = F,
      imageQuality = 30,
      imageWidth = 300,
      showProgress = F
    ),
    object = comparison
  )
  #imputation and missing timestamp switches should not change result
  expect_equal(
    mapPhotos(
      photos,
      imputeExif = T,
      ignoreMissingTimestamp = T,
      imageQuality = 30,
      imageWidth = 300,
      showProgress = F
    ),
    object = readRDS("valid_f_f_30_300.RDS")
  )
})


test_that("invalid_exif_error", {
  photos <- paste0(imagePath, "invalid.jpg")
  #invalid image should lead to a message pointing to an exif error
  expect_error(
    mapPhotos(
      photos,
      imputeExif = F,
      ignoreMissingTimestamp = F,
      imageQuality = 30,
      imageWidth = 300,
      showProgress = F
    ),
    regexp = ".* EXIF .*"
  )
  #imputation and missing timestamp switches should not change result
  expect_error(
    mapPhotos(
      photos,
      imputeExif = T,
      ignoreMissingTimestamp = T,
      imageQuality = 30,
      imageWidth = 300,
      showProgress = F
    ),
    regexp = ".* EXIF .*"
  )
})


test_that("parameter_variation", {
  photos <- paste0(imagePath, "valid.jpg")
  #rds file was created with different parameters
  expect_false(identical(
    mapPhotos(
      photos,
      imputeExif = F,
      ignoreMissingTimestamp = F,
      imageQuality = 30,
      imageWidth = 30,
      showProgress = F
    ),
    readRDS("valid_f_f_30_300.RDS")
  ))
  #imageQuality variation should not result in different object
  expect_equal(
    mapPhotos(
      photos,
      imputeExif = T,
      ignoreMissingTimestamp = T,
      imageQuality = 10,
      imageWidth = 300,
      showProgress = F
    ),
    object = readRDS("valid_f_f_30_300.RDS")
  )
})


test_that("valid_invalid_exif_parameter_test", {
  photos <- paste0(imagePath, c("valid.jpg", "invalid.jpg"))
  #invalid image should be ignored and lead to same result as with valid only
  expect_equal(
    mapPhotos(
      photos,
      imputeExif = F,
      ignoreMissingTimestamp = F,
      imageQuality = 30,
      imageWidth = 300,
      showProgress = F
    ),
    object = readRDS("valid_f_f_30_300.RDS")
  )
  #imputation should lead to invalid image being included
  expect_equal(
    mapPhotos(
      photos,
      imputeExif = T,
      ignoreMissingTimestamp = F,
      imageQuality = 30,
      imageWidth = 300,
      showProgress = F
    ),
    object = readRDS("valid_invalid_t_f_30_300.RDS")
  )
})

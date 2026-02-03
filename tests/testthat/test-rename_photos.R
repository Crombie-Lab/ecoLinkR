# tests/testthat/test-rename_photos.R

test_that("rename_photos() renames files via two-step rename", {
  td <- withr::local_tempdir()
  folder <- file.path(td, "collection")
  dir.create(folder, recursive = TRUE)

  # create dummy source files
  sources <- c("IMG_7436.heic", "IMG_7437.heic", "IMG_7438.heic")
  file.create(file.path(folder, sources))

  mapping <- c(
    "IMG_7436.heic" = "C-0011.heic",
    "IMG_7437.heic" = "C-0020.heic",
    "IMG_7438.heic" = "C-0019.heic"
  )

  res <- rename_photos(folder_path = folder, mapping = mapping, verbose = FALSE)

  # returned structure
  expect_s3_class(res, "data.frame")
  expect_true(all(c("old_path", "new_path", "status") %in% names(res)))
  expect_true(all(res$status == "renamed"))

  # files moved correctly
  expect_false(any(file.exists(file.path(folder, names(mapping)))))
  expect_true(all(file.exists(file.path(folder, unname(mapping)))))

  # no tmp files left behind
  expect_false(any(grepl("^\\.tmp_", list.files(folder))))
})

test_that("rename_photos() dry_run does not rename anything", {
  td <- withr::local_tempdir()
  folder <- file.path(td, "collection")
  dir.create(folder, recursive = TRUE)

  file.create(file.path(folder, "IMG_1.heic"))

  mapping <- c("IMG_1.heic" = "C-0001.heic")

  res <- rename_photos(folder, mapping, dry_run = TRUE, verbose = FALSE)

  expect_true(all(res$status == "dry_run"))
  expect_true(file.exists(file.path(folder, "IMG_1.heic")))
  expect_false(file.exists(file.path(folder, "C-0001.heic")))
})

test_that("rename_photos() errors when a source file is missing", {
  td <- withr::local_tempdir()
  folder <- file.path(td, "collection")
  dir.create(folder, recursive = TRUE)

  # only one exists
  file.create(file.path(folder, "IMG_1.heic"))

  mapping <- c(
    "IMG_1.heic" = "C-0001.heic",
    "IMG_2.heic" = "C-0002.heic"  # missing
  )

  expect_error(
    rename_photos(folder, mapping, verbose = FALSE),
    "source files are missing|missing",
    ignore.case = TRUE
  )
})

test_that("rename_photos() errors if target exists and overwrite = FALSE", {
  td <- withr::local_tempdir()
  folder <- file.path(td, "collection")
  dir.create(folder, recursive = TRUE)

  file.create(file.path(folder, "IMG_1.heic"))
  file.create(file.path(folder, "C-0001.heic")) # pre-existing target

  mapping <- c("IMG_1.heic" = "C-0001.heic")

  expect_error(
    rename_photos(folder, mapping, overwrite = FALSE, verbose = FALSE),
    "already exist|overwrit",
    ignore.case = TRUE
  )
})

test_that("rename_photos() overwrites when overwrite = TRUE", {
  td <- withr::local_tempdir()
  folder <- file.path(td, "collection")
  dir.create(folder, recursive = TRUE)

  # create source and an existing target
  file.create(file.path(folder, "IMG_1.heic"))
  writeLines("old_target", file.path(folder, "C-0001.heic"))

  mapping <- c("IMG_1.heic" = "C-0001.heic")

  res <- rename_photos(folder, mapping, overwrite = TRUE, verbose = FALSE)

  expect_true(all(res$status == "renamed"))
  expect_false(file.exists(file.path(folder, "IMG_1.heic")))
  expect_true(file.exists(file.path(folder, "C-0001.heic")))
})

test_that("rename_photos() errors when target filenames are not unique", {
  td <- withr::local_tempdir()
  folder <- file.path(td, "collection")
  dir.create(folder, recursive = TRUE)

  file.create(file.path(folder, c("IMG_1.heic", "IMG_2.heic")))

  mapping <- c(
    "IMG_1.heic" = "C-0001.heic",
    "IMG_2.heic" = "C-0001.heic" # duplicate target
  )

  expect_error(
    rename_photos(folder, mapping, verbose = FALSE),
    "unique|duplicate",
    ignore.case = TRUE
  )
})


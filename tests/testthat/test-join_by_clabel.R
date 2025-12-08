# tests/testthat/test-join_by_clabel.R

test_that("join_by_clabel stacks rows and keeps all c_labels", {
  df1 <- data.frame(
    c_label = c("C-0001", "C-0002"),
    a = 1:2,
    stringsAsFactors = FALSE
  )

  df2 <- data.frame(
    c_label = c("C-0003", "C-0004"),
    b = 3:4,
    stringsAsFactors = FALSE
  )

  res <- join_by_clabel(df1, df2)

  expect_equal(res$c_label, c("C-0001", "C-0002", "C-0003", "C-0004"))
  expect_equal(nrow(res), 4L)
})

test_that("join_by_clabel coerces c_label to character and accepts list input", {
  df1 <- data.frame(
    c_label = 1:2,
    a = 1:2,
    stringsAsFactors = FALSE
  )

  df2 <- data.frame(
    c_label = c("3", "4"),
    b = 3:4,
    stringsAsFactors = FALSE
  )

  res <- join_by_clabel(list(df1, df2))

  expect_type(res$c_label, "character")
  expect_equal(res$c_label, c("1", "2", "3", "4"))
})

test_that("join_by_clabel errors nicely on bad input", {
  expect_error(join_by_clabel(), "No input data frames supplied", fixed = TRUE)

  not_df <- 1:3
  df1 <- data.frame(c_label = "C-0001", x = 1)

  expect_error(
    join_by_clabel(df1, not_df),
    "is not a data frame",
    fixed = TRUE
  )

  df_no_clabel <- data.frame(other = 1)
  expect_error(
    join_by_clabel(df1, df_no_clabel),
    "does not contain a `c_label` column",
    fixed = TRUE
  )
})

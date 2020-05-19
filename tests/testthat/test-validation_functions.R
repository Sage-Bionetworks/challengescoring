test_that("validate_submission", {
  val_df1 <- tibble::tibble(
    prediction_name = c("p1", "p2"),
    validation = c(1, 0)
  )
  pred_df1 <- tibble::tibble(
    prediction_name = c("p1", "p2"),
    prediction = c(1, 0)
  )
  pred_df2 <- tibble::tibble(
    prediction_name = c("p1", "p1"),
    prediction = c(1, 0)
  )
  pred_df3 <- tibble::tibble(
    prediction_name = c("p1", "p2"),
    prediction = c(1, NA)
  )
  pred_df4 <- tibble::tibble(
    prediction = c(1, NA)
  )
  expect_equal(
    validate_submission(val_df1, pred_df1),
    combine_validation_prediction_dfs(val_df1, pred_df1)
  )
  expect_equal(
    validate_submission(val_df1, pred_df2),
    paste0(
      "Prediction file has duplicate predictions for: [p1], ",
      "Prediction file is missing predictions: [p2]."
    )
  )
  expect_equal(
    validate_submission(val_df1, pred_df3),
    "Prediction file is missing predictions: [p2]."
  )
  expect_equal(
    validate_submission(val_df1, pred_df4),
    "Prediction file is missing columns: [prediction_name]."
  )
})

test_that("validate_required_columns", {
    name_columns <- c("dataset_name", "sample_id")
    df1 <- tibble::tribble(~dataset_name, ~sample_id, ~prediction)
    df2 <- tibble::tribble(~sample_id, ~dataset_name, ~prediction)
    df3 <- tibble::tribble(~dataset_name, ~prediction)
    df4 <- tibble::tribble(~dataset.name, ~sample.id, ~prediction)
    df5 <- tibble::tribble(~dataset_name, ~sample_id, ~sample_id, ~prediction)
    df6 <- tibble::tribble(~dataset.name, ~sample_id, ~sample_id, ~prediction)
    df7 <- tibble::tribble(~dataset_name, ~sample_id, ~pred)

    expect_null(validate_required_columns(df1, name_columns))
    expect_null(validate_required_columns(df2, name_columns))
    expect_equal(
        validate_required_columns(df3, name_columns),
        "Prediction file is missing columns: [sample_id]."
    )
    expect_equal(
        validate_required_columns(df4, name_columns),
        "Prediction file is missing columns: [dataset_name, sample_id].",
    )
    expect_equal(
      validate_required_columns(df5, name_columns),
      "Prediction file has duplicate columns: [sample_id].",
    )
    expect_equal(
      validate_required_columns(df6, name_columns),
      stringr::str_c(
        "Prediction file has duplicate columns: [sample_id], ",
        "Prediction file is missing columns: [dataset_name]."
      )
    )
    expect_equal(
      validate_required_columns(df7, name_columns),
      stringr::str_c(
        "Prediction file is missing columns: [prediction]."
      )
    )
    expect_null(
      validate_required_columns(df7, name_columns, "pred"),
    )
})

test_that("combine_validation_prediction_dfs", {
  val_df1  <- tibble::tribble(~prediction_name, ~validation)
  pred_df1 <- tibble::tribble(~prediction_name, ~prediction)
  val_df2  <- tibble::tribble(~name_col1, ~name_col2, ~name_col3, ~validation)
  pred_df2 <- tibble::tribble(~name_col1, ~name_col2, ~name_col4, ~prediction)
  expect_equal(
    combine_validation_prediction_dfs(val_df1, pred_df1),
    tibble::tribble(~prediction_name, ~validation, ~prediction)
  )
  expect_equal(
    combine_validation_prediction_dfs(
      val_df2, pred_df2, name_columns = c("name_col1", "name_col2")
    ),
    tibble::tribble(~name_col1, ~name_col2, ~validation, ~prediction)
  )
})

test_that("validate_combined_df", {
  df1 <- tibble::tibble(
    prediction_name = c("p1", "p2"),
    validation = c(1, 0),
    prediction = c(1, 0)
  )
  df2 <- tibble::tibble(
    prediction_name = c("p1", "p1"),
    validation = c(1, 0),
    prediction = c(1, 0)
  )
  df3 <- tibble::tibble(
    prediction_name = c("p1", "p2"),
    validation = c(1, 0),
    prediction = c(1, NA)
  )
  expect_equal(
    validate_combined_df(df1),
    NULL
  )
  expect_equal(
    validate_combined_df(df2),
    "Prediction file has duplicate predictions for: [p1]."
  )
  expect_equal(
    validate_combined_df(df3),
    "Prediction file is missing predictions: [p2]."
  )
})

test_that("combine_error_messages", {
  expect_equal(combine_error_messages("error1"), "error1.")
  expect_equal(combine_error_messages(c("error1", "error2")), "error1, error2.")
})

test_that("get_duplicate_rows_by_name_columns", {
  df1 <- tibble::tibble(prediction_name = c("p1", "p2"))
  df2 <- tibble::tibble(prediction_name = c("p1", "p2", "p2"))

  expect_equal(get_duplicate_rows_by_name_columns(df1), character())
  expect_equal(get_duplicate_rows_by_name_columns(df2), "p2")
})

test_that("get_duplicate_rows_by_name_columns", {
  df1 <- tibble::tibble(prediction_name = c("p1", "p2"))
  df2 <- tibble::tibble(prediction_name = c("p1", "p2", "p2"))

  expect_equal(get_duplicate_rows_by_name_columns(df1), character())
  expect_equal(get_duplicate_rows_by_name_columns(df2), "p2")
})

test_that("get_missing_rows_by_name_columns", {
  df1 <- tibble::tibble(prediction_name = "p1", prediction = 1)
  df2 <- tibble::tibble(prediction_name = c("p1", "p2"), prediction = c(1, NA))

  expect_equal(get_missing_rows_by_name_columns(df1), character())
  expect_equal(get_missing_rows_by_name_columns(df2), "p2")
})

test_that("get_duplicate_column_names", {
  df1 <- tibble::tribble(~dataset_name)
  df2 <- tibble::tribble(~dataset_name, ~dataset_name)

  expect_equal(get_duplicate_column_names(df1), character())
  expect_equal(get_duplicate_column_names(df2), c("dataset_name"))
})

test_that("create_duplicate_column_names_message", {
  expect_equal(
    create_duplicate_column_names_message("col1"),
    stringr::str_c(
      "Prediction file has duplicate columns: [col1]"
    )
  )
})

test_that("create_missing_column_names_message", {
  expect_equal(
    create_missing_column_names_message("col1"),
    stringr::str_c(
      "Prediction file is missing columns: [col1]"
    )
  )
})

test_that("create_duplicate_rows_message", {
  expect_equal(
    create_duplicate_rows_message("prediction1"),
    stringr::str_c(
      "Prediction file has duplicate predictions for: [prediction1]"
    )
  )
})

test_that("create_missing_rows_message", {
  expect_equal(
    create_missing_rows_message("prediction1"),
    stringr::str_c(
      "Prediction file is missing predictions: [prediction1]"
    )
  )
})

test_that("values_to_list_string", {
  expect_equal(
    values_to_list_string(c("A;B;C", "X;Y;Z")),
    "[A;B;C, X;Y;Z]")
  expect_equal(
    values_to_list_string(list("A;B;C", "X;Y;Z")),
    "[A;B;C, X;Y;Z]")
  expect_equal(
    values_to_list_string(c(8, 9, 10)),
    "[8, 9, 10]")
})

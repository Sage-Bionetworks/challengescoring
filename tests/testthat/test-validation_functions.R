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
    correct_columns <- c("dataset_name", "sample_id")
    df1 <- tibble::tribble(~dataset_name, ~sample_id)
    df2 <- tibble::tribble(~sample_id, ~dataset_name)
    df3 <- tibble::tribble(~dataset_name)
    df4 <- tibble::tribble(~dataset.name, ~sample.id)
    df5 <- tibble::tribble(~dataset_name, ~sample_id, ~sample_id)
    df6 <- tibble::tribble(~dataset.name, ~sample_id, ~sample_id)

    expect_null(validate_required_columns(df1, correct_columns))
    expect_null(validate_required_columns(df2, correct_columns))
    expect_equal(
        validate_required_columns(df3, correct_columns),
        "Prediction file is missing columns: [sample_id]."
    )
    expect_equal(
        validate_required_columns(df4, correct_columns),
        "Prediction file is missing columns: [dataset_name, sample_id].",
    )
    expect_equal(
      validate_required_columns(df5, correct_columns),
      "Prediction file has duplicate columns: [sample_id].",
    )
    expect_equal(
      validate_required_columns(df6, correct_columns),
      stringr::str_c(
        "Prediction file has duplicate columns: [sample_id], ",
        "Prediction file is missing columns: [dataset_name]."
      )
    )
})

test_that("combine_validation_prediction_dfs", {
  val_df1  <- tibble::tibble(
    prediction_name = c("p1", "p2"),
    validation = c(1, 0)
  )
  pred_df1 <- tibble::tibble(
    prediction_name = c("p1", "p2"),
    prediction = c(0, 1)
  )
  val_df2  <- tibble::tibble(
    name_col1 = c("p1", "p2"),
    name_col2 = c("p3", "p4"),
    name_col3 = c("p5", "p6"),
    validation = c(2, 1)
  )
  pred_df2 <- tibble::tibble(
    name_col1 = c("p1", "p2"),
    name_col2 = c("p3", "p4"),
    name_col3 = c("p5", "p6"),
    prediction = c(3, 4)
  )
  expect_equal(
    combine_validation_prediction_dfs(val_df1, pred_df1),
    tibble::tibble(
      prediction_name = c("p1", "p2"),
      validation = c(1, 0),
      prediction = c(0, 1))
  )
  expect_equal(
    combine_validation_prediction_dfs(
      val_df2, pred_df2, name_columns = c("name_col1", "name_col2")
    ),
    tibble::tibble(
      name_col1 = c("p1", "p2"),
      name_col2 = c("p3", "p4"),
      validation = c(2, 1),
      prediction = c(3, 4)
    )
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

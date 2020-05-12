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
  val_df1  <- tibble::tribble(~prediction_name, ~validation)
  pred_df1 <- tibble::tribble(~prediction_name, ~prediction)
  val_df2  <- tibble::tribble(~name_col1, ~name_col2, ~name_col3, ~validation)
  pred_df2 <- tibble::tribble(~name_col1, ~name_col2, ~name_col4, ~prediction)
  expect_equal(
    combine_validation_prediction_dfs(val_df1, pred_df1),
    tibble::tribble(~prediction_name, ~validation, ~prediction)
  )
  expect_equal(
    combine_validation_prediction_dfs(val_df2, pred_df2, name_columns = c("name_col1", "name_col2")),
    tibble::tribble(~name_col1, ~name_col2, ~validation, ~prediction)
  )
  expect_equal(
    combine_validation_prediction_dfs(val_df2, pred_df2, name_columns = c("name_col1", "name_col3")),
    NULL
  )
  # df2 <- tibble::tribble(~sample_id, ~dataset_name)
  # df3 <- tibble::tribble(~dataset_name)
  # df4 <- tibble::tribble(~dataset.name, ~sample.id)
  # df5 <- tibble::tribble(~dataset_name, ~sample_id, ~sample_id)
  # df6 <- tibble::tribble(~dataset.name, ~sample_id, ~sample_id)
  #
  # expect_null(validate_required_columns(df1, correct_columns))
})


test_that("create_missing_column_names_message", {
  expect_equal(
    create_missing_column_names_message("col3"),
    stringr::str_c(
      "Prediction file is missing columns: [col3]"
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

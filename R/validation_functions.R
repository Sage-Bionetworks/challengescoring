#' Validate Submission
#'
#' @param prediction_df A df made from the prediction file
#' @param validation_df A df made from the validation file
#' @param name_columns A character vector of columns used to identify a
#' prediction
#' @param pred_column A string, the column in the prediction df that has the
#' prediction values
#' @param val_column A string, the column in the validation df that has the
#' validation values
#' @param prediction_validation_funs A list of functions to run on the
#' prediction df. These functions must take params prediction_df, name_columns,
#' and pred_column.
#' @param combined_validation_funs A list of functions to run on the
#' combined_df. These functions must take the param combined_df.
#' @export
validate_submission <- function(
  validation_df,
  prediction_df,
  name_columns  = "prediction_name",
  pred_column   = "prediction",
  val_column    = "validation",
  prediction_validation_funs = NULL,
  combined_validation_funs = NULL
){
  column_result <- validate_required_columns(
    prediction_df, c(name_columns, pred_column)
  )
  if (!is.null(column_result)) return(column_result)
  combined_df <- combine_validation_prediction_dfs(
    validation_df, prediction_df, name_columns, pred_column, val_column
  )
  combined_result <- validate_combined_df(combined_df)
  if (!is.null(combined_result)) return(combined_result)
  return(combined_df)
}

# Prediction df validation functions -------------------------------------------
# These functions must take the following paramters:
# prediction_df: A df made from the prediction file
# name_columns: A character vector of columns used to identify a prediction
# pred_column A string, the column in the prediction df that has the
# prediction values
# All other parameters must have defaults
# These functions must return either NULL, or a string that is returned to
# participants

#' Validate Required Columns
#'
#' @param df A df from a prediction file
#' @param required_columns A character of vector of columns the df should have
#' @export
validate_required_columns <- function(
  prediction_df,
  name_columns  = "prediction_name",
  pred_column   = "prediction"
){
  error_messages <- c()

  duplicated_columns <- get_duplicate_column_names(prediction_df)
  required_columns   <- c(name_columns, pred_column)
  missing_columns    <- setdiff(required_columns, colnames(prediction_df))

  if (length(duplicated_columns) != 0) {
    error_messages <- create_duplicate_column_names_message(duplicated_columns)
  }
  if (length(missing_columns) != 0) {
    error_messages <- c(
      error_messages, create_missing_column_names_message(missing_columns)
    )
  }
  if (length(error_messages) != 0) {
    return(combine_error_messages(error_messages))
  } else {
    return(NULL)
  }
}

# Prediction df validation functions -------------------------------------------
# These functions must take the following paramters:
# combined_df: A df made from the prediction file and validation file
# name_columns: A character vector of columns used to identify a prediction
# pred_column A string, the column in the prediction df that has the
# prediction values
# val_column A string, the column in the validation df that has the
# validation values
# All other parameters must have defaults
# These functions must return either NULL, or a string that is returned to
# participants


#' Validate Combined DF
#'
#' @param combined_df A combined prediction and validation df, typically a result of
#' combine_validation_prediction_dfs
#' @param name_columns A character vector of columns used to identify a
#' prediction
#' @param pred_column A string, the column in the prediction df that has the
#' prediction values
#' @param val_column A string, the column in the validation df that has the
#' validation values
#' @export
validate_combined_df <- function(
  combined_df,
  name_columns  = "prediction_name",
  pred_column   = "prediction",
  val_column    = "validation"
){
  error_messages <- c()

  duplicate_rows <- get_duplicate_rows_by_name_columns(combined_df, name_columns)
  missing_rows <- get_missing_rows_by_name_columns(combined_df, name_columns)

  if (length(duplicate_rows) != 0) {
    error_messages <- create_duplicate_rows_message(duplicate_rows)
  }
  if (length(missing_rows) != 0) {
    error_messages <- c(
      error_messages, create_missing_rows_message(missing_rows)
    )
  }
  if (length(error_messages) != 0) {
    return(combine_error_messages(error_messages))
  } else {
    return(NULL)
  }
}

#' Validate numeric prediction DF
#'
#' @param combined_df A combined prediction and validation df, typically a result of
#' combine_validation_prediction_dfs
#' @param name_columns A character vector of columns used to identify a
#' prediction
#' @param pred_column A string, the column in the prediction df that has the
#' prediction values
#' @param val_column A string, the column in the validation df that has the
#' validation values
#' @export
validate_numeric_prediction_df <- function(
  combined_df,
  name_columns  = "prediction_name",
  pred_column   = "prediction",
  val_column    = "validation"
){
  error_messages <- c()

  df <- combined_df %>%
    tidyr::unite("prediction_name", name_columns, sep = ";") %>%
    dplyr::select("prediction_name", "prediction" = pred_column) %>%
    dplyr::mutate(prediction = as.numeric(.data$prediction))

  non_numeric_rows <- df %>%
    dplyr::filter(is.na(.data$prediction)) %>%
    dplyr::pull("prediction_name")

  inf_rows <- df %>%
    dplyr::filter(is.infinite(.data$prediction)) %>%
    dplyr::pull("prediction_name")

  if (length(non_numeric_rows) != 0) {
    error_messages <- create_duplicate_rows_message(non_numeric_rows)
  }
  if (length(inf_rows) != 0) {
    error_messages <- c(
      error_messages, create_missing_rows_message(inf_rows)
    )
  }
  if (length(error_messages) != 0) {
    return(combine_error_messages(error_messages))
  } else {
    return(NULL)
  }
}

# Other functions --------------------------------------------------------------

#' Combine Validation and Prediction DFs
#'
#' @param prediction_df A df made from the prediction file
#' @param validation_df A df made from the validation file
#' @param name_columns A character vector of columns used to identify a
#' prediction
#' @param pred_column A string, the column in the prediction df that has the
#' prediction values
#' @param val_column A string, the column in the validation df that has the
#' validation values
#' @export
#' @importFrom dplyr inner_join
combine_validation_prediction_dfs <- function(
  validation_df,
  prediction_df,
  name_columns  = "prediction_name",
  pred_column   = "prediction",
  val_column    = "validation"
){
  dplyr::left_join(
    dplyr::select(validation_df, name_columns, "validation" = val_column),
    dplyr::select(prediction_df, name_columns, "prediction" = pred_column),
    by = name_columns
  )
}

# helpers ----------------------------------------------------------------------

#' @importFrom magrittr %>%
combine_error_messages <- function(error_messages){
  error_messages %>%
    stringr::str_c(collapse = ", ") %>%
    stringr::str_c(".")
}

#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_duplicate_rows_by_name_columns <- function(
  df, name_columns  = "prediction_name"
){
  df %>%
    tidyr::unite("prediction_name", name_columns, sep = ";") %>%
    dplyr::group_by(.data$prediction_name) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::filter(.data$count > 1) %>%
    dplyr::pull("prediction_name")
}

#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_missing_rows_by_name_columns <- function(
  df, name_columns  = "prediction_name"
){
  df %>%
    tidyr::unite("prediction_name", name_columns, sep = ";") %>%
    dplyr::filter(is.na(.data$prediction) | is.nan(.data$prediction)) %>%
    dplyr::pull("prediction_name")
}

#' @importFrom magrittr %>%
#' @importFrom rlang .data
get_duplicate_column_names <- function(df){
  df %>%
    colnames %>%
    dplyr::tibble("col" = .) %>%
    dplyr::group_by(.data$col) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(count > 1) %>%
    dplyr::pull(.data$col)
}

create_duplicate_column_names_message <- function(duplicate_columns){
  stringr::str_c(
    "Prediction file has duplicate columns: ",
    values_to_list_string(duplicate_columns)
  )
}

create_missing_column_names_message <- function(missing_columns){
  stringr::str_c(
    "Prediction file is missing columns: ",
    values_to_list_string(missing_columns)
  )
}

create_duplicate_rows_message <- function(duplicate_rows){
  stringr::str_c(
    "Prediction file has duplicate predictions for: ",
    values_to_list_string(duplicate_rows)
  )
}

create_missing_rows_message <- function(missing_rows){
  stringr::str_c(
    "Prediction file is missing predictions: ",
    values_to_list_string(missing_rows)
  )
}

#' @importFrom magrittr %>%
values_to_list_string <- function(values, sep = ", "){
  values %>%
    unlist %>%
    as.character() %>%
    stringr::str_c(collapse = sep) %>%
    stringr::str_c("[", ., "]")
}


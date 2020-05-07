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
validate_submission <- function(
  prediction_df,
  validation_df,
  name_columns  = "prediction_name",
  pred_column   = "prediction",
  val_column    = "validation"
){
  validate_required_columns(prediction_df, c(name_columns, pred_column))
  prediction_df <- dplyr::select(
    prediction_df, name_columns, "prediction" = pred_column
  )
  validation_df <- dplyr::select(
    validation_df, name_columns, "validation" = val_column
  )
  # validate_complete_df(submission_df, key_cols)
  # submission_df2 <- submission_df %>%
  #   tidyr::unite("key", key_cols, sep = ";") %>%
  #   dplyr::select("key", "prediction" = pred_col)
  # validation_df2 <- validation_df %>%
  #   tidyr::unite("key", key_cols, sep = ";") %>%
  #   dplyr::select("key", "measured" = meas_col) %>%
  #   dplyr::mutate(measured = 0)
  # validate_no_duplicate_rows(submission_df2)
  # validate_prediction_column_complete(submission_df2)
  # validate_combined_df(submission_df2, validation_df2, fail_missing)
}


#' Validate Required Columns
#'
#' @param df A df from a prediction file
#' @param required_columns A character of vector of columns the df should have
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stringr str_c
validate_required_columns <- function(df, required_columns){
  error_messages <- c()

  duplicated_columns <- get_duplicate_column_names(df)
  missing_columns    <- required_columns[!required_columns %in% colnames(df)]

  if (length(duplicated_columns) != 0) {
    error_messages <- create_duplicate_column_names_message(duplicated_columns)
  }
  if (length(missing_columns) != 0) {
    error_messages <- c(
      error_messages, create_missing_column_names_message(missing_columns)
    )
  }

  if (length(error_messages) != 0) {
    error_messages %>%
      stringr::str_c(collapse = ", ") %>%
      stringr::str_c(".") %>%
      return()
  } else {
    return(NULL)
  }
}

# helpers ---------------------------------------------------------------------
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

values_to_list_string <- function(values, sep = ", "){
  values %>%
    unlist %>%
    as.character() %>%
    stringr::str_c(collapse = sep) %>%
    stringr::str_c("[", ., "]")
}


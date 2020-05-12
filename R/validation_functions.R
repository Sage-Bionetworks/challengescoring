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
  validation_df,
  prediction_df,
  name_columns  = "prediction_name",
  pred_column   = "prediction",
  val_column    = "validation"
){
  column_result <- validate_required_columns(prediction_df, c(name_columns, pred_column))
  if (!is.null(column_result)) return(column_result)
  combine_result <- try(combine_validation_prediction_dfs(
    prediction_df, validation_df, name_columns, pred_column, val_column
  ))
  if (inherits(combine_result, "try-error")) return(combine_result)
  else combined_df <- combine_result
  combined_result <- validate_combined_df(combined_df)
  if (!is.null(combined_result)) return(combined_df)
  else return(combined_result)
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
#'
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


validate_combined_df <- function(df, name_columns  = "prediction_name"){
  error_messages <- c()

  duplicated_rows <- df %>%
    tidyr::unite("prediction_name", name_columns, sep = ";") %>%
    dplyr::group_by(.data$prediction_name) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::filter(.data$count > 1) %>%
    dplyr::pull("prediction_name")

  missing_rows <- df %>%
    tidyr::unite("prediction_name", name_columns, sep = ";") %>%
    dplyr::filter(is.na(.data$prediction) | is.nan(.data$prediction))
    dplyr::pull("prediction_name")

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


#' Summarize Response Distribution for a Variable
#'
#' Prints a formatted summary showing counts and percentages for each response
#' category in a specified column.
#'
#' @param df A data frame containing the variable to summarize.
#' @param column Character string or unquoted column name.
#' @param exclude_na Logical. If TRUE (default), excludes NA values from the summary.
#' @param exclude_empty Logical. If TRUE (default), excludes empty strings.
#' @param sort_desc Logical. If TRUE (default), sorts results by count in descending order.
#'
#' @return Invisibly returns a data frame with the summary statistics.
#'
#' @examples
#' \dontrun{
#' response_summary(mydata, "question_column")
#' response_summary(mydata, question_column)
#' response_summary(mydata, "question_column", exclude_na = FALSE)
#' }
#'
#' @import dplyr
#' @importFrom rlang enquo quo_name
#'
#' @export
response_summary <- function(df, column, exclude_na = TRUE, exclude_empty = TRUE, sort_desc = TRUE) {

  # Handle both quoted and unquoted column names
  col_enquo <- enquo(column)
  col_name <- quo_name(col_enquo)

  # If column is already a string, use it directly
  if (is.character(substitute(column))) {
    col_name <- column
  }

  # Start with the data
  summary_df <- df %>%
    count(!!sym(col_name), name = "count")

  # Apply filters if requested
  if (exclude_na) {
    summary_df <- summary_df %>%
      filter(!is.na(!!sym(col_name)))
  }

  if (exclude_empty) {
    summary_df <- summary_df %>%
      filter(!!sym(col_name) != "" & !!sym(col_name) != "NA")
  }

  # Calculate percentages and format
  summary_df <- summary_df %>%
    mutate(
      percentage = count / sum(count) * 100,
      pct_formatted = sprintf("%.1f%%", percentage)
    )

  # Sort if requested
  if (sort_desc) {
    summary_df <- summary_df %>%
      arrange(desc(count))
  }

  # Print formatted output
  cat("\n")
  cat("Summary for:", col_name, "\n")
  cat(strrep("=", nchar(col_name) + 13), "\n\n")

  total <- sum(summary_df$count)

  for (i in 1:nrow(summary_df)) {
    response <- summary_df[[col_name]][i]
    count <- summary_df$count[i]
    pct <- summary_df$pct_formatted[i]

    cat(sprintf("  %s tools (%s) answer: %s\n", count, pct, response))
  }

  cat("\n")
  cat(sprintf("Total responses: %s\n", total))
  cat("\n")

  # Return the summary invisibly for further use
  invisible(summary_df)
}


#' Compact Response Summary (One-line Version)
#'
#' Prints a compact, single-line summary of response distribution.
#'
#' @param df A data frame containing the variable to summarize.
#' @param column Character string or unquoted column name.
#' @param exclude_na Logical. If TRUE (default), excludes NA values.
#' @param exclude_empty Logical. If TRUE (default), excludes empty strings.
#'
#' @return Invisibly returns a data frame with the summary statistics.
#'
#' @examples
#' \dontrun{
#' response_summary_compact(mydata, "question_column")
#' }
#'
#' @import dplyr
#' @importFrom rlang enquo quo_name
#'
#' @export
response_summary_compact <- function(df, column, exclude_na = TRUE, exclude_empty = TRUE) {

  # Handle both quoted and unquoted column names
  col_enquo <- enquo(column)
  col_name <- quo_name(col_enquo)

  if (is.character(substitute(column))) {
    col_name <- column
  }

  # Calculate summary
  summary_df <- df %>%
    count(!!sym(col_name), name = "count")

  if (exclude_na) {
    summary_df <- summary_df %>%
      filter(!is.na(!!sym(col_name)))
  }

  if (exclude_empty) {
    summary_df <- summary_df %>%
      filter(!!sym(col_name) != "" & !!sym(col_name) != "NA")
  }

  summary_df <- summary_df %>%
    mutate(percentage = count / sum(count) * 100) %>%
    arrange(desc(count))

  # Build compact string
  parts <- sapply(1:nrow(summary_df), function(i) {
    response <- summary_df[[col_name]][i]
    count <- summary_df$count[i]
    pct <- sprintf("%.1f%%", summary_df$percentage[i])
    sprintf("%s (%s) '%s'", count, pct, response)
  })

  cat(paste(parts, collapse = ", "), "\n")

  invisible(summary_df)
}

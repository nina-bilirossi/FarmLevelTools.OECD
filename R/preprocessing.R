#' Create a new dataframe to disaggregate variable for which there are several entries.
#'
#' The variable is typically a list of values separated by a comma. The resulting dataframe contains one row for each user type for each tool.
#' In other words, if a tool is designed for 2 distinct user types, the resulting dataframe will have 2 rows for that tool.
#' @param df your dataframe
#' @param target_col a column containing multiple values to be disaggregated, all separated by a comma
#' @examples
#' df_wide <- disaggregate_target(database, "Main.target.user.or.client.group")
#' plot_category_counts(df_wide, "Tool")
#' @export
disaggregate_target <- function(df, target_col) {
  # Split the comma-separated values and create long format
  df_long <- df |>
    dplyr::select(dplyr::all_of(c("Tool", target_col))) |>
    # Convert to character and replace blanks with "NA"
    dplyr::mutate(!!rlang::sym(target_col) := as.character(!!rlang::sym(target_col)),
                  !!rlang::sym(target_col) := dplyr::if_else(
                    !!rlang::sym(target_col) == "" | is.na(!!rlang::sym(target_col)),
                    "NA",
                    !!rlang::sym(target_col)
                  )) |>
    tidyr::separate_rows(!!rlang::sym(target_col), sep = ",") |>
    dplyr::mutate(!!rlang::sym(target_col) := trimws(!!rlang::sym(target_col))) |>
    dplyr::filter(!!rlang::sym(target_col) != "") |>
    dplyr::distinct(Tool, !!rlang::sym(target_col), .keep_all = TRUE) |>
    dplyr::mutate(value = 1)

  #print(df_long, n = 500)
  # Pivot to wide format
  df_wide <- df_long |>
    tidyr::pivot_wider(
      names_from = !!rlang::sym(target_col),
      values_from = value,
      values_fill = 0
    )

  return(df_wide)
}


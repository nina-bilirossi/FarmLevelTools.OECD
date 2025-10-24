#' Get combination statistics for a categorical variable
#'
#' Calculates frequency counts and percentages for each unique value in a categorical variable.
#' Automatically filters out NA values, empty strings, and "#N/A" entries.
#'
#' @param database A dataframe containing the data
#' @param category_var The column name (unquoted) to analyze
#' @param descending Logical. If TRUE, orders results in descending order by count. If FALSE, ascending. Default is TRUE
#' @return A dataframe with columns: category value, count (n), and percentage
#' @examples
#' get_combo_stats(database, Approach.for.Farm.level.data.quality.assurance)
#' get_combo_stats(database, Country, descending = FALSE)
#' @export
get_combo_stats <- function(database,
                            category_var,
                            descending = TRUE) {
  # Filter out NA, empty strings, and #N/A
  data_filtered <- database |>
    dplyr::filter(!is.na({{category_var}}),
                  {{category_var}} != "")
  
                  # uncomment this line to kick out NAs
                  # ,{{category_var}} != "#N/A")
  
  # Count occurrences and calculate percentages
  category_stats <- data_filtered |>
    dplyr::count({{category_var}}, name = "n") |>
    dplyr::mutate(percentage = round(n / sum(n) * 100, 1))
  
  # Order based on descending parameter
  if (descending) {
    category_stats <- category_stats |> dplyr::arrange(dplyr::desc(n))
  } else {
    category_stats <- category_stats |> dplyr::arrange(n)
  }
  
  return(category_stats)
}



#' A function that creates a new dataframe to disaggregate variable for which there are several entries.
#' The varaible is typically a list of values separated by a coma. The resulting dataframe contains one row for each user type for ech tool.
#' In other words, if a tool is designed for 2 distinct user types, the resulting dataframe will have 2 rows for that tool.
#' @param df your dataframe
#' @param target_col a column containing multiple values to be disaggregated, all separated by a coma
#' @examples
#' df_wide <- disaggregate_target(database, "Main.target.user.or.client.group")
#' @export
disaggregate_target <- function(df, target_col) {
  # Split the comma-separated values and create long format
  df_long <- df %>%
    select(all_of(c("Tool", target_col))) %>%
    separate_rows(!!sym(target_col), sep = ",") %>%
    mutate(!!sym(target_col) := trimws(!!sym(target_col))) %>%  # Trim whitespace
    filter(!!sym(target_col) != "" & !is.na(!!sym(target_col))) %>%     # Remove empty values
    mutate(value = 1)     # Add indicator column

  # Pivot to wide format
  df_wide <- df_long %>%
    pivot_wider(
      names_from = !!sym(target_col),
      values_from = value,
      values_fill = 0
    )

  return(df_wide)
}

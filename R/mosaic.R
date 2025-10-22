#' Create a Binary Mosaic Plot
#'
#' Creates a mosaic plot for visualizing the relationship between two categorical
#' variables, with column names passed as strings for programmatic use.
#'
#' @param df A data frame containing the variables to plot.
#' @param x Character string. Name of the column to use for the x-axis.
#' @param y Character string. Name of the column to use for the y-axis (fill).
#' @param title Character string. Title for the plot. If NA (default), an automatic
#'   title will be generated.
#' @param show_pct Logical. If TRUE, displays percentage labels on the plot.
#'   Default is FALSE.
#'
#' @return A ggplot2 object representing the mosaic plot.
#'
#' @details The function automatically filters out missing values, empty strings,
#'   and "NA" text values from both variables. The x variable is factored with
#'   levels c("Yes", "No").
#'
#' @examples
#' \dontrun{
#' mosaic_bin2(mydata, "variable1", "variable2")
#' mosaic_bin2(mydata, "variable1", "variable2",
#'             title = "Custom Title", show_pct = TRUE)
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang sym .data
#' @importFrom ggmosaic geom_mosaic theme_mosaic
#' @importFrom scales percent
#'
#' @export
mosaic_bin2 <- function(df, x, y, title = NA, show_pct = FALSE) {
  # x and y are strings
  x_name <- x
  y_name <- y

  df <- df %>%
    filter(
      !is.na(!!sym(x_name)),
      .data[[x_name]] != "NA",
      .data[[x_name]] != "",
      !is.na(!!sym(y_name)),
      .data[[y_name]] != ""
    )

  # Compute counts per combination
  df_sum <- df %>%
    count(!!sym(x_name), !!sym(y_name), name = "n") %>%
    mutate(
      !!sym(x_name) := factor(.data[[x_name]],
                              levels = c("Yes", "No")),
      pct = n / sum(n)
    )

  # Build the plot
  if (is.na(title)) {
    title <- paste("Distribution of Tools by", x_name, "and", y_name)
  }

  p <- ggplot(df_sum) +
    geom_mosaic(aes(
      x = product(!!sym(x_name)),
      fill = !!sym(y_name),
      weight = n
    )) +
    labs(
      x = x_name,
      y = y_name,
      title = title
    ) +
    theme_mosaic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none"
    )

  # Optional percentage labels
  if (show_pct) {
    df_labels <- df_sum %>%
      group_by(.data[[y_name]]) %>%
      mutate(
        xpos = as.numeric(factor(.data[[x_name]])) - 0.5 / n(),
        ypos = 0.5
      )

    p <- p +
      geom_text(
        data = df_labels,
        aes(x = xpos, y = ypos, label = percent(pct, accuracy = 0.1)),
        inherit.aes = FALSE,
        size = 3,
        color = "black"
      )
  }

  return(p)
}


#' Create Summary Table from Mosaic Plot Data
#'
#' Generates a summary table with counts and percentages for two categorical
#' variables, matching the data used in mosaic plots.
#'
#' @param df A data frame containing the variables to summarize.
#' @param x Unquoted column name for the grouping variable.
#' @param y Unquoted column name for the fill/category variable.
#'
#' @return A tibble containing counts, percentages within each x category
#'   (pct_within_x), and percentages of the total (pct_total). Includes
#'   subtotal rows for each x category.
#'
#' @details The function applies the same data cleaning filters as
#'   \code{mosaic_bin2}, removing missing values, empty strings, and "NA" text.
#'   Percentages are rounded to one decimal place.
#'
#' @examples
#' \dontrun{
#' mosaic_summary_table(mydata, Aligned.with.national.inventory,
#'                      External.certification.against.reporting.standard)
#' }
#'
#' @import dplyr
#' @importFrom rlang sym .data
#'
#' @export
mosaic_summary_table <- function(df, x, y) {
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))

  # Apply same filters as mosaic function
  df_clean <- df %>%
    filter(
      !is.na(!!sym(x_name)),
      .data[[x_name]] != "NA",
      .data[[x_name]] != "",
      !is.na(!!sym(y_name)),
      .data[[y_name]] != ""
    )

  # Create summary with counts and percentages
  summary_table <- df_clean %>%
    count(!!sym(x_name), !!sym(y_name), name = "count") %>%
    group_by(!!sym(x_name)) %>%
    mutate(
      pct_within_x = count / sum(count) * 100,
      pct_total = count / nrow(df_clean) * 100
    ) %>%
    ungroup() %>%
    arrange(!!sym(x_name), !!sym(y_name))

  # Add totals row
  totals <- summary_table %>%
    group_by(!!sym(x_name)) %>%
    summarise(
      !!sym(y_name) := "TOTAL",
      count = sum(count),
      pct_within_x = 100,
      pct_total = sum(pct_total)
    )

  # Combine and format
  final_table <- bind_rows(summary_table, totals) %>%
    mutate(
      pct_within_x = round(pct_within_x, 1),
      pct_total = round(pct_total, 1)
    )

  return(final_table)
}

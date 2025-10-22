#' Create a Mosaic-Style Plot for Two Binary Variables
#'
#' Creates a tile-based visualization showing the relationship between two
#' categorical variables, with tile sizes proportional to counts.
#'
#' @param df A data frame containing the variables to plot.
#' @param x Character string. Name of the column for the x-axis.
#' @param y Character string. Name of the column for the y-axis (fill).
#' @param title Character string. Title for the plot. If NA (default), an
#'   automatic title will be generated.
#'
#' @return A ggplot2 object representing the mosaic-style plot.
#'
#' @details The function automatically filters out missing values and empty
#'   strings from both variables. Creates a tile plot where tile width
#'   represents the proportion within each x category.
#'
#' @examples
#' \dontrun{
#' mosaic_plot(mydata, "variable1", "variable2")
#' mosaic_plot(mydata, "variable1", "variable2", title = "Custom Title")
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang sym .data
#'
#' @export
mosaic_plot <- function(df, x, y, title = NA) {
  x_name <- x
  y_name <- y

  # Filter and prepare data
  df_clean <- df %>%
    filter(
      !is.na(!!sym(x_name)),
      .data[[x_name]] != "NA",
      .data[[x_name]] != "",
      !is.na(!!sym(y_name)),
      .data[[y_name]] != ""
    ) %>%
    mutate(
      !!sym(x_name) := factor(.data[[x_name]], levels = c("Yes", "No"))
    )

  # Calculate proportions for positioning
  df_plot <- df_clean %>%
    count(!!sym(x_name), !!sym(y_name), name = "n") %>%
    group_by(!!sym(x_name)) %>%
    mutate(
      total_x = sum(n),
      prop_y = n / total_x,
      ymax = cumsum(prop_y),
      ymin = lag(ymax, default = 0)
    ) %>%
    ungroup() %>%
    mutate(
      total = sum(n),
      prop_x = total_x / total,
      xmax = cumsum(prop_x),
      xmin = lag(xmax, default = 0)
    ) %>%
    group_by(!!sym(x_name)) %>%
    mutate(
      xmax_adj = xmax,
      xmin_adj = xmin
    ) %>%
    ungroup()

  # Generate title if not provided
  if (is.na(title)) {
    title <- paste("Distribution by", x_name, "and", y_name)
  }

  # Create plot
  p <- ggplot(df_plot, aes(
    xmin = xmin_adj, xmax = xmax_adj,
    ymin = ymin, ymax = ymax,
    fill = !!sym(y_name)
  )) +
    geom_rect(color = "white", size = 1) +
    scale_x_continuous(
      breaks = df_plot %>%
        group_by(!!sym(x_name)) %>%
        summarise(pos = mean(c(xmin_adj, xmax_adj))) %>%
        pull(pos),
      labels = df_plot %>%
        distinct(!!sym(x_name)) %>%
        pull(!!sym(x_name))
    ) +
    labs(
      title = title,
      x = x_name,
      y = "Proportion",
      fill = y_name
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )

  return(p)
}


#' Create Summary Table from Mosaic Plot Data
#'
#' Generates a summary table with counts and percentages for two categorical
#' variables.
#'
#' @param df A data frame containing the variables to summarize.
#' @param x Unquoted column name for the grouping variable.
#' @param y Unquoted column name for the fill/category variable.
#'
#' @return A tibble containing counts, percentages within each x category
#'   (pct_within_x), and percentages of the total (pct_total). Includes
#'   subtotal rows for each x category.
#'
#' @examples
#' \dontrun{
#' mosaic_summary_table(mydata, variable1, variable2)
#' }
#'
#' @import dplyr
#' @importFrom rlang sym .data
#'
#' @export
mosaic_summary_table <- function(df, x, y) {
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))

  # Apply same filters
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

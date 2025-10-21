#' Mosaic Plot Using String Column Names
#'
#' This function creates a mosaic plot showing the relationship between two categorical
#' variables, where the variable names are supplied as strings. This version is useful
#' for programmatic workflows such as loops or `purrr::map()`.
#'
#' @param df A data frame containing the variables to be plotted.
#' @param x A string giving the name of the categorical variable for the x-axis.
#' @param y A string giving the name of the categorical variable for the fill variable.
#' @param title Optional plot title. If `NA` (default), a title is generated automatically.
#' @param show_pct Logical; if `TRUE`, displays percentage labels on the plot.
#'
#' @return A `ggplot` object displaying the mosaic plot.
#' @examples
#' \dontrun{
#' library(ggmosaic)
#' library(dplyr)
#'
#' data <- data.frame(
#'   ToolUsed = sample(c("Yes", "No"), 100, replace = TRUE),
#'   Category = sample(c("A", "B", "C"), 100, replace = TRUE)
#' )
#'
#' mosaic_bin2(data, "ToolUsed", "Category", show_pct = TRUE)
#' }
#'
#' @importFrom dplyr filter count mutate group_by ungroup arrange summarise bind_rows
#' @importFrom ggplot2 ggplot aes geom_text labs theme element_text
#' @importFrom ggmosaic geom_mosaic theme_mosaic product
#' @importFrom rlang sym .data
#' @importFrom scales percent
#' @export
mosaic_bin2 <- function(df, x, y, title = NA, show_pct = FALSE) {
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

  df_sum <- df %>%
    count(!!sym(x_name), !!sym(y_name), name = "n") %>%
    mutate(
      !!sym(x_name) := factor(.data[[x_name]], levels = c("Yes", "No")),
      pct = n / sum(n)
    )

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


#' Create a Summary Table for Mosaic Plot Data
#'
#' This function generates a summary table that mirrors the data used in a mosaic plot,
#' including counts and percentage breakdowns by each category.
#'
#' @param df A data frame containing the variables to summarize.
#' @param x The categorical variable for the x-axis (unquoted).
#' @param y The categorical variable for the fill variable (unquoted).
#'
#' @return A data frame containing counts, within-group percentages, and total percentages.
#' @examples
#' \dontrun{
#' mosaic_summary_table(mtcars, cyl, gear)
#' }
#'
#' @importFrom dplyr filter count mutate group_by ungroup arrange summarise bind_rows
#' @importFrom rlang sym .data
#' @export
mosaic_summary_table <- function(df, x, y) {
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))

  df_clean <- df %>%
    filter(
      !is.na(!!sym(x_name)),
      .data[[x_name]] != "NA",
      .data[[x_name]] != "",
      !is.na(!!sym(y_name)),
      .data[[y_name]] != ""
    )

  summary_table <- df_clean %>%
    count(!!sym(x_name), !!sym(y_name), name = "count") %>%
    group_by(!!sym(x_name)) %>%
    mutate(
      pct_within_x = count / sum(count) * 100,
      pct_total = count / nrow(df_clean) * 100
    ) %>%
    ungroup() %>%
    arrange(!!sym(x_name), !!sym(y_name))

  totals <- summary_table %>%
    group_by(!!sym(x_name)) %>%
    summarise(
      !!sym(y_name) := "TOTAL",
      count = sum(count),
      pct_within_x = 100,
      pct_total = sum(pct_total)
    )

  final_table <- bind_rows(summary_table, totals) %>%
    mutate(
      pct_within_x = round(pct_within_x, 1),
      pct_total = round(pct_total, 1)
    )

  return(final_table)
}

#' Create a bar chart of a binary variable (Yes/No) against a categorical variable
#' @param bin_x binary variable
#' @param cat_y categorical variable
#' @param title Defaults to ""
#' @param x_title title, for the x axis, defaults to ""
#' @param legend_title defaults to ""
#' @param caption optional caption (e.g., "*Excluded 1 tool, open-source status = Pending.")
#' @export
binary_X_categorical_Y_chart <- function(database, bin_x, cat_y, title = "",
                                         x_title = "", legend_title = "", caption = NA) {
  data_filtered <- database |>
    dplyr::filter(!is.na({{bin_x}}), {{bin_x}} != "", {{bin_x}} != "Pending",
                  !is.na({{cat_y}}), {{cat_y}} != "") |>
    dplyr::mutate(
      bin_x = factor({{bin_x}}),
      cat_y = factor({{cat_y}})
    ) |>
    dplyr::mutate(
      bin_x = factor(
        {{bin_x}},
        levels = c("Yes", "No", "Not applicable (model-agnostic)", "NA")
      ),
      cat_y = factor({{cat_y}})
    )

  n_tools <- nrow(data_filtered)

  if(is.na(caption)){
    caption <- glue::glue('Number of tools: {n_tools}')
  }

  # Plot
  ggplot2::ggplot(data_filtered, ggplot2::aes(x = bin_x, fill = cat_y)) +
    ggplot2::geom_bar() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title,
      x = x_title,
      y = "number of tools",
      fill = legend_title,
      caption = caption
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}


#' Create a simple bar chart for binary variables
#' @param bin_x the binary variable to be shown, a column name in the database
#' @param title chart title (default is no title)
#' @export
yes_no_histogram <- function(database, bin_x, title = "") {
  data_filtered <- database |>
    dplyr::filter(!is.na({{bin_x}}),
                  {{bin_x}} != "") |>
    dplyr::mutate(
      `Aligned.with.national.inventory.` = factor(
        {{bin_x}},
        levels = c("Yes", "No", "Not Applicable (model-agnostic)")
      )
    )

  # Compute percentages
  plot_data <- data_filtered |>
    dplyr::group_by({{bin_x}}) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(percent = count / sum(count) * 100)

  # Plot bar chart with percentages
  ggplot2::ggplot(plot_data, ggplot2::aes(x = {{bin_x}}, y = percent)) +
    ggplot2::geom_col(fill = "#3B82F6", width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(percent, 1), "%")),
                       vjust = -0.5, size = 4.2, color = "black") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(
      title = title,
      x = "",
      y = "% of tools"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, vjust = 1),
      plot.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::ylim(0, max(plot_data$percent) * 1.15)  # add headroom for text labels
}


#' Plot Counts of Categorical Variables from Wide-Format Data
#'
#' Creates a bar chart showing the sum of values for each categorical column in
#' a wide-format data frame. Useful for visualizing the distribution of binary
#' or count data across multiple categories.
#'
#' @param df_wide A data frame in wide format where columns represent categories
#'   and rows represent observations. Values should be numeric (typically binary
#'   0/1 indicators or counts).
#' @param id_col Character vector specifying the name(s) of identifier column(s)
#'   to exclude from counting. If \code{NULL} (default), all columns will be
#'   counted.
#' @param title Character string for the plot title. Default is "Category Counts".
#' @param x_label Character string for the x-axis label. Default is "Category".
#' @param y_label Character string for the y-axis label. Default is "Count".
#' @param fill_color Character string specifying the fill color for bars.
#'   Default is "darkgreen". Can be any valid color name or hex code.
#' @param sort_descending Logical indicating whether to sort categories by count
#'   in descending order. Default is \code{TRUE}.
#' @param show_counts Logical indicating whether to display count labels above
#'   bars. Default is \code{TRUE}.
#' @param top_n Numberindicating whether how many categories to be displayed (starting from the most frequent).
#' Default is \code{NULL}.
#'
#' @return A \code{ggplot2} object that can be further customized or displayed.
#'
#' @details
#' This function is designed to work with wide-format data where each column
#' represents a categorical variable and contains numeric values (often binary
#' indicators). The function sums the values in each column and creates a bar
#' chart to visualize the distribution.
#'
#' The resulting plot uses a minimal theme with rotated x-axis labels (45 degrees)
#' for better readability when category names are long.
#'
#' @examples
#' df_wide <- disaggregate_target(database, "Main.target.user.or.client.group")
#' plot_category_counts(df_wide, "Tool")
#' @export
plot_category_counts <- function(df_wide,
                                 id_col = NULL,
                                 title = "Category Counts",
                                 x_label = "Category",
                                 y_label = "Count",
                                 fill_color = "darkgreen",
                                 sort_descending = TRUE,
                                 show_counts = TRUE,
                                 top_n = NULL) {

  # Determine which columns to count
  if (is.null(id_col)) {
    # If no ID column specified, use all columns
    category_cols <- names(df_wide)
  } else {
    # Remove the identifier column(s)
    category_cols <- setdiff(names(df_wide), id_col)
  }

  # Calculate counts for each category
  category_counts <- data.frame(
    category = category_cols,
    count = colSums(df_wide[, category_cols, drop = FALSE])
  )

  # Sort by count if requested
  if (sort_descending) {
    category_counts <- category_counts[order(-category_counts$count), ]
  }

  # Keep only top N categories if specified
  if (!is.null(top_n) && top_n > 0) {
    category_counts <- head(category_counts, top_n)
  }

  # Set factor levels to preserve order
  category_counts$category <- factor(category_counts$category,
                                     levels = category_counts$category)

  # Create bar chart
  p <- ggplot2::ggplot(category_counts, ggplot2::aes(x = category, y = count)) +
    ggplot2::geom_bar(stat = "identity", fill = fill_color) +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )

  # Add count labels if requested
  if (show_counts) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = count), vjust = -0.5, size = 3.5)
  }

  return(p)
}

#' Create an ordered horizontal bar chart for categorical variables
#'
#' Creates a horizontal bar chart with categories ordered by frequency (descending).
#' Automatically filters out NA values, empty strings, and "#N/A" entries.
#'
#' @param database A dataframe containing the data
#' @param category_var The column name (unquoted) to be plotted
#' @param title Character string for the plot title. Default is "Distribution of Categories"
#' @param x_label Character string for the x-axis label (left side in horizontal chart). Default is ""
#' @param y_label Character string for the y-axis label (bottom in horizontal chart). Default is "Count"
#' @param fill_color Character string specifying the fill color for bars. Default is "steelblue"
#' @param descending Logical. If TRUE, orders bars in descending order. If FALSE, ascending. Default is TRUE
#' @return A ggplot2 object
#' @examples
#' ordered_horizontal_barchart(database, Initiative...Partnership.type,
#'                             title = "Classification of organizations")
#' ordered_horizontal_barchart(database, Country, title = "Tools by Country")
#' @export
ordered_horizontal_barchart <- function(database,
                                        category_var,
                                        title = "Distribution of Categories",
                                        x_label = "",
                                        y_label = "Count",
                                        fill_color = "steelblue",
                                        descending = TRUE) {

  # Filter out NA, empty strings, and #N/A
  data_filtered <- database |>
    dplyr::filter(!is.na({{category_var}}),
                  {{category_var}} != "",
                  {{category_var}} != "#N/A")

  # Count occurrences and create ordering
  category_counts <- data_filtered |>
    dplyr::count({{category_var}}, name = "value")

  # Order based on descending parameter
  if (descending) {
    category_counts <- category_counts |> dplyr::arrange(dplyr::desc(value))
  } else {
    category_counts <- category_counts |> dplyr::arrange(value)
  }

  # Reorder factor levels
  data_filtered <- data_filtered |>
    dplyr::mutate({{category_var}} := factor(
      {{category_var}},
      levels = category_counts |> dplyr::pull({{category_var}})
    ))

  # Plot horizontal bar chart
  ggplot2::ggplot(data_filtered, ggplot2::aes(x = {{category_var}})) +
    ggplot2::geom_bar(fill = fill_color) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = y_label
    )
}

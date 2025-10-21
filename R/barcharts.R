

#' This function allows to create a bar chart of a binary variable (Yes/No) against a categorical variable
#' @param bin_x binary variable
#' @param cat_y categorical variable
#' @param title Defaults to ""
#' @param x_title title, for the x axis, defaults to ""
#' @param legend_title defaults to ""
#' @param caption optional caption (e.g., "*Excluded 1 tool, open-source status = Pending.")
#' @export
binary_X_categorical_Y_chart <- function(database, bin_x, cat_y, title = "",
                                         x_title = "", legend_title = "", caption = NA) {
  data_filtered <- database %>%
    filter(!is.na({{bin_x}}), {{bin_x}} != "", {{bin_x}} != "Pending",
           !is.na({{cat_y}}), {{cat_y}} != "")
  n_tools <- nrow(data_filtered)

  # Convert to factors
  data_filtered <- data_filtered %>%
    mutate(
      bin_x = factor({{bin_x}}),
      cat_y = factor({{cat_y}})
    ) %>%
    mutate(
      bin_x = factor(
        {{bin_x}},
        levels = c("Yes", "No", "Not applicable (model-agnostic)", "NA")
      ),
      cat_y = factor({{cat_y}})
    )

  if(is.na(caption)){
    caption <- glue('Number of tools: {n_tools}')
  }

  # Plot
  ggplot(data_filtered, aes(x = bin_x, fill = cat_y)) +
    geom_bar() +
    theme_minimal() +
    labs(
      title = title,
      x = x_title,
      y = "number of tools",
      fill = legend_title
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(caption = caption)
}


#' This function creates a simple bar chart for binary variables
#' @param bin_x the binary variable to be shown, a column name in the database
#' @param title chart title (default is no title)
#' @importFrom magrittr %>%
#' @export
yes_no_histogram <- function(database, bin_x, title = "") {
  data_filtered <- database %>%
    filter(!is.na({{bin_x}}),
           {{bin_x}} != "") %>%
    mutate(
      `Aligned.with.national.inventory.` = factor(
        {{bin_x}},
        levels = c("Yes", "No", "Not Applicable (model-agnostic)")
      )
    )

  # Compute percentages
  plot_data <- data_filtered %>%
    group_by({{bin_x}}) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(percent = count / sum(count) * 100)

  # Plot bar chart with percentages
  ggplot(plot_data, aes(x = {{bin_x}}, y = percent)) +
    geom_col(fill = "#3B82F6", width = 0.6) +
    geom_text(aes(label = paste0(round(percent, 1), "%")),
              vjust = -0.5, size = 4.2, color = "black") +
    theme_minimal(base_size = 13) +
    labs(
      title = title,
      x = "",
      y = "% of tools"
    ) +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
      plot.title = element_text(face = "bold")
    ) +
    ylim(0, max(plot_data$percent) * 1.15)  # add headroom for text labels
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
#' plot_target_user_counts(df_wide, "Main.target.user.or.client.group")
#' @importFrom ggplot2 ggplot aes geom_bar geom_text labs theme_minimal theme
#'   element_text
#' @export
plot_category_counts <- function(df_wide,
                                 id_col = NULL,
                                 title = "Category Counts",
                                 x_label = "Category",
                                 y_label = "Count",
                                 fill_color = "darkgreen",
                                 sort_descending = TRUE,
                                 show_counts = TRUE) {

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

  # Set factor levels to preserve order
  category_counts$category <- factor(category_counts$category,
                                     levels = category_counts$category)

  # Create bar chart
  p <- ggplot(category_counts, aes(x = category, y = count)) +
    geom_bar(stat = "identity", fill = fill_color) +
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )

  # Add count labels if requested
  if (show_counts) {
    p <- p + geom_text(aes(label = count), vjust = -0.5, size = 3.5)
  }

  return(p)
}

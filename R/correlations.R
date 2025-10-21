#' Create a correlation matrix for binary categorical variables
#'
#' Calculates correlations between columns in a wide-format dataframe,
#' typically used to analyze co-occurrence patterns in binary indicator data.
#'
#' @param df_wide A dataframe in wide format where columns represent categories
#'   and contain binary (0/1) or numeric values
#' @param id_col Character vector specifying the name(s) of identifier column(s)
#'   to exclude from correlation calculation. Default is "Tool"
#' @param method Character string indicating which correlation coefficient to compute.
#'   Options are "pearson" (default), "kendall", or "spearman"
#' @param round_digits Integer. Number of decimal places to round results. Default is 2
#' @return A correlation matrix
#' @examples
#' df_wide <- disaggregate_target(database, "Main.target.user.or.client.group")
#' cor_matrix <- create_correlation_matrix(df_wide, id_col = "Tool")
#' @export
create_correlation_matrix <- function(df_wide,
                                      id_col = "Tool",
                                      method = "pearson",
                                      round_digits = 2) {
  # Remove the identifier column(s)
  target_cols <- setdiff(names(df_wide), id_col)

  # Extract only the numeric columns
  binary_matrix <- df_wide[, target_cols, drop = FALSE]

  # Calculate correlation matrix
  cor_matrix <- stats::cor(binary_matrix, method = method)

  # Round for readability
  cor_matrix <- round(cor_matrix, round_digits)

  return(cor_matrix)
}


#' Plot correlation matrix as a heatmap
#'
#' Creates a heatmap visualization of a correlation matrix with annotations
#' showing the correlation values.
#'
#' @param cor_matrix A correlation matrix (typically from create_correlation_matrix)
#' @param title Character string for the plot title. Default is "Correlation Heatmap"
#' @param x_label Character string for the x-axis label. Default is "Variable"
#' @param y_label Character string for the y-axis label. Default is "Variable"
#' @param low_color Character string specifying color for low correlation values. Default is "blue"
#' @param high_color Character string specifying color for high correlation values. Default is "red"
#' @param mid_color Character string specifying color for middle correlation values. Default is "white"
#' @param midpoint Numeric value for the midpoint of the color scale. Default is 0
#' @param show_values Logical. If TRUE, displays correlation values on tiles. Default is TRUE
#' @param text_size Numeric. Size of correlation value text. Default is 3
#' @param angle_x Numeric. Angle of x-axis text. Default is 45
#' @return A ggplot2 object
#' @examples
#' df_wide <- disaggregate_target(database, "Main.target.user.or.client.group")
#' cor_matrix <- create_correlation_matrix(df_wide)
#' plot_correlation_heatmap(cor_matrix, title = "Target User Co-occurrence")
#' @export
plot_correlation_heatmap <- function(cor_matrix,
                                     title = "Correlation Heatmap",
                                     x_label = "Variable",
                                     y_label = "Variable",
                                     low_color = "blue",
                                     high_color = "red",
                                     mid_color = "white",
                                     midpoint = 0,
                                     show_values = TRUE,
                                     text_size = 3,
                                     angle_x = 45) {

  # Convert correlation matrix to long format
  cor_melted <- data.frame(
    Var1 = rep(rownames(cor_matrix), each = ncol(cor_matrix)),
    Var2 = rep(colnames(cor_matrix), times = nrow(cor_matrix)),
    value = as.vector(cor_matrix)
  )

  # Create base heatmap
  p <- ggplot2::ggplot(cor_melted, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(
      low = low_color,
      high = high_color,
      mid = mid_color,
      midpoint = midpoint,
      limit = c(-1, 1),
      space = "Lab",
      name = "Correlation"
    ) +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = angle_x, hjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    ) +
    ggplot2::coord_fixed()

  # Add text values if requested
  if (show_values) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = value), size = text_size)
  }

  return(p)
}

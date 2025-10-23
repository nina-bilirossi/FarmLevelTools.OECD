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
  n_tools = nrow(data_filtered)
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
      y = "% of tools",
      caption = paste0("Number of tools: ", n_tools)
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

  n_tools = nrow(df_wide)
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
      y = y_label,
      caption = paste0("Number of tools: ", n_tools)
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

#' Create a bar chart of a categorical variable against a binary variable (Yes/No)
#'
#' @param database A data frame containing the variables to plot.
#' @param cat_x Categorical variable for the x-axis.
#' @param bin_y Binary variable for the fill/color (Yes/No).
#' @param title Plot title. Defaults to "".
#' @param x_title X-axis label. Defaults to "".
#' @param legend_title Legend title. Defaults to "".
#' @param caption Optional caption. If NA (default), shows number of tools.
#' @param position Bar position: "stack" (default), "dodge", or "fill".
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' categorical_X_binary_Y_chart(database, Type.of.tool, Open.source)
#' categorical_X_binary_Y_chart(database, Type.of.tool, Open.source,
#'                               position = "dodge")
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom glue glue
#'
#' @export
categorical_X_binary_Y_chart <- function(database, cat_x, bin_y, title = "",
                                         x_title = "", legend_title = "",
                                         caption = NA, position = "stack") {

  data_filtered <- database |>
    dplyr::filter(!is.na({{cat_x}}), {{cat_x}} != "",
                  !is.na({{bin_y}}), {{bin_y}} != "", {{bin_y}} != "Pending") |>
    dplyr::mutate(
      cat_x = factor({{cat_x}}),
      bin_y = factor(
        {{bin_y}},
        levels = c("Yes", "No", "Not applicable (model-agnostic)", "NA")
      )
    )

  n_tools <- nrow(data_filtered)

  if (is.na(caption)) {
    caption <- glue::glue('Number of tools: {n_tools}')
  }

  # Plot
  ggplot2::ggplot(data_filtered, ggplot2::aes(x = cat_x, fill = bin_y)) +
    ggplot2::geom_bar(position = position) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title,
      x = x_title,
      y = "number of tools",
      fill = legend_title,
      caption = caption
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )
}

#' Plot Geographic Scope of Tools
#'
#' Creates a bar chart showing the distribution of tools by their geographic scope,
#' categorizing them by number of countries covered or regional scope (Global/Europe).
#'
#' @param df A data frame containing tool information with geographic scope data.
#' @param country_col Character string specifying the column name containing country/scope information.
#'   Default is "Countries..Scope".
#' @param title Character string for the plot title.
#'   Default is "Number of Tools by Geographic Scope".
#' @param fill_color Character string specifying the bar fill color.
#'   Default is "steelblue".
#'
#' @return A ggplot2 object showing the distribution of tools by geographic scope.
#'   Also prints summary statistics to the console.
#'
#' @details
#' The function categorizes tools into the following scope categories:
#' \itemize{
#'   \item Global: Tools marked as "Global"
#'   \item Europe: Tools marked as "Europe"
#'   \item 1-5 countries: Tools covering specific numbers of countries
#'   \item 6+ countries: Tools covering six or more countries
#'   \item Unspecified: Tools with no scope information
#' }
#'
#' Countries should be comma-separated in the scope column. The function
#' excludes "Global" and "Europe" when counting individual countries.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' plot_country_scope(database)
#'
#' # Custom parameters
#' plot_country_scope(
#'   df = my_data,
#'   country_col = "geographic_scope",
#'   title = "Tool Distribution by Region",
#'   fill_color = "coral"
#' )
#' }
#'
#' @import ggplot2
#' @importFrom dplyr filter mutate group_by summarise case_when sym
#'
#' @export
plot_country_scope <- function(df,
                               country_col = "Countries..Scope",
                               title = "Number of Tools by Geographic Scope",
                               fill_color = "steelblue") {

  # Validate inputs
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }

  if (!country_col %in% names(df)) {
    stop(paste0("Column '", country_col, "' not found in data frame"))
  }

  # Filter out empty scope entries
  clean_df <- df |>
    dplyr::filter(.data[[country_col]] != "")

  n_tools <- nrow(clean_df)

  if (n_tools == 0) {
    warning("No tools with non-empty geographic scope found")
    return(NULL)
  }

  # Create a categorization of scope
  scope_summary <- clean_df |>
    dplyr::mutate(
      # Split countries by comma and trim whitespace
      countries_list = strsplit(as.character(.data[[country_col]]), ","),
      countries_clean = lapply(.data$countries_list, function(x) trimws(x)),

      # Count number of countries (excluding Global and Europe)
      num_countries = sapply(.data$countries_clean, function(x) {
        # Remove empty strings, NA, Global, and Europe
        x_filtered <- x[x != "" & !is.na(x) &
                          tolower(x) != "global" &
                          tolower(x) != "europe"]
        length(x_filtered)
      }),

      # Check if Global or Europe is present
      has_global = sapply(.data$countries_clean, function(x) {
        any(tolower(x) %in% c("global"))
      }),
      has_europe = sapply(.data$countries_clean, function(x) {
        any(tolower(x) %in% c("europe"))
      }),

      # Create scope category
      scope_category = dplyr::case_when(
        .data$has_global ~ "Global",
        .data$has_europe ~ "Europe",
        .data$num_countries == 0 ~ "Unspecified",
        .data$num_countries == 1 ~ "1 country",
        .data$num_countries == 2 ~ "2 countries",
        .data$num_countries == 3 ~ "3 countries",
        .data$num_countries == 4 ~ "4 countries",
        .data$num_countries == 5 ~ "5 countries",
        .data$num_countries >= 6 ~ "6+ countries",
        TRUE ~ "Other"
      )
    )

  # Count tools in each category
  scope_counts <- scope_summary |>
    dplyr::group_by(.data$scope_category) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  # Define order for categories
  category_order <- c("Global", "Europe", "6+ countries", "5 countries",
                      "4 countries", "3 countries", "2 countries",
                      "1 country", "Unspecified")

  # Filter to only categories present in data and order
  scope_counts$scope_category <- factor(
    scope_counts$scope_category,
    levels = category_order[category_order %in% scope_counts$scope_category]
  )

  scope_counts <- scope_counts[order(scope_counts$scope_category), ]

  # Create bar chart
  p <- ggplot2::ggplot(scope_counts, ggplot2::aes(x = .data$scope_category, y = .data$count)) +
    ggplot2::geom_bar(stat = "identity", fill = fill_color) +
    ggplot2::geom_text(ggplot2::aes(label = .data$count), vjust = -0.5, size = 3.5) +
    ggplot2::labs(
      title = title,
      x = "Geographic Scope",
      y = "Number of Tools",
      caption = paste0("Number of tools: ", n_tools)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )

  # Print summary statistics
  cat("\nSummary Statistics:\n")
  cat("Tools with country-specific scope (1 country):",
      sum(scope_counts$count[scope_counts$scope_category == "1 country"], na.rm = TRUE), "\n")
  cat("Tools with Global scope:",
      sum(scope_counts$count[scope_counts$scope_category == "Global"], na.rm = TRUE), "\n")
  cat("Tools with Europe scope:",
      sum(scope_counts$count[scope_counts$scope_category == "Europe"], na.rm = TRUE), "\n")

  return(p)
}

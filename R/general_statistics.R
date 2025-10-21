#' Count active tools over time
#'
#' @param database A dataframe containing tool information
#' @param ownership Logical. If TRUE, displays ownership type as a stacked bar chart. Default is FALSE.
#' @param precision Logical. If FALSE, doesn't distinguish between private industry types. Default is TRUE.
#' @param thisyear Numeric. The current year for calculating active tools. Default is current year.
#' @return A ggplot2 object
#' @export
active_tools <- function(database, ownership = FALSE, precision = TRUE, thisyear = as.numeric(format(Sys.Date(), "%Y"))){
  # Replace empty strings with NA
  database$Latest.update[database$Latest.update == ""] <- NA
  database$Latest.update <- as.numeric(database$Latest.update)
  database$Latest.update <- suppressWarnings(as.numeric(database$Latest.update))
  cat("Number of NAs in Latest.update:", sum(is.na(database$Latest.update)), "\n")
  cat("Number of NAs in Year.of.release:", sum(is.na(database$Year.of.release)), "\n")

  # Define year range
  year_range <- seq(min(database$Year.of.release, na.rm = TRUE), thisyear)

  # For each tool, define the "end year" of activity
  database <- database |>
    dplyr::mutate(
      end_year = dplyr::case_when(
        Still.active. == "Yes" ~ max(year_range, na.rm = TRUE),
        Still.active. == "No" & !is.na(Latest.update) ~ Latest.update,
        Still.active. == "No" & is.na(Latest.update) ~ Year.of.release,
        TRUE ~ Year.of.release
      )
    )

  if (ownership == TRUE){
    # Expand dataset: one row per year the tool is active
    active_timeline <- lapply(year_range, function(y) {
      database |>
        dplyr::filter(Year.of.release <= y & end_year >= y) |>
        dplyr::mutate(Year = y)
    }) |> dplyr::bind_rows()

    # Count active tools per year *and* by ownership
    active_counts <- active_timeline |>
      dplyr::group_by(Year, Initiative...Partnership.type) |>
      dplyr::summarise(Active_Tools = dplyr::n(), .groups = "drop")

    if (precision == FALSE){
      active_counts <- active_counts |>
        dplyr::mutate(
          Initiative...Partnership.type = dplyr::case_when(
            Initiative...Partnership.type %in% c('Private-led (core industry)', 'Private-led (service industry)') ~ "Private-led",
            TRUE ~ Initiative...Partnership.type
          )
        )
    }

    # Plot stacked histogram
    ggplot2::ggplot(active_counts, ggplot2::aes(x = Year, y = Active_Tools, fill = Initiative...Partnership.type)) +
      ggplot2::geom_col() +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Active Tools Over Time by Ownership",
        x = "Year",
        y = "Active Tools",
        fill = "Ownership"
      ) +
      ggplot2::scale_fill_brewer(palette = "Set2")
  }

  else{
    # Count active tools per year efficiently
    active_df <- data.frame(
      Year = year_range,
      Active_Tools = sapply(year_range, function(y) {
        sum(database$Year.of.release <= y & database$end_year >= y, na.rm = TRUE)
      })
    )

    # Plot
    ggplot2::ggplot(active_df, ggplot2::aes(x = Year, y = Active_Tools)) +
      ggplot2::geom_col(fill = "steelblue") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Number of Active Tools Over Time",
        x = "Year",
        y = "Active Tools"
      )
  }
}


#' Calculate year-on-year growth in active tools
#'
#' @param database A dataframe containing tool information
#' @param thisyear Numeric. The current year for calculating active tools. Default is current year.
#' @return A dataframe with year-on-year growth metrics
#' @export
calculate_yoy_growth <- function(database, thisyear = as.numeric(format(Sys.Date(), "%Y"))) {
  # Replace empty strings with NA
  database$Latest.update[database$Latest.update == ""] <- NA
  database$Latest.update <- as.numeric(database$Latest.update)
  database$Latest.update <- suppressWarnings(as.numeric(database$Latest.update))

  # Define year range
  year_range <- seq(min(database$Year.of.release, na.rm = TRUE), thisyear)

  # For each tool, define the "end year" of activity
  database <- database |>
    dplyr::mutate(
      end_year = dplyr::case_when(
        Still.active. == "Yes" ~ max(year_range, na.rm = TRUE),
        Still.active. == "No" & !is.na(Latest.update) ~ Latest.update,
        Still.active. == "No" & is.na(Latest.update) ~ Year.of.release,
        TRUE ~ Year.of.release
      )
    )

  # Count active tools per year efficiently
  active_df <- data.frame(
    Year = year_range,
    Active_Tools = sapply(year_range, function(y) {
      sum(database$Year.of.release <= y & database$end_year >= y, na.rm = TRUE)
    })
  )

  # Calculate year-on-year metrics
  yoy_growth_df <- active_df |>
    dplyr::mutate(
      Active_Tools_Previous_Year = dplyr::lag(Active_Tools, 1),
      YoY_Increase = Active_Tools - Active_Tools_Previous_Year,
      YoY_Percent_Change = round((YoY_Increase / Active_Tools_Previous_Year) * 100, 2)
    )

  return(yoy_growth_df)
}


#' Plot year-on-year growth in active tools
#'
#' @param yoy_df A dataframe from calculate_yoy_growth function
#' @param metric Character. Type of metric to plot: "absolute", "percentage", or "both". Default is "absolute".
#' @return A ggplot2 object
#' @export
plot_yoy_growth <- function(yoy_df, metric = "absolute") {
  # Remove first year (NA values)
  plot_data <- yoy_df |> dplyr::filter(!is.na(YoY_Increase))

  if (metric == "absolute") {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Year, y = YoY_Increase)) +
      ggplot2::geom_col(fill = "steelblue") +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Year-on-Year Increase in Active Tools",
        x = "Year",
        y = "Change in Number of Active Tools"
      )
  } else if (metric == "percentage") {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Year, y = YoY_Percent_Change)) +
      ggplot2::geom_col(fill = "darkgreen") +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Year-on-Year Percentage Change in Active Tools",
        x = "Year",
        y = "Percentage Change (%)"
      )
  } else {
    # Both metrics side by side
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Year)) +
      ggplot2::geom_line(ggplot2::aes(y = YoY_Increase, color = "Absolute Change"), linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(y = YoY_Increase, color = "Absolute Change"), size = 2) +
      ggplot2::geom_line(ggplot2::aes(y = YoY_Percent_Change, color = "Percent Change"), linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(y = YoY_Percent_Change, color = "Percent Change"), size = 2) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Year-on-Year Growth in Active Tools",
        x = "Year",
        y = "Change",
        color = "Metric"
      ) +
      ggplot2::scale_color_manual(values = c("Absolute Change" = "steelblue",
                                             "Percent Change" = "darkgreen"))
  }

  return(p)
}


#' Calculate average year-on-year growth for recent years
#'
#' @param database A dataframe containing tool information
#' @param years Numeric. Number of recent years to calculate average for. Default is 5.
#' @param thisyear Numeric. The current year for calculating active tools. Default is current year.
#' @return A list containing summary dataframe and detailed data
#' @export
calculate_avg_yoy_recent <- function(database, years = 5, thisyear = as.numeric(format(Sys.Date(), "%Y"))) {
  # Get the full year-on-year growth data
  yoy_data <- calculate_yoy_growth(database, thisyear)

  # Get the most recent year in the data
  max_year <- max(yoy_data$Year, na.rm = TRUE)

  # Filter to the specified number of recent years
  recent_years <- yoy_data |>
    dplyr::filter(Year > (max_year - years) & Year <= max_year) |>
    dplyr::filter(!is.na(YoY_Increase))

  # Calculate averages
  avg_absolute <- mean(recent_years$YoY_Increase, na.rm = TRUE)
  avg_percentage <- mean(recent_years$YoY_Percent_Change, na.rm = TRUE)

  # Create summary dataframe
  summary_df <- data.frame(
    Period = paste0("Last ", years, " years (",
                    min(recent_years$Year), "-", max(recent_years$Year), ")"),
    Avg_YoY_Absolute_Increase = round(avg_absolute, 2),
    Avg_YoY_Percent_Change = round(avg_percentage, 2),
    Total_Years = nrow(recent_years),
    Total_Net_Increase = sum(recent_years$YoY_Increase, na.rm = TRUE)
  )

  # Print summary
  cat("\n=== Year-on-Year Growth Summary ===\n")
  cat("Period:", summary_df$Period, "\n")
  cat("Average absolute increase per year:", summary_df$Avg_YoY_Absolute_Increase, "tools\n")
  cat("Average percentage change per year:", summary_df$Avg_YoY_Percent_Change, "%\n")
  cat("Total net increase over period:", summary_df$Total_Net_Increase, "tools\n")
  cat("Years included in calculation:", summary_df$Total_Years, "\n\n")

  # Return both summary and detailed data
  return(list(
    summary = summary_df,
    detailed_data = recent_years
  ))
}


#' Create histogram of tool release years
#'
#' @param database A dataframe containing tool information with Year.of.release column
#' @return A ggplot2 object
#' @export
release_histogram <- function(database){
  ggplot2::ggplot(database, ggplot2::aes(x = Year.of.release)) +
    ggplot2::geom_bar(fill = "steelblue") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Number of Tools by Release Year",
      x = "Year of Release",
      y = "Count"
    )
}


#' Create histogram of latest update years
#'
#' @param database A dataframe containing tool information with Latest.update column
#' @return A ggplot2 object
#' @export
latest_update_histogram <- function(database){
  ggplot2::ggplot(database |> dplyr::filter(!is.na(Latest.update), Latest.update != ""),
                  ggplot2::aes(x = Latest.update)) +
    ggplot2::geom_bar(fill = "forestgreen") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Last Updated Status of Tools",
      x = "Last Updated",
      y = "Count"
    )
}


#' Create histogram of active vs inactive tools
#'
#' @param database A dataframe containing tool information with Still.active. column
#' @return A ggplot2 object
#' @export
active_histogram <- function(database){
  ggplot2::ggplot(database |> dplyr::filter(Still.active. != ""),
                  ggplot2::aes(x = Still.active., fill = Still.active.)) +
    ggplot2::geom_bar() +
    ggplot2::scale_fill_manual(values = c("Yes" = "green", "No" = "red")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Active Tools",
      x = "",
      y = "Count"
    ) +
    ggplot2::theme(legend.position = "none")
}

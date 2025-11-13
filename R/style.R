#' Load Official OECD Fonts for Charts
#'
#' Loads a modified version of Arial Narrow from P: drive that works with R
#' (and also Python).
#'
#' @param verbose Logical. Whether to print informative messages. Defaults to
#'   TRUE in interactive sessions, FALSE otherwise.
#' @importFrom systemfonts system_fonts register_font
#'
#' @export
load_oecd_fonts <- function(verbose = interactive()) {
  # Some systems expose the font under 'name' or 'family'.
  sf <- systemfonts::system_fonts()
  # Check for various possible names the OECD font might have
  oecd_names <- c("Arial Narrow OECD", "ArialNarrow-OECD", "Arial Narrow", "ArialNarrow",
                  "Arial OECD", "ArialNarrowOECD")
  exists_name <- "name" %in% names(sf) && any(oecd_names %in% sf$name)
  exists_family <- "family" %in% names(sf) && any(oecd_names %in% sf$family)

  if (!exists_name && !exists_family) {
    # Helper to attempt registration and ignore the specific "already exists" error

    try_register <- function(path) {
      p <- path.expand(path)
      if (!file.exists(p)) {
        if (verbose) message("Font file not found: ", p)
        return(invisible(FALSE))
      }
      if (grepl("\\.zip$", p, ignore.case = TRUE)) {
        if (verbose) message("Found a .zip file; please extract the .ttf to ~/Downloads and try again: ", p)
        return(invisible(FALSE))
      }
      # Basic sanity check on file size to avoid trying to register an empty/corrupt file
      sz <- tryCatch(file.info(p)$size, error = function(e) NA)
      if (is.na(sz) || sz < 1000) {
        if (verbose) message("Font file looks too small or can't be read: ", p)
        return(invisible(FALSE))
      }

      # First, let's check what font name is actually in the file
      font_info <- tryCatch(
        systemfonts::font_info(p),
        error = function(e) NULL
      )

      if (is.null(font_info)) {
        if (verbose) message("Cannot read font information from: ", p)
        return(invisible(FALSE))
      }

      # Use the actual font family name from the file
      detected_family <- font_info$family[1]
      if (verbose) message("systemfonts detected family: '", detected_family, "'")

      # Try to register with "Arial Narrow OECD" first (the expected name)
      # since FontForge shows this as the correct name
      target_names <- c("Arial Narrow OECD", detected_family)
      target_names <- unique(target_names[target_names != ""])

      success <- FALSE
      registered_name <- NULL

      for (target_name in target_names) {
        if (verbose) message("Attempting to register as: '", target_name, "'")

        res <- tryCatch(
          {
            systemfonts::register_font(target_name, plain = p)
            TRUE
          },
          error = function(e) {
            msg <- conditionMessage(e)
            if (grepl("already exists", msg, ignore.case = TRUE)) {
              if (verbose) message("Font '", target_name, "' appears already registered on this system (ignored).")
              return(TRUE)
            }
            if (verbose) message("Failed to register font '", target_name, "': ", msg)
            return(FALSE)
          }
        )

        if (isTRUE(res)) {
          # Verify registration actually added the font
          sf2 <- systemfonts::system_fonts()
          if (target_name %in% sf2$name || target_name %in% sf2$family) {
            if (verbose) message("Successfully registered font: '", target_name, "'")
            success <- TRUE
            registered_name <- target_name
            break
          } else {
            if (verbose) message("Registration of '", target_name, "' succeeded but font not found in system list")
          }
        }
      }

      if (!success) {
        if (verbose) message("Could not successfully register font from: ", p)
        return(invisible(FALSE))
      }

      invisible(res)
    }

    p1 <- "//OECDMAIN/em_apps/R/Fonts/ArialNarrowOECD.ttf"
    p2 <- "~/Downloads/ArialNarrowOECD.ttf"
    ok <- FALSE
    if (isTRUE(file.exists(p1))) {
      ok <- try_register(p1)
    } else if (file.exists(path.expand(p2))) {
      ok <- try_register(p2)
    } else {
      if (verbose) {
        message("Your account can't access OECDMAIN. You can download Arial Narrow OECD from:")
        message("  https://algobank.oecd.org:4430/r-oecd-graphs/oecdplot/-/raw/main/fonts/ArialNarrowOECD.zip")
        message("Extract the zip and place ArialNarrowOECD.ttf in ~/Downloads/")
        message("Use debug_font_file('~/Downloads/ArialNarrowOECD.ttf') to verify the font file is correct.")
      }
    }

    # If OECD-specific font wasn't registered, attempt a safe fallback to plain Arial Narrow
    if (!isTRUE(ok)) {
      if (verbose) message("Attempting fallback: plain 'Arial Narrow' (only if an OECD font wasn't available).")
      sf3 <- systemfonts::system_fonts()
      if (!("Arial Narrow" %in% sf3$name || "Arial Narrow" %in% sf3$family)) {
        try_register_plain <- function(path) {
          p <- path.expand(path)
          if (!file.exists(p)) return(invisible(FALSE))
          # reuse try_register logic but register under plain name
          res <- tryCatch(
            {
              systemfonts::register_font("Arial Narrow", plain = p)
              TRUE
            },
            error = function(e) {
              msg <- conditionMessage(e)
              if (grepl("already exists", msg, ignore.case = TRUE)) {
                if (verbose) message("Arial Narrow appears already registered on this system (ignored).")
                return(TRUE)
              }
              if (verbose) message("Failed to register fallback font 'Arial Narrow' from file: ", p, "\n", msg)
              return(FALSE)
            }
          )
          invisible(res)
        }

        p3 <- "//OECDMAIN/em_apps/R/Fonts/ArialNarrow.ttf"
        p4 <- "~/Downloads/ArialNarrow.ttf"
        if (isTRUE(file.exists(p3))) {
          try_register_plain(p3)
        } else if (file.exists(path.expand(p4))) {
          try_register_plain(p4)
        }
      }
    }
  } else {
    if (verbose) message("Arial Narrow OECD font is already registered.")
  }
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL



#' 'ggplot2' Theme Compatible with PAC Rules
#'
#' Complete theme which controls all non-data display. You can use \code{theme()}
#' in a chained expression to tweak the display for this current theme.
#'
#' @param base_size Base font size, given in pts.
#' @param base_family Base font family
#' @param base_line_size Base size forline elements
#' @param base_rect_size Base size for rect elements
#' @param base_background Base background color (white or gray/grey)
#' @param base_x_axis_angle Base angle of the x-axis text (default 45)
#' @param base_col_width Base angle of the column width (default 0.7)
#' @importFrom ggplot2 theme theme_bw element_rect element_line element_text
#'  element_blank margin unit ggsave '%+replace%' scale_y_continuous
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' load_oecd_fonts()
#'
#' p <- ggplot(pta) +
#'   geom_col(
#'     aes(x = country, y = pct_pta, fill = category),
#'     position = "dodge2"
#'   )
#'
#' # both fill and fill direction are optional
#' p + theme_oecd()
#' }
#' @export
theme_oecd <- function(base_size = 7.5,
                       base_family = "Arial Narrow OECD",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22,
                       base_background = "gray",
                       base_x_axis_angle = 45,
                       base_col_width = 0.7) {
  base_background <- switch(
    EXPR = base_background,
    white = "white",
    gray = "gray",
    grey = "gray",
    {
      warning(paste0("Option '", !!sym("option"), "' does not exist. Defaulting to 'white'."))
      "white"
    }
  )

  g <- theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.line = element_line(colour = "black", linewidth = 0.5, linetype = "solid"),
      axis.ticks = element_line(colour = "black", linewidth = 0.3),
      axis.ticks.length = unit(-2.0, units = "pt"),
      axis.text.x = element_text(
        size = base_size,
        color = "black",
        angle = base_x_axis_angle,
        vjust = 1,
        hjust = ifelse(base_x_axis_angle == 0, 0.5, 1),
        margin = margin(7, 0, 0, 0, unit = "pt")
      ),
      axis.text.y = element_text(
        size = base_size, color = "black", hjust = 1,
        margin = margin(0, 7, 5.5, 5.5, unit = "pt")
      ),
      axis.title.x = element_text(
        size = base_size,
        margin = margin(0, 0, 0, 2, unit = "pt"), hjust = 0.5
      ),
      axis.title.y = element_text(
        size = base_size, angle = 90,
        margin = margin(0, 0, 0, 2, unit = "pt"), hjust = 0.5
      ),
      legend.text = element_text(size = base_size),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.background = element_rect(fill = c("#f2f0f2"), colour = NA),
      legend.margin = margin(t = 1, r = 1, b = 1, l = 1),
      # legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "#f2f0f2", colour = NA),
      legend.key.height = unit(0.6, "lines"),
      panel.border = element_blank(),
      panel.spacing = unit(1, "lines"),
      plot.title = element_text(size = base_size),
      plot.subtitle = element_text(size = base_size, hjust = 0),
      plot.caption = element_blank(),
      strip.text.x = element_text(size = base_size),
      strip.text = element_text(
        size = base_size, face = "bold",
        margin = margin(b = 3.0)
      ),
      strip.background = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      complete = T
    )

  if (base_background == "white") {
    g <- g %+replace%
      theme(panel.grid = element_line(colour = "grey92"))
  }

  if (base_background == "gray") {
    g <- g %+replace%
      theme(
        panel.background = element_rect(fill = "#eaeaea", colour = NA),
        panel.grid = element_line(colour = "white")
      )
  }

  return(g)
}

rgb2hex <- function(r, g, b) {
  grDevices::rgb(r, g, b, maxColorValue = 255)
}


#' @export
#' @inheritParams scale_fill_oecd_d
#' @rdname scale_fill_oecd_d
scale_colour_oecd_d <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                                option = "darkblue", aesthetics = "colour") {
  discrete_scale(aesthetics, "oecd_standard", oecd_pal(alpha, begin, end, direction, option), ...)
}

#' @export
#' @inheritParams scale_fill_oecd_d
#' @importFrom scales gradient_n_pal
#' @importFrom ggplot2 continuous_scale
#' @rdname scale_fill_oecd_d
scale_colour_oecd_c <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                                option = "darkblue", values = NULL,
                                na.value = "grey50", guide = "colourbar",
                                aesthetics = "colour") {
  continuous_scale(aesthetics, "oecd_standard",
                   gradient_n_pal(
                     oecd_pal(alpha, begin, end, direction, option)(10),
                     values
                   ),
                   na.value = na.value,
                   guide = guide, ...
  )
}







#' @title OECD Colour Palettes
#'
#' @description This function creates a vector of colours along the selected
#'  colour map.
#'
#' @param n The number of colors (\eqn{\ge 1}) to be in the palette.
#'
#' @param alpha	The alpha transparency, a number in [0,1], see argument alpha in
#' \code{\link[grDevices]{hsv}}.
#'
#' @param begin The (corrected) hue in [0,1] at which the color map begins.
#'
#' @param end The (corrected) hue in [0,1] at which the color map ends.
#'
#' @param direction Sets the order of colors in the scale. If 1, the default,
#'  colors are ordered from darkest to lightest. If -1, the order of colors is
#'  reversed.
#'
#' @param option A character string indicating the color map option to use.
#'  Options are available:
#'  \itemize{
#'   \item "darkblue"
#'   \item "orange"
#'   \item "darkgreen"
#'   \item "blue"
#'   \item "green"
#'   \item "pink"
#'   \item "purple"
#'   \item "red"
#'   \item "ppt"
#'  }
#'
#' @importFrom dplyr rowwise mutate distinct pull across everything arrange as_tibble
#' @importFrom tidyr crossing
#' @importFrom grDevices colorRampPalette col2rgb rgb
#' @importFrom stats kmeans
#' @importFrom rlang sym
#'
#' @return \code{oecd_clrs} returns a character vector, \code{cv}, of color hex
#'  codes. This can be used either to create a user-defined color palette for
#'  subsequent graphics.
#'
#'
#' @details
#'
#' \if{html}{Here are the color scales:
#'
#'   \out{<div style="text-align: center">}\figure{oecd-scales.png}{options: style="width:750px;max-width:75\%;"}\out{</div>}
#'   }
#' \if{latex}{Here are the color scales:
#'
#'   \out{\begin{center}}\figure{oecd-scales.png}\out{\end{center}}
#'   }
#'
#' Semi-transparent colors (\eqn{0 < alpha < 1}) are supported only on some
#'  devices: see \code{\link[grDevices]{rgb}}.
#'
#' @examples
#' \dontrun{
#' # using code from RColorBrewer to demo the palette
#' n <- 20
#' image(
#'   1:n, 1, as.matrix(1:n),
#'   col = oecd_clrs(n, option = "darkblue"),
#'   xlab = "OECD darkblue n", ylab = "", xaxt = "n", yaxt = "n", bty = "n"
#' )
#' }
#' @export
oecd_clrs <- function(n = 8, alpha = 1, begin = 0, end = 1, direction = 1, option = "darkblue") {
  if (begin < 0 | begin > 1 | end < 0 | end > 1) {
    stop("begin and end must be in [0,1]")
  }

  if (abs(direction) != 1) {
    stop("direction must be 1 or -1")
  }

  if (n == 0) {
    return(character(0))
  }

  option <- switch(
    EXPR = option,
    darkblue = "darkblue",
    orange = "orange",
    darkgreen = "darkgreen",
    blue = "blue",
    green = "green",
    pink = "pink",
    purple = "purple",
    red = "red",
    ppt = "ppt",
    {
      warning(paste0("Option '", option, "' does not exist. Defaulting to 'darkblue'."))
      "darkblue"
    }
  )

  use_clrs <- switch(
    EXPR = option,
    darkblue = c(FarmLevelTools.OECD::oecd_colours$hex[FarmLevelTools.OECD::oecd_colours$palette == "Dark Blue"], "#ffffff"),
    orange = c(FarmLevelTools.OECD::oecd_colours$hex[FarmLevelTools.OECD::oecd_colours$palette == "Orange"], "#ffffff"),
    darkgreen = c(FarmLevelTools.OECD::oecd_colours$hex[FarmLevelTools.OECD::oecd_colours$palette == "Dark Green"], "#ffffff"),
    blue = c(FarmLevelTools.OECD::oecd_colours$hex[FarmLevelTools.OECD::oecd_colours$palette == "Blue"], "#ffffff"),
    green = c(FarmLevelTools.OECD::oecd_colours$hex[FarmLevelTools.OECD::oecd_colours$palette == "Green"], "#ffffff"),
    pink = c(FarmLevelTools.OECD::oecd_colours$hex[FarmLevelTools.OECD::oecd_colours$palette == "Pink"], "#ffffff"),
    purple = c(FarmLevelTools.OECD::oecd_colours$hex[FarmLevelTools.OECD::oecd_colours$palette == "Purple"], "#ffffff"),
    red = c(FarmLevelTools.OECD::oecd_colours$hex[FarmLevelTools.OECD::oecd_colours$palette == "Red"], "#ffffff"),
    ppt = c(FarmLevelTools.OECD::oecd_colours$hex[FarmLevelTools.OECD::oecd_colours$palette == "OECD Corporate Colors"], "#ffffff")
  )

  if (n <= 8) {
    use_clrs <- use_clrs[1:min(n, 8)]

    if (alpha != 1) {
      use_clrs <- t(col2rgb(use_clrs)) / 255
      use_clrs <- rgb(use_clrs[, 1], use_clrs[, 2], use_clrs[, 3], alpha = alpha)[1:n]
    }

    if (direction != 1) {
      use_clrs <- rev(use_clrs)
    }

    return(use_clrs)
  }

  set.seed(1234)

  use_clrs_s <- crossing(col1 = use_clrs, col2 = use_clrs) %>%
    rowwise() %>%
    # interpolate 9 colors, take the midpoint
    mutate(colf = colorRampPalette(c(!!sym("col1"), !!sym("col2")))(9)[5]) %>%
    distinct(!!sym("colf")) %>%
    pull()

  dfcolors <- as_tibble(t(col2rgb(use_clrs_s)))

  use_clrs_n <- kmeans(dfcolors, n)$centers %>%
    as_tibble() %>%
    mutate(across(everything(), as.integer)) %>%
    mutate(color = rgb2hex(!!sym("red"), !!sym("green"), !!sym("blue"))) %>%
    arrange(!!sym("red"), !!sym("green"), !!sym("blue")) %>%
    pull(!!sym("color"))

  if (alpha != 1) {
    use_clrs_n <- t(col2rgb(use_clrs_n)) / 255
    use_clrs_n <- rgb(use_clrs_n[, 1], use_clrs_n[, 2], use_clrs_n[, 3], alpha = alpha)
  }

  if (direction != 1) {
    use_clrs_n <- rev(use_clrs_n)
  }

  return(use_clrs_n)
}

#' @title OECD Colour Palettes
#'
#' @description A wrapper function around \code{oecd_clrs} to
#'  turn it into a palette function compatible with
#'  \code{\link[ggplot2]{discrete_scale}}.
#' @details See \code{oecd_clrs} for more information on the color palettes.
#' @param alpha The alpha transparency, a number in [0,1], see argument alpha in
#' \code{\link[grDevices]{hsv}}.
#' @param begin The (corrected) hue in [0,1] at which the color map begins.
#' @param end The (corrected) hue in [0,1] at which the color map ends.
#' @param direction Sets the order of colors in the scale. If 1, the default,
#'  colors are ordered from darkest to lightest. If -1, the order of colors is
#'  reversed.
#' @param option A character string indicating the color map option to use.
#'  Options are available:
#'  \itemize{
#'   \item "darkblue" (Standard)
#'   \item "orange"
#'   \item "darkgreen"
#'   \item "blue"
#'   \item "green"
#'   \item "pink"
#'   \item "purple"
#'   \item "red"
#'   \item "ppt"
#'  }
#'
#' @examples
#' library(scales)
#' show_col(oecd_pal()(8), ncol = 8)
#'
#' @export
oecd_pal <- function(alpha = 1, begin = 0, end = 1, direction = 1, option = "darkblue") {
  function(n) {
    oecd_clrs(n, alpha, begin, end, direction, option)
  }
}

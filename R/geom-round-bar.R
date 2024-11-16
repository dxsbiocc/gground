#' @title round bar
#' @export
#' @rdname geom_round_bar
#'
#' @details
#' There are two types of bar charts: `geom_round_bar()` and `geom_round_col()`.
#' `geom_round_bar()` makes the height of the bar proportional to the number of
#' cases in each group (or if the weight aesthetic is supplied, the sum of the
#' weights). If you want the heights of the bars to represent values in the data,
#' use `geom_round_col()` instead. `geom_round_bar()` uses `stat_count()` by default:
#' it counts the number of cases at each x position. `geom_round_col()` uses
#' `stat_identity()`: it leaves the data as is.
#'
#' @param mapping Set of aesthetic mappings created by `aes()` or `aes_()`.
#' @param data A layer specific dataset - only needed if you want to override the
#' @param stat The statistical transformation to use on the data for this layer,
#' @param position Position adjustment, either as a string, or the result of a
#' @param ... Other arguments passed on to `layer()`.
#' @param radius The radius of the rounded corners.
#' @param just A numeric vector of length 1 or 2, giving the x (and optionally y)
#' @param na.rm If FALSE, the default, missing values are removed with a warning.
#' @param orientation The orientation of the layer. The default (NA) automatically
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics.
#'
#' @examples
#' # geom_bar is designed to make it easy to create bar charts that show
#' # counts (or sums of weights)
#' g <- ggplot(mpg, aes(class))
#' # Number of cars in each class:
#' g + geom_bar()
#' # Total engine displacement of each class
#' g + geom_bar(aes(weight = displ))
#' # Map class to y instead to flip the orientation
#' ggplot(mpg) + geom_bar(aes(y = class))
geom_round_bar <- function(
        mapping = NULL, data = NULL,
        stat = "count", position = "stack",
        ...,
        radius = grid::unit(2, "pt"),
        just = 0.5,
        na.rm = FALSE,
        orientation = NA,
        show.legend = NA,
        inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRoundBar,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            radius = radius,
            just = just,
            na.rm = na.rm,
            orientation = orientation,
            ...
        )
    )
}

#' @format NULL
#' @usage NULL
#' @export
#' @include geom-round-rect.R
GeomRoundBar <- ggplot2::ggproto(
    "GeomRoundBar", GeomRoundRect,
    required_aes = c("x", "y"),
    # These aes columns are created by setup_data(). They need to be listed here so
    # that GeomRect$handle_na() properly removes any bars that fall outside the defined
    # limits, not just those for which x and y are outside the limits
    non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

    default_aes = ggplot2::aes(!!!GeomRoundRect$default_aes, width = NULL),

    setup_params = function(data, params) {
       params$flipped_aes <- ggplot2::has_flipped_aes(data, params)
       params
    },

    extra_params = c("just", "na.rm", "orientation"),

    setup_data = function(data, params) {
       data$flipped_aes <- params$flipped_aes
       data <- flip_data(data, params$flipped_aes)
       data$width <- data$width %||%
           params$width %||% (min(vapply(
               split(data$x, data$PANEL, drop = TRUE),
               resolution, numeric(1), zero = FALSE
           )) * 0.9)
       data$just <- params$just %||% 0.5
       data <- transform(data,
                         ymin = pmin(y, 0), ymax = pmax(y, 0),
                         xmin = x - width * just, xmax = x + width * (1 - just),
                         width = NULL, just = NULL
       )
       ggplot2::flip_data(data, params$flipped_aes)
    },
    draw_panel = function(self, data, panel_params, coord, radius = grid::unit(2, "pt")) {
        ggplot2::ggproto_parent(GeomRoundRect, self)$draw_panel(data, panel_params, coord, radius = radius)
    },
    rename_size = TRUE
)

#' Rectangles
#'
#' `geom_round_rect()` and `geom_round_tile()` do the same thing, but are
#' parameterised differently: `geom_round_rect()` uses the locations of the four
#' corners (`xmin`, `xmax`, `ymin` and `ymax`), while
#' `geom_round_tile()` uses the center of the tile and its size (`x`,
#' `y`, `width`, `height`). `geom_raster()` is a high
#' performance special case for when all the tiles are the same size, and no
#' pattern fills are applied.
#'
#' @export
#'
#' @details
#' `geom_round_rect()` and `geom_round_tile()`'s respond differently to scale
#' transformations due to their parameterisation. In `geom_round_rect()`, the scale
#' transformation is applied to the corners of the rectangles. In `geom_round_tile()`,
#' the transformation is applied only to the centres and its size is determined
#' after transformation.
#'
#' @param mapping Set of aesthetic mappings created by `aes()` or `aes_()`.
#' If specified and `inherit.aes = TRUE` (the default), it is combined with the default
#' mapping at the top level of the plot. You must supply `mapping` if there is no plot
#' mapping.
#' A `NULL` value will remove the mapping.
#' @param data The data to be displayed in this layer. There are three options:
#' If `NULL`, the default, the data is inherited from the plot data as specified in the
#' call to `ggplot()`. A data.frame, or other object, will override the plot data.
#' All objects will be fortified to produce a data frame. See `fortify()` for which
#' variables will be created.
#' A function will be called with a single argument, the plot data. The return value
#' must be a data.frame, and will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this layer,
#' as a string.
#' @param position Position adjustment, either as a string, or the result of a
#' call to a position adjustment function.
#' @param ... Other arguments passed on to `layer()`.
#' @param linejoin Line join style (round, mitre, bevel).
#' @param radius The radius of the rounded corners.
#' @param na.rm If `FALSE`, the default, missing values are removed with a warning.
#' If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#' `NA`, the default, includes if any aesthetics are mapped. `FALSE` never includes,
#' and `TRUE` always includes.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#'
#' @examples
#' # The most common use for rectangles is to draw a surface. You always want
#' # to use geom_raster here because it's so much faster, and produces
#' # smaller output when saving to PDF
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'  geom_raster(aes(fill = density))
#'
#' # Interpolation smooths the surface & is most helpful when rendering images.
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'  geom_raster(aes(fill = density), interpolate = TRUE)
#'
#' # If you want to draw arbitrary rectangles, use geom_tile() or geom_rect()
#' df <- data.frame(
#'   x = rep(c(2, 5, 7, 9, 12), 2),
#'   y = rep(c(1, 2), each = 5),
#'   z = factor(rep(1:5, each = 2)),
#'   w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
#' )
#' ggplot(df, aes(x, y)) +
#'   geom_round_tile(aes(fill = z), colour = "grey50")
#' ggplot(df, aes(x, y, width = w)) +
#'   geom_round_tile(aes(fill = z), colour = "grey50")
#' ggplot(df, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y, ymax = y + 1)) +
#'   geom_round_rect(aes(fill = z), colour = "grey50")
#'
#' \donttest{
#' # Justification controls where the cells are anchored
#' df <- expand.grid(x = 0:5, y = 0:5)
#' set.seed(1)
#' df$z <- runif(nrow(df))
#' # default is compatible with geom_tile()
#' ggplot(df, aes(x, y, fill = z)) +
#'   geom_raster()
#' # zero padding
#' ggplot(df, aes(x, y, fill = z)) +
#'   geom_raster(hjust = 0, vjust = 0)
#'
#' # Inspired by the image-density plots of Ken Knoblauch
#' cars <- ggplot(mtcars, aes(mpg, factor(cyl)))
#' cars + geom_point()
#' cars + stat_bin_2d(aes(fill = after_stat(count)), binwidth = c(3,1))
#' cars + stat_bin_2d(aes(fill = after_stat(density)), binwidth = c(3,1))
#'
#' cars +
#'   stat_density(
#'     aes(fill = after_stat(density)),
#'     geom = "raster",
#'     position = "identity"
#'    )
#' cars +
#'   stat_density(
#'     aes(fill = after_stat(count)),
#'     geom = "raster",
#'     position = "identity"
#'   )
#' }
geom_round_tile <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      linejoin = "mitre",
                      radius = grid::unit(1, "pt"),
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRoundTile,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            radius = radius,
            linejoin = linejoin,
            na.rm = na.rm,
            ...
        )
    )
}

draw_key_round_tile <- function (data, params, size)
{
    if (is.null(data$linewidth)) {
        data$linewidth <- 0.5
    }
    lwd <- min(data$linewidth, min(size)/4)
    grid::roundrectGrob(
        r = min(params$radius, grid::unit(3, "pt")),
        width = grid::unit(1, "npc") - grid::unit(lwd, "mm"),
        height = grid::unit(1, "npc") - grid::unit(lwd, "mm"),
        gp = grid::gpar(
            col = data$colour %||% NA,
            fill = ggplot2::fill_alpha(data$fill %||% "grey20", data$alpha),
            lty = data$linetype %||% 1, lwd = lwd * ggplot2::.pt,
            linejoin = params$linejoin %||% "mitre",
            lineend = params$lineend %||% "butt")
        )
}

#' @format NULL
#' @usage NULL
#' @export
#' @include geom-round-rect.R
GeomRoundTile <- ggplot2::ggproto("GeomRoundTile", GeomRoundRect,
                    extra_params = c("na.rm"),

                    setup_data = function(data, params) {
                        data$width <- data$width %||% params$width %||% resolution(data$x, FALSE, TRUE)
                        data$height <- data$height %||% params$height %||% resolution(data$y, FALSE, TRUE)

                        transform(data,
                                  xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
                                  ymin = y - height / 2, ymax = y + height / 2, height = NULL
                        )
                    },

                    default_aes = ggplot2::aes(fill = "grey20", colour = NA, linewidth = 0.1, linetype = 1,
                                      alpha = NA, width = NA, height = NA),

                    required_aes = c("x", "y"),

                    # These aes columns are created by setup_data(). They need to be listed here so
                    # that GeomRect$handle_na() properly removes any bars that fall outside the defined
                    # limits, not just those for which x and y are outside the limits
                    non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

                    draw_key = draw_key_round_tile
)

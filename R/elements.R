#' @title: Round rectangle element
#' @description
#' In conjunction with the \link{theme} system, the `element_` functions
#' specify the display of how non-data components of the plot are drawn.
#'
#'   - `element_blank()`: draws nothing, and assigns no space.
#'   - `element_rect()`: borders and backgrounds.
#'   - `element_line()`: lines.
#'   - `element_text()`: text.
#'
#' `rel()` is used to specify sizes relative to the parent,
#' `margin()` is used to specify the margins of elements.
#'
#' @param fill Fill colour.
#' @param colour,color Line/border colour. Color is an alias for colour.
#' @param linewidth Line/border size in mm.
#' @param linetype Line type.
#' @param inherit.blank Should this element inherit the existence of an
#'   `element_blank` among its parents? If `TRUE` the existence of
#'   a blank element among its parents will cause this element to be blank as
#'   well. If `FALSE` any blank parent element will be ignored when
#'   calculating final element state.
#' @param radius The radius of the corners of the rectangle.
#' @return An S3 object of class `element`, `rel`, or `margin`.
#' @examples
#' plot <- ggplot(mpg, aes(displ, hwy)) + geom_point()
#'
#'
#' plot + theme(
#'   panel.background = element_round_rect(fill = "white"),
#'   plot.margin = margin(2, 2, 2, 2, "cm"),
#'   plot.background = element_round_rect(
#'     radius = grid::unit(1, "mm"),
#'     fill = "grey90",
#'     colour = "black",
#'     linewidth = 1,
#'     linetype = 1
#'   )
#' )
#' @name element_round_rect
#' @export
#' @rdname element_round_rect
element_round_rect <- function(fill = NULL, colour = NULL, linewidth = NULL,
                               linetype = NULL, color = NULL, inherit.blank = FALSE,
                               radius = NULL) {

    if (!is.null(color))  colour <- color
    structure(
        list(fill = fill, colour = colour, linewidth = linewidth,
             linetype = linetype, radius = radius,
             inherit.blank = inherit.blank),
        class = c("element_round_rect", "element_rect", "element")
    )
}

#' @importFrom ggplot2 element_grob
#' @method element_grob element_round_rect
#' @export
element_grob.element_round_rect <- function(
        element, x = 0.5, y = 0.5, width = 1, height = 1,
        radius = grid::unit(2, 'pt'),
        fill = NULL, colour = NULL, linewidth = NULL, linetype = NULL,
        ...) {

    # The gp settings can override element_gp
    gp <- grid::gpar(
        lwd = linewidth %||% element$linewidth,
        col = colour %||% element$colour,
        fill = fill %||% element$fill,
        lty = linetype %||% element$linetype)
    # radius
    r <- radius %||% element$radius
    grid::roundrectGrob(x, y, width, height, r = r, gp = gp, ...)
}

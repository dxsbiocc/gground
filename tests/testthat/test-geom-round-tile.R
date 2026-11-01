roundrect_linejoins <- function(grob) {
  if (inherits(grob, "roundrect")) {
    return(grob$gp$linejoin)
  }
  children <- c(grob$grobs, grob$children)
  unlist(lapply(children, roundrect_linejoins), use.names = FALSE)
}

round_tile_linejoins <- function(...) {
  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  data <- data.frame(x = 1, y = 1, fill = "tile")
  plot <- ggplot(data, aes(x, y, fill = fill)) +
    geom_round_tile(
      colour = "black", linewidth = 2, radius = grid::unit(0.1, "npc"), ...
    )

  roundrect_linejoins(ggplotGrob(plot))
}

test_that("tile and legend borders use round joins by default", {
  expect_identical(round_tile_linejoins(), c("round", "round"))
})

test_that("tile legend uses round joins when no line join is supplied", {
  data <- data.frame(
    fill = "grey20", colour = "black", linewidth = 2, linetype = 1, alpha = NA
  )
  key <- GeomRoundTile$draw_key(
    data, list(radius = grid::unit(0.1, "npc")), size = 5
  )
  expect_identical(key$gp$linejoin, "round")
})

test_that("tile and legend borders respect explicit line joins", {
  for (linejoin in c("round", "mitre", "bevel")) {
    expect_identical(
      round_tile_linejoins(linejoin = linejoin),
      rep(linejoin, 2)
    )
  }
})

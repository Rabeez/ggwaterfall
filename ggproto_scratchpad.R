# function to generate data (computed variables from plot parameters)
create_circle <- function(data, n) {
  angles = seq(
    from = 0,
    to = 2 * pi,
    length.out = n + 1
  )
  
  data.frame(
    x = cos(angles) * data$r + data$x0,
    y = sin(angles) * data$r + data$y0,
    data
  )
}

library(ggplot2)

# manually constructing geometries required
ggplot(
  create_circle(
    data = list(
      r = 10,
      x0 = 4,
      y0 = 2
    ),
    n = 200
  )
) + 
  geom_polygon(aes(x = x, y = y)) +
  coord_fixed()


# custom stat/geom
StatCirc <- ggproto(
  "StatCirc", Stat,
  setup_data = function(data, params) {
    if (data$group[1] == -1) {
      nrows <- nrow(data)
      data$group <- seq_len(nrows)
    }
    data
  },
  compute_group = function(data, scales, n = 360) {
    create_circle(data, n)
  },
  required_aes = c("x0", "y0", "r")
)

stat_circle <- function(mapping = NULL, data = NULL, geom = "polygon",
                        position = "identity", na.rm = FALSE, show.legend = NA, 
                        inherit.aes = TRUE, ...) {
  layer(
    stat = StatCirc, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
geom_circle <- function(mapping = NULL, data = NULL, stat = StatCirc,
                        position = "identity", na.rm = FALSE, show.legend = NA, 
                        inherit.aes = TRUE, ...) {
  layer(
    stat = stat, data = data, mapping = mapping, geom = "polygon", 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# using custom stat/geom
ggplot(
  data.frame(
    r = c(5, 4),
    x0 = c(-5, 5),
    y0 = c(5, -5),
    type = c("a", "b")
  )
) + 
  # stat_circle(aes(x0 = x0, y0 = y0, r = r, fill = type)) + 
  geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = type)) + 
  coord_fixed() +
  facet_wrap(~type)

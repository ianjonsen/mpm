##' @title Generate a projected map of tracks shaded by behavioural state
##' @param m a fitted object of class mpm
##' @param proj a character string defining the map projection, default = "lambert"
##' @param params parameters specific to the supplied projection
##' @param xlim longitude range to plot
##' @param ylim latitude range to plot
##' @importFrom ggplot2 fortify ggplot aes coord_map xlab ylab geom_point geom_polygon
##' @importFrom ggplot2 annotate theme theme_dark element_blank element_text aes_string
##' @importFrom mapproj mapproject
##' @importFrom viridis scale_colour_viridis
##' @export
map <- function(m, proj = "mercator", params = NULL, xlim = NULL, ylim = NULL, theme = "dark") {

countriesHigh <- NULL
data(countriesHigh, package = "rworldxtra", envir = environment())
wm <- suppressMessages(fortify(countriesHigh))

if(is.null(xlim)) xlim <- extendrange(m$data$lon, f = 0.04)
if(is.null(ylim)) ylim <- extendrange(m$data$lat, f = 0.04)

p <-
  ggplot() +
  coord_map(projection = proj, parameters = params, xlim = xlim, ylim = ylim) +
  xlab("Longitude") + ylab("Latitude")

p <- p +
  geom_point(aes(x = lon, y = lat, colour = m$fitted$g),
             data = m$data,
             size = 0.5) +
  viridis::scale_colour_viridis(name = expression(italic(gamma[t])), begin = 0, end = 1, direction = -1) +
  switch(theme,
         dark = theme_dark(),
         none = theme_void(),
         light = theme_light(),
         minimal = theme_minimal()
  )

if(proj == "lambert") {
  p <- p +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(legend.title = element_text(hjust = 0.5))
  }

p <- p +
  geom_polygon(
    data = wm,
    aes_string(x = "long", y = "lat", group = "group"),
    fill = grey(0.95)
  )

p
}

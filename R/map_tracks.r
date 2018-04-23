##' @title Generate a projected map of tracks shaded by behavioural state
##' @param m a fitted object of class mpm
##' @param quickmap should a quick map be rendered using ggplot2::coord_quickmap, if so then proj, params and orientation arguments will be ignored
##' @param proj a character string defining the map projection, default = "lambert"
##' @param params parameters specific to the supplied projection
##' @param orientation
##' @param xlim longitude range to plot
##' @param ylim latitude range to plot
##' @param vopt specifies which viridis colour palette is to be used
##' @param dir direction of viridis colour palette, default = 1, use -1 to reverse
##' @param theme ggplot2 theme to be used - choices include, "dark", "minimal", "void", "light" and "classic"
##' @importFrom ggplot2 fortify ggplot aes coord_map xlab ylab geom_point geom_polygon
##' @importFrom ggplot2 annotate theme theme_dark element_blank element_text aes_string
##' @importFrom mapproj mapproject
##' @importFrom viridis scale_colour_viridis
##' @export
map_tracks <-
  function(m,
           quickmap = FALSE,
           proj = "mercator",
           params = NULL,
           orientation = NULL,
           xlim = NULL,
           ylim = NULL,
           vopt = "E",
           dir = 1,
           theme = "dark",
           landcol = grey(0.95)) {


  if(is.null(xlim)) xlim <- extendrange(m$data$lon, f = 0.04)
  if(is.null(ylim)) ylim <- extendrange(m$data$lat, f = 0.04)

  countriesHigh <- NULL
  data(countriesHigh, package = "rworldxtra", envir = environment())
  ## filter out unneeded landmasses to speed plotting
  ##  FIXME: fortify() will be deprecated in ggplot2 - replace w appropriate broom:: fn
  wm <- suppressMessages(fortify(countriesHigh)) %>%
      filter(., lat <= ylim[2]) ## FIXME: this is biased to S hemisphere...

p <-
  ggplot()

if(!quickmap) {
  p <- p + coord_map(
    projection = proj,
    parameters = params,
    orientation = orientation,
    xlim = xlim,
    ylim = ylim
  )
}
else {
  p <- p + coord_quickmap(
    xlim = xlim,
    ylim = ylim,
    expand = FALSE
  )
}

p <- p + xlab("Longitude") + ylab("Latitude")

p <- p +
  geom_point(aes(x = lon, y = lat, colour = m$fitted$g),
             data = m$data,
             size = 0.5) +
  viridis::scale_colour_viridis(
    name = expression(italic(gamma[t])),
    begin = 0,
    end = 1,
    limits = c(0,1),
    direction = dir,
    option = vopt
  ) +
    if(theme == "dark") { theme_dark() }
else if(theme == "minimal") { theme_minimal() }
else if(theme == "void") { theme_void() }
else if(theme == "light") { theme_light() }
else if(theme == "classic") { theme_classic() }
## FIXME: should we go w "void" & use Sumner's graticule() here? at least for conic projections

 if(proj != "mercator" & !quickmap) {
   p <- p +
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank()) +
   theme(axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank()) +
   theme(legend.title = element_text(hjust = 0.5))
   }

p <- p +
  geom_polygon(
    data = wm,
    aes_string(x = "long", y = "lat", group = "group"),
    fill = landcol
  )

p
}
